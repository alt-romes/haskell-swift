{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
-- | Produce a swift library by building a Haskell package using
-- TemplateHaskell and Cabal SetupHooks
module Foreign.Swift.Lib
-- todo: probably rename this e.g. Data and/move something else soemwhere like Yield?

  ( plugin, SwiftExport(..), genSwiftActionAndAnn
    -- * Datatypes
  , swiftData
  , swiftPtr
  , yieldType
    -- * Functions
  , yieldFunction
    -- ** Re-exports
  , Aeson.deriveJSON, Aeson.defaultOptions
  , Proxy(..), ToMoatType
  ) where

import Control.Exception
import GHC.Utils.Trace
import GHC.Linker.Loader
import Control.Monad
import GHC
import Data.Data (Data)
import Data.List (intersperse)
import Data.Proxy (Proxy(..))
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.InstEnv
import GHC.Core.TyCo.Compare (eqType)
import GHC.Driver.Env
import GHC.Plugins
import GHC.Tc.Utils.Monad (TcM, TcGblEnv)
import GHC.Types.TyThing (MonadThings(..))
import GHC.Unit.External
import Language.Haskell.TH as TH hiding (Name, ppr)
import Language.Haskell.TH.Syntax (Lift (..))
import Moat
import System.Directory
import System.FilePath
import System.IO.Error
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.List as List
import qualified GHC.Core.Class as Core
import qualified GHC.Tc.Utils.Monad as TcM
import qualified GHC.Tc.Utils.TcType as Core
import qualified GHC.Types.Name.Occurrence as NameSpace
import qualified Language.Haskell.TH as TH (Name)

data SwiftExport = ExportSwiftData (String {- tycon name -})
                 | ExportSwiftFunction
  deriving (Show, Data, Lift)

instance Outputable SwiftExport where
  ppr s = text (show s)

--------------------------------------------------------------------------------
-- * Plugin
--------------------------------------------------------------------------------

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = \_ tds ->
      return (CoreDoPluginPass "swift-ffi" yieldSwiftCode : tds)
  , typeCheckResultAction = \_ -> typeCheckAct }

-- | The plugin will load expressions from this module, so it must be loaded
typeCheckAct :: ModSummary -> TcGblEnv -> TcM TcGblEnv
typeCheckAct ms g = do
  env <- TcM.getTopEnv
  -- pprTraceM "going to load" (empty)
  -- -- _ <- loadModuleInterface (text "load module interface for runtime loading of bcos here") (ms_mod ms)
  -- _ <- liftIO $ forceLoadModuleInterfaces env (text "load module interface for runtime loading of bcos here") [ms_mod ms]
  -- pprTraceM "loaded load" (empty)
  return g

yieldSwiftCode :: ModGuts -> CoreM ModGuts
yieldSwiftCode g@ModGuts{..} = do
  hsc_env <- getHscEnv
  let logger = hsc_logger hsc_env

  case ghcLink $ hsc_dflags hsc_env of
    -- Only run the plugin if the backend is not interpreter (useful for repl?)
    LinkInMemory -> return g
    _ -> do

      (_, anns) <- getFirstAnnotations deserializeWithData g

      let modPath = buildDir </> moduleNameSlashes (moduleName mg_module) <.> "hs"

      -- We tried to evaluate the expressions directly as bytecode, but that
      -- just wasn't working out (see commit history).
      -- Just write them all to a Haskell module which gets built and run at install time
      removeFile' modPath
      liftIO $ createDirectoryIfMissing True (takeDirectory modPath)
      liftIO $ writeFile modPath $ unlines
        [ "module Main where"
        , "import " ++ moduleNameString (moduleName mg_module)
        , ""
        , "main :: IO"
        , "main = do"
        ]

      forM_ (nonDetUFMToList anns) $ \case
        (uq, _exportType :: SwiftExport) -> do
          let ids = bindersOfBinds mg_binds
              Just genId = List.find (\x -> uq == getUnique x) ids
              genFunStr = showPprUnsafe genId
          liftIO $ appendFile modPath ("  " ++ genFunStr)

      return g

lookupInstance :: ModGuts -> Core.Class -> Core.Type -> CoreM (Maybe ClsInst)
lookupInstance ModGuts{mg_insts, mg_inst_env} cls ty = do
  eps <- liftIO =<< hscEPS <$> getHscEnv
  let instEnvs = InstEnvs { ie_global  = eps_inst_env eps
                          , ie_local   = mg_inst_env
                          , ie_visible = mempty }
  return $ case lookupInstEnv False instEnvs cls [ty] of
    (map fst -> matches, _, _) -> case matches of
      [] -> List.find (\ClsInst{is_tys=headTy:_,is_cls} -> ty `eqType` headTy && is_cls == cls) mg_insts :: Maybe ClsInst
      x:_ -> Just x

lookupFunctionId :: GlobalRdrEnv -> String -> CoreM Id
lookupFunctionId g s = lookupOcc NameSpace.varName g s >>= \case
  Nothing -> error $ "Id " ++ s ++ " not found in GRE " ++ showPprUnsafe g
  Just name -> lookupId name

-- | Find the TyCon by the given occurrence String, then construct a Type from
-- saturating that TyCon to as many type variables as necessary.
lookupTypeByName :: GlobalRdrEnv -> String -> CoreM (Maybe Core.Type)
lookupTypeByName gre occStr = do
  lookupOcc tcName gre occStr >>= \case
    Nothing -> pure Nothing
    Just name ->
      lookupThing name >>= \case
        ATyCon tc ->
          pure . Just =<< saturateTyConApp tc
        _ -> error "lookupTypeByName"

saturateTyConApp :: TyCon -> CoreM Core.Type
saturateTyConApp tc = do
  let arity = tyConArity tc
  uniqs <- take arity <$> getUniquesM
  let dummyVars = mkTyVarTys
        [ mkTyVar name liftedTypeKind
        | (i :: Int, uq) <- zip [1..] uniqs
        , let name = mkInternalName uq (mkTyVarOcc ("a" ++ show i)) noSrcSpan
        ]
  return $ mkTyConApp tc dummyVars

lookupOcc :: GHC.Plugins.NameSpace -> GlobalRdrEnv -> String -> CoreM (Maybe Name)
lookupOcc ns gre occStr =
  let occ = GHC.Plugins.mkOccName ns occStr
      elts = lookupGRE gre (LookupOccName occ SameNameSpace)
  in case elts of
      [] -> pure Nothing
      (r:_) -> do
        let name = gre_name r -- Take the first match
        pure (Just name)

getProxyDataCon :: GlobalRdrEnv -> CoreM Id
getProxyDataCon mg_rdr_env =
  lookupOcc dataName mg_rdr_env "Proxy" >>= \case
    Nothing -> error "Proxy datacon is not in scope, please import it:\n\nimport Data.Proxy (Proxy(..))\n\nIt should have been re-exported from Foreign.Swift.*!"
    Just thing -> lookupThing thing >>= \case
      AConLike (RealDataCon cl)
        -> pure $ dataConWorkId cl
      _ -> error "unexpected"

getToMoatTypeCls :: GlobalRdrEnv -> CoreM Core.Class
getToMoatTypeCls mg_rdr_env =
  lookupOcc clsName mg_rdr_env "ToMoatType" >>= \case
    Nothing -> error "ToMoatType tycon is not in scope, please import it:\n\n    import Moat\n\nIt should have been re-exported from Foreign.Swift.*!"
    Just n -> lookupThing n >>= \case
      ATyCon tc -> case tyConClass_maybe tc of
        Nothing -> error "expected class"
        Just cl -> return cl
      _ -> error "unexpected"


removeFile' :: TcM.MonadIO m => FilePath -> m ()
removeFile' loc = liftIO $
  removeFile loc `catch ` \case e | isDoesNotExistError e -> pure ()
                                  | otherwise -> throwIO e

--------------------------------------------------------------------------------
-- * Types / Data
--------------------------------------------------------------------------------

-- | Yield a datatype declaration for the given datatype name
--
-- Example of top level splice: @$(swiftData ''User)@
swiftData :: TH.Name -> Q [Dec]
swiftData name = do
  -- generate Moat class instances
  mg <- mobileGenWith defaultOptions { dataProtocols = [Codable] } name

  kind <- reifyType name
  let tyVars (AppT a x) = a:tyVars x
      tyVars _ret = []
  tyNames <- mapM (\_ -> VarT <$> newName "a") (tyVars kind)
  let typ = foldl' AppT (ConT name) tyNames
      -- Like `typ`, but use units. Used in genSwiftActionAndAnn rather than
      -- using metavariables when applying to phantom roles (only way...)
      -- How to generate types that are generic? I think this may be the right way but I'm unsure.
      tyUnits = map (\_ -> ConT ''()) (tyVars kind)
      typWithUnits = foldl' AppT (ConT name) tyUnits

  hasToJSON <- isInstance ''Aeson.ToJSON [typ]
  hasFromJSON <- isInstance ''Aeson.FromJSON [typ]

  toJSON   <- if hasToJSON then pure []
              else Aeson.deriveToJSON Aeson.defaultOptions name
  fromJSON <- if hasFromJSON then pure []
              else Aeson.deriveFromJSON Aeson.defaultOptions name

  gens <- genSwiftActionAndAnn yieldType name typWithUnits [|| ExportSwiftData $$(unsafeCodeCoerce [| $(litE $ StringL $ nameBase name) |]) ||]

  return (mg ++ toJSON ++ fromJSON ++ gens)

-- | Generate a ToMoatType for a datatype that will wrap a stable pointer
-- TODO: Support for types with type variables
swiftPtr :: TH.Name -> Q [Dec]
swiftPtr name = do
  kind <- reifyType name
      -- As above
  let tyVars (AppT a x) = a:tyVars x
      tyVars _ret = []
      tyUnits = map (\_ -> ConT ''()) (tyVars kind)
      typWithUnits = foldl' AppT (ConT name) tyUnits
  insts <- [d|
      instance ToMoatType $(conT name) where
        toMoatType _ = Concrete
          { concreteName = $(lift $ nameBase name)
          , concreteTyVars = [{-todo-}] }

      instance ToMoatData $(conT name) where
        -- todo: moat newtype?
        toMoatData _ = MoatAlias
          { aliasName = $(lift $ nameBase name)
          , aliasDoc = Nothing -- todo
          , aliasTyVars = [] -- todo
          , aliasTyp = Concrete { concreteName = "UnsafeMutableRawPointer", concreteTyVars = [] }
          }
    |]
  gens <- genSwiftActionAndAnn yieldType name typWithUnits [|| ExportSwiftData $$(unsafeCodeCoerce [| $(litE $ StringL $ nameBase name) |]) ||]
  return (insts ++ gens)


-- | Output a Swift datatype declaration to a file using the current module
-- name as the filename.
yieldType :: Q Exp -- ^ Proxy @ty expression
          -> String -- gen name (ignored)
          -> Q Exp
yieldType prx _ = do
  outputCode [| pure $ generateSwiftCode $prx ++ "\n" |]

-- | Generate an IO action which, when run, will generate swift code and output
-- it to a file.
-- It also attaches an annotation pragma so the plugin can find this action and run it.
genSwiftActionAndAnn :: (Q Exp -> String -> Q Exp) -> TH.Name -> TH.Type -> Code Q SwiftExport -> Q [Dec]
genSwiftActionAndAnn yielding name ty exportAnn = do
  genName <- newName ("gen_swift_" ++ nameBase name)
  ann <- AnnP (ValueAnnotation genName) <$> unTypeCode exportAnn
  -- The generated function which outputs Swift code is going to be run by the plugin
  genFunSig <- TH.SigD genName <$> [t| IO () |]
  genFunBody <- yielding [| Proxy @($(pure ty)) |] (nameBase genName)
  let genFun = FunD genName [Clause [] (NormalB genFunBody) []]
  return [genFun, genFunSig, PragmaD ann]

--------------------------------------------------------------------------------
-- * Functions
--------------------------------------------------------------------------------

-- | Yield a Haskell function as Swift code wrapping a foreign call.
-- It also exports the function to the foreign interface
--
-- @
-- yieldFunction @(Query -> Int -> String) 'f [Just "x", Nothing] Proxy
-- @
yieldFunction :: Q Exp  -- ^ Proxy @ty expr
              -> String -- ^ Function name
              -> Q Exp
yieldFunction prx name = do
  outputCode [|
    do
      let moatTy = toMoatType $prx
          (argTys, retTy) = splitRetMoatTy ([], undefined) moatTy
          splitRetMoatTy (args, r) (Moat.App arg c) = splitRetMoatTy (arg:args, r) c
          splitRetMoatTy (args, _) ret = (reverse args, ret)

      swiftParams <- liftIO $ zipWithM prettyParam [1..] argTys

      return $ unlines
        [ "@ForeignImportHaskell"
        , "func " ++ $(litE $ StringL name) ++ "(cconv: HsCallJSON" ++ (if null swiftParams then "" else ", ") ++ concat (intersperse ", " swiftParams) ++ ")"
                  ++ " -> " ++ prettyMoatType retTy
        , "{ stub() }"
        ]
    |]

--------------------------------------------------------------------------------
-- * Utils
--------------------------------------------------------------------------------

-- | Yield an expression which will yield a piece of code to the current module being generated.
outputCode :: Q Exp -> Q Exp
outputCode produceCode = do
  loc <- (buildDir </>) . modSwiftLoc . loc_module <$> location

  -- When this splice runs (rather than when the produced expression is run)
  -- reset the files written by it
  runIO $ removeFile' loc
  [| do

      -- When mod is finalizer, write all swift code to file from scratch
      createDirectoryIfMissing True (takeDirectory loc)
      putStrLn $ "Writing Swift code to: " ++ loc
      code <- $produceCode
      appendFile loc (code ++ "\n")
   |]

-- | Where to write lib
buildDir :: FilePath
buildDir = "_build"

-- | Convert a module's 'Location' into the relative path to the Swift file we're writing
modSwiftLoc :: String -> FilePath
modSwiftLoc l = map (\case '.' -> '/'; x -> x) l <.> "swift"

generateSwiftCode :: ToMoatData a => Proxy a -> String
generateSwiftCode = prettySwiftData . toMoatData

prettyParam :: Int -> MoatType -> IO String
prettyParam ix fieldType = do
  internalFieldName <- newName ("v" ++ show ix)
  let fName = "_ " ++ nameBase internalFieldName
  return $ fName ++ ": " ++ prettyMoatType fieldType ++ (if isOptional fieldType then " = nil" else "")

isOptional :: MoatType -> Bool
isOptional (Optional _) = True
isOptional _ = False


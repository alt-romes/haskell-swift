{-# LANGUAGE DataKinds #-}
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

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import Language.Haskell.TH as TH hiding (Name, ppr)
import qualified Language.Haskell.TH as TH (Name)
import Moat
import Data.Proxy (Proxy(..))

import System.FilePath
import System.Directory
import Control.Exception
import System.IO.Error

import Data.List (intersperse)
import Control.Monad
import Data.Data (Data)

import GHC.Plugins
import GHC.Utils.Trace
import GHC.Types.TyThing (MonadThings(..), TyThing (..), tyThingId)
import qualified GHC.Tc.Utils.TcType as Core
import qualified Data.List as List
import qualified GHC.Types.Name.Occurrence as NameSpace
import GHC.Driver.Main (hscCompileCoreExpr)
import qualified GHC.Core as Core
import GHC.Core.InstEnv
import GHC.Runtime.Interpreter
import GHC.Driver.Config (initEvalOpts)
import GHC.Core.ConLike (ConLike(..))
import GHC.Unit.External
import GHC.Core.TyCo.Compare (eqType)
import qualified GHC.Core.Class as Core
import Language.Haskell.TH.Syntax (Lift (..))

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
      return (CoreDoPluginPass "swift-ffi" yieldSwiftCode : tds) }

yieldSwiftCode :: ModGuts -> CoreM ModGuts
yieldSwiftCode g@ModGuts{..} = do
  hsc_env <- getHscEnv
  let modname = moduleNameString . moduleName $ mg_module

  (_, anns) <- getFirstAnnotations deserializeWithData g

  -- yieldTypeId       <- lookupFunctionId mg_rdr_env "yieldType"
  --
  -- yieldFunctionId   <- lookupFunctionId mg_rdr_env "yieldFunction"
  -- proxyDataCon      <- getProxyDataCon  mg_rdr_env
  -- toMoatTypeCls     <- getToMoatTypeCls mg_rdr_env
  --
  -- let proxy ty    = Core.Var proxyDataCon `Core.App` Core.Type ty
  --     getMoatTyInstDict ty = do
  --       lookupInstance g toMoatTypeCls ty >>= \case
  --         Just ClsInst{is_dfun} -> return is_dfun
  --         Nothing -> error $ "MoatTyInst not found for " ++ showPprUnsafe ty

  forM_ (nonDetUFMToList anns) $ \case
    (uq, _exportType :: SwiftExport) -> do
      let
        bindsToIds (NonRec v _)   = [v]
        bindsToIds (Rec    binds) = map fst binds
        ids = concatMap bindsToIds mg_binds

      let Just genId = List.find (\x -> uq == getUnique x) ids
      liftIO $ putStrLn "Generating..."
      runCoreExpr (Var genId)
      liftIO $ putStrLn "Done"


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


-- | Interpret a CoreExpr and drop the result
runCoreExpr :: CoreExpr -> CoreM ()
runCoreExpr expr = do
  hsc_env <- getHscEnv
  pprTraceM "Evaluating expression..." (ppr expr)
  (fval, _, _) <- liftIO $ hscCompileCoreExpr hsc_env noSrcSpan expr
  _ <- liftIO $ evalStmt (hscInterp hsc_env) (initEvalOpts (hsc_dflags hsc_env) False) (EvalThis fval)
  return ()

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

  gens <- genSwiftActionAndAnn name typWithUnits [|| ExportSwiftData $$(unsafeCodeCoerce [| $(litE $ StringL $ nameBase name) |]) ||]

  return (mg ++ toJSON ++ fromJSON ++ gens)

-- | Generate a ToMoatType for a datatype that will wrap a stable pointer
-- TODO: Support for types with type variables
swiftPtr :: TH.Name -> Q [Dec]
swiftPtr name = do
  [d| instance ToMoatType $(conT name) where
        toMoatType _ = Concrete
          { concreteName = $(lift $ nameBase name)
          , concreteTyVars = [{-todo-}] }

      instance ToMoatData $(conT name) where
        toMoatData _ = MoatAlias
          { aliasName = $(lift $ nameBase name)
          , aliasDoc = Nothing -- todo
          , aliasTyVars = [] -- todo
          , aliasTyp = Concrete { concreteName = "UnsafeMutableRawPointer", concreteTyVars = [] }
          }
    |]


-- | Output a Swift datatype declaration to a file using the current module
-- name as the filename.
yieldType :: Q Exp -- ^ Proxy @ty expression
          -> Q Exp
yieldType prx = do
  outputCode [| generateSwiftCode $prx ++ "\n" |]

-- | Generate an IO action which, when run, will generate swift code and output
-- it to a file.
-- It also attaches an annotation pragma so the plugin can find this action and run it.
genSwiftActionAndAnn :: TH.Name -> TH.Type -> Code Q SwiftExport -> Q [Dec]
genSwiftActionAndAnn name ty exportAnn = do
  genName <- newName ("gen_swift_" ++ nameBase name)
  ann <- AnnP (ValueAnnotation genName) <$> unTypeCode exportAnn
  -- The generated function which outputs Swift code is going to be run by the plugin
  genFunSig <- SigD genName <$> [t| IO () |]
  genFunBody <- yieldFunction [| Proxy @($(pure ty)) |] (nameBase genName)
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
  loc <- (buildDir </>) . locToFile . loc_module <$> location

  -- When this splice runs (rather than when the produced expression is run)
  -- reset the files written by it
  runIO $
    removeFile loc `catch ` \case e | isDoesNotExistError e -> pure ()
                                    | otherwise -> throwIO e
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
locToFile :: String -> FilePath
locToFile l = map (\case '.' -> '/'; x -> x) l <.> "swift"

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


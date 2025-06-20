{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
-- | Produce a swift library by building a Haskell package using
-- TemplateHaskell and Cabal SetupHooks
module Foreign.Swift.Lib
-- todo: probably rename this e.g. Data and/move something else soemwhere like Yield?

  ( plugin, SwiftExport(..)
    -- * Datatypes
  , swiftData
  , yieldType
    -- * Functions
  , yieldFunction
    -- ** Re-exports
  , Aeson.deriveJSON, Aeson.defaultOptions
  , Proxy(..), ToMoatType
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Kind as K
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
import GHC.Types.TyThing (MonadThings(..), TyThing (..))
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
import Data.IORef

data SwiftExport = ExportSwiftData (String {- tycon name -})
                 | ExportSwiftFunction
  deriving (Show, Data)

instance Outputable SwiftExport where
  ppr s = text (show s)

--------------------------------------------------------------------------------
-- * Plugin
--------------------------------------------------------------------------------

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = \_ tds ->
      return (tds ++ [CoreDoPluginPass "swift-ffi" yieldSwiftCode]) }

yieldSwiftCode :: ModGuts -> CoreM ModGuts
yieldSwiftCode g@ModGuts{..} = do
  let modname = moduleNameString . moduleName $ mg_module

  yieldTypeId       <- lookupFunctionId mg_rdr_env "yieldType"
  yieldFunctionId   <- lookupFunctionId mg_rdr_env "yieldFunction"
  proxyDataCon      <- getProxyDataCon  mg_rdr_env
  toMoatTypeCls     <- getToMoatTypeCls mg_rdr_env

  let proxy ty    = Core.Var proxyDataCon `Core.App` Core.Type ty
      getMoatTyInstDict ty = do
        lookupInstance g toMoatTypeCls ty >>= \case
          Just ClsInst{is_dfun} -> return is_dfun
          Nothing -> error $ "MoatTyInst not found for " ++ showPprUnsafe ty

  (_, anns) <- getFirstAnnotations deserializeWithData g

  forM_ (nonDetUFMToList anns) $ \case
    (_u, ExportSwiftData str) -> do
      lookupTypeByName mg_rdr_env str >>= \case
        Nothing -> error "exported data not found!?"
        Just ty -> do
          pprTraceM ("found type for " ++ str ++ ": ") (ppr ty)
          dictExpr <- Core.Var <$> getMoatTyInstDict ty
          runCoreExpr (Core.Var yieldTypeId `Core.App` dictExpr
                                            `Core.App` (proxy ty))
    (uq, ExportSwiftFunction) -> do
      -- linear search; whatever
      case List.find (\b -> getUnique b == uq) (bindersOfBinds mg_binds) of
        Nothing -> error "exported function not found!"
        Just bndr -> do
          let ty = idType bndr
          dictExpr <- Core.Var <$> getMoatTyInstDict ty
          pprTraceM ("found type for: ") (ppr (getName bndr) <+> ppr ty)
          funNameExpr <- mkStringExpr (occNameString $ getOccName bndr)
          modnameExpr <- mkStringExpr modname
          runCoreExpr (Core.Var yieldFunctionId `Core.App` dictExpr
                                                `Core.App` proxy ty
                                                `Core.App` funNameExpr
                                                `Core.App` modnameExpr)

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

  hasToJSON <- isInstance ''Aeson.ToJSON [typ]
  hasFromJSON <- isInstance ''Aeson.FromJSON [typ]

  toJSON   <- if hasToJSON then pure []
              else Aeson.deriveToJSON Aeson.defaultOptions name
  fromJSON <- if hasFromJSON then pure []
              else Aeson.deriveFromJSON Aeson.defaultOptions name

  ann <- AnnP (TypeAnnotation name) <$> [| ExportSwiftData $(litE $ StringL $ nameBase name) |]

  return (PragmaD ann : mg ++ toJSON ++ fromJSON)

-- | Output a Swift datatype declaration to a file using the current module
-- name as the filename.
yieldType :: forall (a :: K.Type) c. ToMoatData a => Proxy a -> String
          -> IO [c] -- ^ Return type compatible with @'evalStmt'@
yieldType prx modname = do
  outputCode modname $ generateSwiftCode prx ++ "\n"
  return []

--------------------------------------------------------------------------------
-- * Functions
--------------------------------------------------------------------------------

-- | Yield a Haskell function as Swift code wrapping a foreign call.
-- It also exports the function to the foreign interface
--
-- @
-- yieldFunction @(Query -> Int -> String) 'f [Just "x", Nothing] Proxy
-- @
yieldFunction :: forall (a :: K.Type) b. ToMoatType a
              => Proxy a
              -> String -- ^ Function name
              -> String -- ^ Module name
              -> IO [b] -- ^ Return type compatible with @'evalStmt'@
yieldFunction prx name modname = do
  -- Function must be exported
  let moatTy = toMoatType prx
      (argTys, retTy) = splitRetMoatTy ([], undefined) moatTy

  swiftParams <- zipWithM prettyParam [1..] argTys

  outputCode modname $ unlines
    [ "@ForeignImportHaskell"
    , "func " ++ name ++ "(cconv: HsCallJSON" ++ (if null swiftParams then "" else ", ") ++ concat (intersperse ", " swiftParams) ++ ")"
              ++ " -> " ++ prettyMoatType retTy
    , "{ stub() }"
    ]
  return []

  where
    splitRetMoatTy (args, r) (Moat.App arg c) = splitRetMoatTy (arg:args, r) c
    splitRetMoatTy (args, _) ret = (reverse args, ret)

--------------------------------------------------------------------------------
-- * Utils
--------------------------------------------------------------------------------

-- | Yield a piece of code to the current module being generated.
-- Note that code is only yielded after typechecking (as a finalizer)
outputCode :: String -> String -> IO ()
outputCode modname code = do
  let loc = buildDir </> locToFile modname

  -- When this splice runs (rather than when module is finalized)
  -- Reset the files written by it
  removeFile loc `catch ` \case e | isDoesNotExistError e -> pure ()
                                  | otherwise -> throwIO e

  -- When mod is finalizer, write all swift code to file from scratch
  createDirectoryIfMissing True (takeDirectory loc)
  putStrLn $ "Writing Swift code to: " ++ loc
  appendFile loc (code ++ "\n")

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


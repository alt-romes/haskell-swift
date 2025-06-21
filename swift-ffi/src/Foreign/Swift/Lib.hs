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

import GHC.Iface.Make
import Control.Exception
import GHC.Utils.Trace
import GHC.Linker.Loader
import GHC.Unit.Module.ModDetails
import GHC.Unit.Finder
import GHC.Types.ForeignStubs
import Control.Monad
import GHC
import Data.Data (Data)
import Data.List (intersperse)
import Data.Maybe
import Data.Proxy (Proxy(..))
import GHC.ByteCode.Types
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.InstEnv
import GHC.Core.TyCo.Compare (eqType)
import GHC.Driver.Env
import GHC.Driver.Main hiding (getHscEnv)
import GHC.Linker.Types
import GHC.Plugins
import GHC.Runtime.Context (InteractiveImport(..))
import GHC.Runtime.Eval
import GHC.Tc.Utils.Monad (TcM, TcGblEnv)
import GHC.Types.TyThing (MonadThings(..), TyThing (..))
import GHC.Unit.External
import GHC.Unit.Home.ModInfo
import Language.Haskell.TH as TH hiding (Name, ppr)
import Language.Haskell.TH.Syntax (Lift (..))
import Moat
import System.Directory
import System.FilePath
import System.IO.Error
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.List as List
import qualified GHC
import qualified GHC.Core.Class as Core
import qualified GHC.Tc.Utils.Monad as TcM
import qualified GHC.Tc.Utils.TcType as Core
import qualified GHC.Types.Name.Occurrence as NameSpace
import qualified GHC.Unit.Env as HUG
import qualified GHC.Unit.Home.Graph as HUG
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
    -- Only run the plugin if the backend is not interpreter (to make sure we don't loop by calling load again; also useful for repl)
    LinkInMemory -> return g
    _ -> do

      (_, anns) <- getFirstAnnotations deserializeWithData g

      -- Run things within a Ghc monad using the current HscEnv as the base
      liftIO $ GHC.runGhc Nothing $ do
        GHC.setSession hsc_env -- (set current env)

        -- let tidy_occ_env = initTidyOccEnv [occName exprId]
        -- let tidy_env     = mkEmptyTidyEnv tidy_occ_env
        -- let tidy_expr    = tidyExpr tidy_env expr

        {- Prepare for codegen -}
        -- cp_cfg :: IO <- initCorePrepConfig hsc_env
        -- prepd_expr :: IO <- corePrepExpr
        --  logger cp_cfg
        --  tidy_expr
        --
        -- let (stg_binds0,_,_) = coreToStg (initCoreToStgOpts dflags) this_mod undefined [NonRec exprId expr]
        -- (stg_binds1,_) <- stg2stg (hsc_logger hsc_env) (interactiveInScope (hsc_IC hsc_env)) (initStgPipelineOpts dflags True) this_mod stg_binds0
        -- let (stg_binds, _stg_deps) = unzip stg_binds1
        --
        -- bcos <- byteCodeGen hsc_env this_mod stg_binds [] Nothing modbreaks []
        --
        -- bco_time <- getCurrentTime
        -- (fv_hvs, mods_needed, units_needed) <- loadDecls interp hsc_env noSrcSpan $ Linkable bco_time this_mod $ NE.singleton $ BCOs bcos
        -- let Just fval = (lookup (idName exprId) fv_hvs)
        --

        -- To run the generation code use the same flags as before, but set
        -- interpreted mode (this also means we won't recursively run the
        -- plugin while trying to load the code)
        -- dflags0 <- GHC.getSessionDynFlags
        -- let dflags' = dflags0 { backend = GHC.interpreterBackend, ghcLink = LinkInMemory }
        -- _ <- GHC.setSessionDynFlags dflags'

        liftIO $ putStrLn "REGENERATING...."
        -- TODO: This forces everything to be recompiled a second time... can't we do better?
        -- _ <- GHC.load GHC.LoadAllTargets -- (Targets were already set in the hsc_env)

        -- old_hmi <- liftIO $ HUG.lookupHugByModule mg_module (hsc_HUG hsc_env)
        -- hmi <- liftIO $ compileOne (hsc_env{hsc_plugins=emptyPlugins, hsc_dflags=dflags'{pluginModNames=[], pluginModNameOpts=[], externalPluginSpecs=[]}}) modsum 0 1 (hm_iface <$> old_hmi) (maybe emptyHomeModInfoLinkable hm_linkable old_hmi)
        -- liftIO $ hscInsertHPT hmi hsc_env

        modsum <- GHC.getModSummary mg_module
        (cgguts, details) <- liftIO $ hscTidy hsc_env g

        !partial_iface <- liftIO $ mkPartialIface hsc_env mg_binds details modsum [] g
        final_iface <- liftIO $ mkFullIface hsc_env partial_iface Nothing Nothing NoStubs []

        -- hscMaybeWriteIface logger dflags True final_iface mb_old_iface_hash location
        bc <- liftIO $ generateFreshByteCode hsc_env (moduleName mg_module) (mkCgInteractiveGuts cgguts) (error "todo: no mod_location")

        let iface = final_iface
            linkable = emptyHomeModInfoLinkable { homeMod_bytecode = Just bc }

        -- details <- liftIO $ genModDetails hsc_env iface
        -- mb_object <- liftIO $ findObjectLinkableMaybe (mi_module iface) undefined
        -- mb_bytecode <- liftIO $ loadIfaceByteCodeLazy hsc_env iface undefined (md_types details)
        -- let hm_linkable = HomeModLinkable mb_bytecode mb_object
        -- let hmi =  (HomeModInfo iface details hm_linkable)

        details <- liftIO $ initModDetails hsc_env iface
        linkable' <- liftIO $ traverse (initWholeCoreBindings hsc_env iface details) (homeMod_bytecode linkable)
        let hmi = HomeModInfo iface details (linkable { homeMod_bytecode = linkable' })
        liftIO $ hscAddSptEntries hsc_env
                  [ spt
                  | linkable <- maybeToList (homeModInfoByteCode hmi)
                  , bco <- linkableBCOs linkable
                  , spt <- bc_spt_entries bco
                  ]
        liftIO $ HUG.addHomeModInfoToHug hmi (hsc_HUG hsc_env)

        -- mb_bytecode <- liftIO $ loadIfaceByteCodeLazy hsc_env iface undefined (md_types details)
        -- let hm_linkable = HomeModLinkable mb_bytecode Nothing
        pprTraceM "doing load decls" =<< liftIO (showLoaderState (hscInterp hsc_env))
        liftIO $ loadDecls (hscInterp hsc_env) hsc_env noSrcSpan (fromJust $ homeModInfoByteCode hmi)

        -- pprTraceM "doing load decls" =<< liftIO (showLoaderState (hscInterp hsc_env))
        -- liftIO $ loadDecls (hscInterp hsc_env) hsc_env noSrcSpan (fromJust linkable')
        -- pprTraceM "did load decls" =<< liftIO (showLoaderState (hscInterp hsc_env))

        -- Import module, making sure top binds are in scope on eval
        _ <- setContext [ IIDecl $ GHC.simpleImportDecl (moduleName mg_module) ]
        liftIO $ putStrLn "LOADED OK...."

        -- Restore after having loaded using LinkInMemory?!
        forM_ (nonDetUFMToList anns) $ \case
          (uq, _exportType :: SwiftExport) -> do
            let ids = bindersOfBinds mg_binds
            let Just genId = List.find (\x -> uq == getUnique x) ids
            liftIO $ putStrLn "Generating..."
            -- It would be cooler to use something like evalIO directly, but
            -- simply print out the Id as a string and interpret that as a
            -- string variable occurrence.
            pprTraceM "SHOW LOADER STATE" =<< liftIO (showLoaderState (hscInterp hsc_env))
            _ <- execStmt (showPprUnsafe genId) execOptions
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


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
import qualified Data.List.NonEmpty as NE
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
#if __GLASGOW_HASKELL__ >= 913
import qualified GHC.Unit.Home.Graph as HUG
#endif
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

  case ghcLink $ hsc_dflags hsc_env of
    -- Only run the plugin if the backend is not interpreter (useful for repl whose CWD is typically with HLS the project root)
    LinkInMemory -> return g
    _ -> do

      (_, anns) <- getFirstAnnotations deserializeWithData g

      -- Run things within a Ghc monad using the current HscEnv as the base
      liftIO $ GHC.runGhc Nothing $ do
        GHC.setSession hsc_env

#if __GLASGOW_HASKELL__ >= 913
        modsum <- GHC.getModSummary mg_module
#else
        modsum <- GHC.getModSummary (moduleName mg_module)
#endif
        (cgguts, details) <- liftIO $ hscTidy hsc_env g

#if __GLASGOW_HASKELL__ >= 912
        !partial_iface <- liftIO $ mkPartialIface hsc_env mg_binds details modsum [] g
        final_iface <- liftIO $ mkFullIface hsc_env partial_iface Nothing Nothing NoStubs []
#else
        let !partial_iface = mkPartialIface hsc_env mg_binds details modsum g
        final_iface <- liftIO $ mkFullIface hsc_env partial_iface Nothing Nothing
#endif

        bc <- liftIO $ generateFreshByteCode hsc_env (moduleName mg_module) (mkCgInteractiveGuts cgguts) (error "todo: no mod_location")

        let iface = final_iface
            linkable = emptyHomeModInfoLinkable { homeMod_bytecode = Just bc }

        details <- liftIO $ initModDetails hsc_env iface
        linkable0' <- liftIO $ traverse (initWholeCoreBindings hsc_env iface details) (homeMod_bytecode linkable)
        -- Drop foreign objects produced by foreign exports because for the
        -- interpreter we won't have the symbols it will try to resolve and fail
#if __GLASGOW_HASKELL__ >= 912
        let linkable' = (\t -> t{linkableParts = NE.fromList $ NE.filter (\case DotO _ ForeignObject -> False;_ -> True) $ linkableParts t}) <$> linkable0'
        let hmi = HomeModInfo iface details (linkable { homeMod_bytecode = linkable' })
        liftIO $ hscAddSptEntries hsc_env
                  [ spt
                  | linkable <- maybeToList (homeModInfoByteCode hmi)
                  , bco <- byteCodeOfObject linkable
                  , spt <- bc_spt_entries bco
                  ]
        liftIO $ HUG.addHomeModInfoToHug hmi (hsc_HUG hsc_env)
#else
        -- For ghc9.10 just filter out ALL DotOs... not great but works for now and complex cases should use 9.14
        let linkable' = (\t -> t{linkableUnlinked = filter (\case DotO _ -> False;_ -> True) $ linkableUnlinked t}) <$> linkable0'
        let hmi = HomeModInfo iface details (linkable { homeMod_bytecode = linkable' })
        setSession $ hscUpdateHPT (addHomeModInfoToHpt hmi) hsc_env
#endif

        -- Import module, making sure top binds are in scope on eval
        _ <- setContext [ IIDecl $ GHC.simpleImportDecl (moduleName mg_module) ]

        -- Restore after having loaded using LinkInMemory?!
        forM_ (nonDetUFMToList anns) $ \case
          (uq, _exportType :: SwiftExport) -> do
            let ids = bindersOfBinds mg_binds
                Just genId = List.find (\x -> uq == getUnique x) ids
            -- It would be cooler to use something like evalIO directly, but
            -- simply print out the Id as a string and interpret that as a
            -- string variable occurrence.
            execStmt (showPprUnsafe genId) execOptions
      return g

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
          , aliasTyp = unsafeMutableRawPointerType
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
yieldFunction :: ([TH.Type], TH.Type) -- ^ The original type 
              -> Q Exp  -- ^ Proxy @ty expr
              -> String -- ^ Generated function name
              -> Q Exp
yieldFunction (origArgsTy, origResTy) prx name = do
  outputCode [|
    do
      let moatTy = toMoatType $prx
          moatData = toMoatData $prx
          (argTys, retTy) = splitRetMoatTy ([], undefined) moatTy
          splitRetMoatTy (args, r) (Moat.App arg c) = splitRetMoatTy (arg:args, r) c
          splitRetMoatTy (args, _) ret = (reverse args, ret)

      swiftParams <- liftIO $ zipWithM prettyParam [1..] argTys

      return $ unlines
        [ -- let's just do it inline rather than -- "@ForeignImportHaskell"
          "public func " ++ $(litE $ StringL name) ++ "(" ++ concat (intersperse ", " swiftParams) ++ ")"
                  ++ " -> " ++ prettyMoatType retTy
        , "{"
        ]
        ++
        [ "let hs_enc = JSONEncoder()" | not (null swiftParams) ]
        [ "let hs_dec = JSONDecoder()" | MoatAlias <- retTy ]
        ++
        [ "}" ]
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

unsafeMutableRawPointerType :: MoatType
unsafeMutableRawPointerType = Concrete { concreteName = "UnsafeMutableRawPointer", concreteTyVars = [] }

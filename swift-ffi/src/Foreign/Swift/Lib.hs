{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
-- | Produce a swift library by building a Haskell package using
-- TemplateHaskell and Cabal SetupHooks
module Foreign.Swift.Lib
  ( plugin, SwiftExport(..), genSwiftActionAndAnn
    -- * Datatypes
  , swiftData, swiftDataWith, moatDefOpts
  , swiftPtr
  , yieldType
    -- * Functions
  , yieldFunction
    -- * Utils
  , locToFile, buildDir, sourcesDir, haskellSourcesDir, swiftSourcesDir
    -- ** Re-exports
  , Aeson.deriveJSON, aesonDefaultOptions
  , Proxy(..), ToMoatType(..), ToMoatData(..), MoatType(..), MoatData(..), Options(..)
  ) where

import GHC.Iface.Make
import Control.Exception
import Control.Monad
import GHC
import Data.Data (Data)
import Data.List (intersperse)
import Data.Proxy (Proxy(..))
import GHC.Driver.Env
import GHC.Driver.Main hiding (getHscEnv)
import GHC.Linker.Types
import GHC.Plugins
import GHC.Tc.Utils.Monad (TcM, TcGblEnv)
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
#if __GLASGOW_HASKELL__ >= 913
import qualified GHC.Unit.Home.Graph as HUG
#endif
import qualified Language.Haskell.TH as TH (Name)
import qualified Data.Char as Char
import qualified Data.Monoid as Monoid

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

typeCheckAct :: ModSummary -> TcGblEnv -> TcM TcGblEnv
typeCheckAct _ms g = return g

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

moatDefOpts :: Options
moatDefOpts = defaultOptions

aesonDefaultOptions :: Aeson.Options
aesonDefaultOptions = Aeson.defaultOptions
  { Aeson.constructorTagModifier = \case
      [] -> []
      (x:xs) -> Char.toLower x : xs
  , Aeson.allNullaryToStringTag = True -- default
  }

-- | Yield a datatype declaration for the given datatype name
--
-- Example of top level splice: @$(swiftData ''User)@
swiftData :: TH.Name -> Q [Dec]
swiftData = swiftDataWith defaultOptions

-- | Like 'swiftData' but with additional 'Moat' 'Options'
swiftDataWith :: Options -> TH.Name -> Q [Dec]
swiftDataWith moatopts name = do
  -- generate Moat class instances
  -- TODO: ONLY IF THEY DO NOT EXIST YET!
  -- we may want to use custom ones!

  info <- reify name
  let shouldDeriveString = case info of
        TyConI (DataD _ _ _ _ cons _)
          -- All constructors are nullary and there's more than one constructor,
          -- then we want to Derive String to Match Aeson's allNullaryToStringTag = True encoding
          | length cons > 1
          , all (\case NormalC _ [] -> True
                       _ -> False
                ) cons
          -> True
        _ -> False

  mg <- mobileGenWith moatopts
          { dataProtocols =
              (if shouldDeriveString then [OtherProtocol "String"] else [])
              ++ Codable:(dataProtocols moatopts)
          , typeAlias = True
          } name

  kind <- reifyType name
  let tyVars (AppT a x) = a:tyVars x
      tyVars _ret = []
  tyNames <- mapM (\_ -> VarT <$> newName "a") (tyVars kind)
  let typ = foldl' AppT (ConT name) tyNames
      -- Like `typ`, but use units. Used in genSwiftActionAndAnn rather than
      -- using metavariables when applying to phantom roles (only way...)
      -- How to generate types that are generic? I think this may be the right way but I'm unsure.
      tyUnits = map (\_ -> ConT ''X) (tyVars kind)
      typWithUnits = foldl' AppT (ConT name) tyUnits

  hasToJSON <- isInstance ''Aeson.ToJSON [typ]
  hasFromJSON <- isInstance ''Aeson.FromJSON [typ]

  toJSON   <- if hasToJSON then pure []
              else Aeson.deriveToJSON aesonDefaultOptions name
  fromJSON <- if hasFromJSON then pure []
              else Aeson.deriveFromJSON aesonDefaultOptions name

  gens <- genSwiftActionAndAnn (yieldType typWithUnits) name [|| ExportSwiftData $$(unsafeCodeCoerce [| $(litE $ StringL $ nameBase name) |]) ||]

  return (mg ++ toJSON ++ fromJSON ++ gens)

-- | Generate a ToMoatType for a datatype that will wrap a stable pointer
-- TODO: Support for types with type variables
swiftPtr :: TH.Name -> Q [Dec]
swiftPtr name = do
  kind <- reifyType name
      -- As above
  let tyVars (AppT a x) = a:tyVars x
      tyVars _ret = []
      tyUnits = map (\_ -> ConT ''X) (tyVars kind)
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
  gens <- genSwiftActionAndAnn (yieldType typWithUnits) name [|| ExportSwiftData $$(unsafeCodeCoerce [| $(litE $ StringL $ nameBase name) |]) ||]
  return (insts ++ gens)


-- | Output a Swift datatype declaration to a file using the current module
-- name as the filename.
yieldType :: TH.Type
          -> Q Exp
yieldType prx = do
  outputCode [| pure $ generateSwiftCode (Proxy @($(pure prx))) ++ "\n" |]

-- | Generate an IO action which, when run, will generate swift code and output
-- it to a file.
-- It also attaches an annotation pragma so the plugin can find this action and run it.
genSwiftActionAndAnn :: Q Exp -> TH.Name -> Code Q SwiftExport -> Q [Dec]
genSwiftActionAndAnn yielding name exportAnn = do
  genName <- newName ("gen_swift_" ++ nameBase name)
  ann <- AnnP (ValueAnnotation genName) <$> unTypeCode exportAnn
  -- The generated function which outputs Swift code is going to be run by the plugin
  genFunSig <- TH.SigD genName <$> [t| IO () |]
  genFunBody <- yielding
  let genFun = FunD genName [Clause [] (NormalB genFunBody) []]
  return [genFun, genFunSig, PragmaD ann]

--------------------------------------------------------------------------------
-- * Functions
--------------------------------------------------------------------------------

-- | Yield a Haskell function as Swift code wrapping a foreign call.
yieldFunction :: TH.Type -- ^ Original function type
              -> String -- ^ Original function name
              -> String -- ^ Wrapper function name
              -> Q Exp
yieldFunction orig_ty orig_name wrapper_name = do

  outputCode [|
    do
      let moatTy = toMoatType (Proxy @($(pure $ dropResultIO orig_ty)))
          -- moatData = toMoatData (Proxy @($(stripTyIO prx))
          (argTys, retTy) = splitRetMoatTy ([], undefined) moatTy
          splitRetMoatTy (args, r) (Moat.App arg c) = splitRetMoatTy (arg:args, r) c
          splitRetMoatTy (args, _) ret = (reverse args, ret)

      swiftParams <- liftIO $ zipWithM prettyParam [1..] argTys

      let fcall fargs = pure $
            "var foreign_haskell_call_result = "
                    ++ $(lift wrapper_name)
                    ++ "(" ++ concat (intersperse ", " fargs) ++ ")"
      let (fbody, _, throwsHsExcp) =
            runSwiftCodeGen (fromHaskell (Proxy @($(pure orig_ty))) fcall) "foreign_haskell_call_result" (map (('v':) . show) [(1::Int)..])

      return $ unlines $
        [ -- let's just do it inline rather than -- "@ForeignImportHaskell"
          "public func " ++ $(litE $ StringL orig_name) ++ "(" ++ concat (intersperse ", " swiftParams) ++ ")"
                  ++ (if Monoid.getAny throwsHsExcp then " throws(HaskellException) " else "") ++ " -> " ++ prettyMoatType retTy
        , "{"
        ]
        ++
        [ "  let hs_enc = JSONEncoder()" | not (null swiftParams) ]
        ++
        [ "  let hs_dec = JSONDecoder()" ] -- not always needed; todo track in SwiftCodeGen
        ++
        [ "  do {"
        , indent 4 fbody
        , "  } catch HsFFIError.decodingFailed(let data, let error) {"
        , "    fatalError(\"Error decoding Haskell data \\(data). Failed with \\(error)\")"
        ]
        ++
        [ unlines $
          [ "  } catch HaskellException.exception(let data) {"
          , "    throw HaskellException.exception(data) // rethrows"
          ]
        | Monoid.getAny throwsHsExcp
        ]
        ++
        [ "  } catch {"
        , "    fatalError(\"Unknown error in foreign Haskell marshal: \\(error)\")"
        , "  }"
        ]
        ++
        [ "}" ]
    |]

--------------------------------------------------------------------------------
-- * Utils
--------------------------------------------------------------------------------

-- | Yield an expression which will yield a piece of code to the current module being generated.
outputCode :: Q Exp -> Q Exp
outputCode produceCode = do
  loc <- (swiftSourcesDir </>) . locToFile . loc_module <$> location

  -- When this splice runs (rather than when the produced expression is run)
  -- reset the files written by it
  runIO $
    removeFile loc `catch ` \case e | isDoesNotExistError e -> pure ()
                                    | otherwise -> throwIO e
  [|
    do
      -- When mod is finalizer, write all swift code to file from scratch
      createDirectoryIfMissing True (takeDirectory loc)
      putStrLn $ "Writing Swift code to: " ++ loc
      code <- $produceCode
      appendFile loc (code ++ "\n")
   |]

-- | Where to write lib
buildDir :: FilePath
buildDir = "_build"

sourcesDir :: FilePath
sourcesDir = buildDir </> "Sources"

swiftSourcesDir :: FilePath
swiftSourcesDir = sourcesDir </> "Swift"

haskellSourcesDir :: FilePath
haskellSourcesDir = sourcesDir </> "ForeignHaskell"

-- | Convert a module's 'Location' into the relative path to the Swift file we're writing
locToFile :: String -> FilePath
locToFile l = map (\case '.' -> '/'; x -> x) l <.> "swift"

generateSwiftCode :: ToMoatData a => Proxy a -> String
generateSwiftCode = prettySwiftData . toMoatData

prettyParam :: Int -> MoatType -> IO (String {- ppr with type -})
prettyParam ix fieldType = do
  internalFieldName <- newName ("v" ++ show ix)
  let fName = "_ " ++ nameBase internalFieldName
  return (fName ++ ": " ++ prettyMoatType fieldType ++ (if isOptional fieldType then " = nil" else ""))

isOptional :: MoatType -> Bool
isOptional (Optional _) = True
isOptional _ = False

unsafeMutableRawPointerType :: MoatType
unsafeMutableRawPointerType = Concrete { concreteName = "UnsafeMutableRawPointer", concreteTyVars = [] }

-- | Drop `IO` from the result type.
-- We need this because ToMoatType doesn't have an IO instance and we only care
-- that on the swift side we get the right result type (without IO)
dropResultIO :: TH.Type -> TH.Type
dropResultIO (AppT (AppT ArrowT a) b) = AppT (AppT ArrowT a) (dropResultIO b)
dropResultIO (AppT (ConT n) t)
  | n == ''IO = t
  | otherwise = AppT (ConT n) t
dropResultIO t = t

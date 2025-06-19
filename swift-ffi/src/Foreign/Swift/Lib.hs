{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
-- | Produce a swift library by building a Haskell package using
-- TemplateHaskell and Cabal SetupHooks
module Foreign.Swift.Lib
  ( -- * Datatypes
    swiftData
  , yieldType
    -- * Functions
  , yieldFunction
  , ty
    -- ** Re-exports
  , Proxy(..)
  , Aeson.deriveJSON, Aeson.defaultOptions
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Kind as K
import Language.Haskell.TH
import Moat
import Data.Proxy (Proxy(..))
import Language.Haskell.TH.Syntax

import System.FilePath
import System.Directory
import Control.Exception
import System.IO.Error

import Foreign.Swift hiding (defaultOptions, deriveJSON)
import Data.List (intersperse)
import Control.Monad

--------------------------------------------------------------------------------
-- * Types / Data
--------------------------------------------------------------------------------

-- | Yield a datatype declaration for the given datatype name
--
-- Example of top level splice: @$(swiftData ''User)@
swiftData :: Name -> Q [Dec]
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

  return (mg ++ toJSON ++ fromJSON)

-- | Output a Swift datatype declaration to a file using the current module
-- name as the filename.
yieldType :: forall (a :: K.Type). ToMoatData a => Proxy a -> Q [Dec]
yieldType prx = do
  outputCode $ generateSwiftCode prx ++ "\n"

--------------------------------------------------------------------------------
-- * Functions
--------------------------------------------------------------------------------

-- | Yield a Haskell function as Swift code wrapping a foreign call.
-- It also exports the function to the foreign interface
--
-- @
-- yieldFunction @(Query -> Int -> String) 'f [Just "x", Nothing] Proxy
-- @
yieldFunction :: forall (a :: K.Type). ToMoatType a
              => Proxy a
              -> Name
              -> [Maybe String]
              -> Q [Dec]
yieldFunction prx name inArgNames = do
  -- Function must be exported
  foreignExp <- foreignExportSwift name
  argNames <- mapM (\case Nothing -> pure Nothing; Just n -> Just <$> newName n) inArgNames

  let moatTy = toMoatType prx
      (argTys, retTy) = splitRetMoatTy ([], undefined) moatTy

  when (length argNames /= length argTys) $
    fail $ "BAD LENGTH: " ++ show argNames ++ " vs " ++ show argTys
  swiftParams <- sequence $ zipWith3 prettyParam argNames [1..] argTys

  ds <- outputCode $ unlines
    [ "@ForeignImportHaskell"
    , "func " ++ nameBase name ++ "(cconv: HsCallJSON" ++ (if null swiftParams then "" else ", ") ++ concat (intersperse ", " swiftParams) ++ ")"
              ++ " -> " ++ prettyMoatType retTy
    , "{ stub() }"
    ]

  return (foreignExp ++ ds)
  where
    splitRetMoatTy (args, r) (App arg c) = splitRetMoatTy (arg:args, r) c
    splitRetMoatTy (args, _) ret = (reverse args, ret)

ty :: Name -> Q Type
ty n = reifyType n

-- Nevermind! VarI always returns a "Nothing" for the RHS
-- collectFunVarNames :: Name -> Q [Maybe Name]
-- collectFunVarNames _ = do
--   reify name >>= \case
--     VarI _ _ (Just (FunD _ (Clause pats _ _:_))) {- look at first clause only -}
--       -> mapM toName pats
--     _ -> pure []
--   where
--     toName :: Pat -> Q (Maybe Name)
--     toName = \case
--       VarP n    -> pure $ Just n
--       ParensP p -> toName p
--       TildeP  p -> toName p
--       BangP   p -> toName p
--       AsP   n _ -> pure $ Just n
--       SigP  p _ -> toName p
--       ViewP _ p -> toName p
--       _         -> pure Nothing


--------------------------------------------------------------------------------
-- * Utils
--------------------------------------------------------------------------------

-- | Yield a piece of code to the current module being generated.
-- Note that code is only yielded after typechecking (as a finalizer)
outputCode :: String -> Q [Dec]
outputCode code = do
  loc <- (buildDir </>) . locToFile <$> location

  -- When this splice runs (rather than when module is finalized)
  -- Reset the files written by it
  runIO $ removeFile loc `catch ` \case e | isDoesNotExistError e -> pure ()
                                          | otherwise -> throwIO e

  -- When mod is finalizer, write all swift code to file from scratch
  addModFinalizer $ do
    runIO $ do
      createDirectoryIfMissing True (takeDirectory loc)
      putStrLn $ "Writing Swift code to: " ++ loc
      appendFile loc (code ++ "\n")
    return ()
  return []


-- | Where to write lib
buildDir :: FilePath
buildDir = "_build"

-- | Convert a module's 'Location' into the relative path to the Swift file we're writing
locToFile :: Loc -> FilePath
locToFile l = map (\case '.' -> '/'; x -> x) (loc_module l) <.> "swift"

generateSwiftCode :: ToMoatData a => Proxy a -> String
generateSwiftCode = prettySwiftData . toMoatData

prettyParam :: Maybe Name -> Int -> MoatType -> Q String
prettyParam mname ix fieldType = do
  fieldName <- case mname of
    Nothing -> do
      internalFieldName <- newName ("v" ++ show ix)
      return $ "_ " ++ nameBase internalFieldName
    Just fieldName -> pure $ nameBase fieldName
  return $ fieldName ++ ": " ++ prettyMoatType fieldType ++ (if isOptional fieldType then " = nil" else "")

isOptional :: MoatType -> Bool
isOptional (Optional _) = True
isOptional _ = False


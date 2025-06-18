{-# LANGUAGE DataKinds #-}
-- | Produce a swift library by building a Haskell package using
-- TemplateHaskell and Cabal SetupHooks
module Foreign.Swift.Lib
  ( swiftType
  , yieldType
    -- ** Re-exports
  , Proxy(..)
  , Aeson.deriveJSON, Aeson.defaultOptions
  ) where

import qualified Data.Aeson.TH as Aeson
import qualified Data.Kind as K
import Language.Haskell.TH
import Moat
import Data.Proxy (Proxy(..))
import Language.Haskell.TH.Syntax

-- import Foreign.Swift hiding (defaultOptions, deriveJSON)

-- | Yield a datatype declaration for the given datatype name
--
-- Example of top level splice: @$(swiftData ''User)@
swiftType :: Name -> Q [Dec]
swiftType name = do
  -- generate Moat class instances
  mobileGenWith defaultOptions { dataProtocols = [Codable] } name

-- | Output a Swift datatype declaration to a file using the current module
-- name as the filename.
yieldType :: forall (a :: K.Type). ToMoatData a => Proxy a -> Q [Dec]
yieldType prx = do
  -- possibly derive json if reify instances shows no instances
  addModFinalizer $ do
    loc <- locToFile <$> location
    runIO $ do
      putStrLn $ "Writing Swift code to: " ++ loc
      appendFile loc $ generateSwiftCode prx ++ "\n\n"
    return ()
  return []

--------------------------------------------------------------------------------

-- | Convert a module's 'Location' into the relative path to the Swift file we're writing
locToFile :: Loc -> FilePath
locToFile l = loc_module l ++ ".swift"

generateSwiftCode :: ToMoatData a => Proxy a -> String
generateSwiftCode = prettySwiftData . toMoatData


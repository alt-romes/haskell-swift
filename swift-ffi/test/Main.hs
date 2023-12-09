{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}
module Main (main) where

import GHC.Generics
import Foreign.StablePtr
import Foreign.Swift

data User = User
  { name :: String
  , age  :: Int
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

birthday :: User -> User
birthday u = u{age = age u + 1}
$(foreignExportSwift 'birthday)

f1 :: User -> IO User
f1 = undefined
$(foreignExportSwift 'f1)

f2 :: User -> IO (StablePtr User)
f2 = undefined
$(foreignExportSwift 'f2)

f3 :: StablePtr User -> User
f3 = undefined
$(foreignExportSwift 'f3)

f4 :: StablePtr User -> IO User
f4 = undefined
$(foreignExportSwift 'f4)

f5 :: StablePtr User -> IO (StablePtr User)
f5 = undefined
$(foreignExportSwift 'f5)

f6 :: User -> StablePtr User -> User -> IO (StablePtr User)
f6 = undefined
$(foreignExportSwift 'f6)

f7 :: User -> StablePtr User -> User -> IO User
f7 = undefined
$(foreignExportSwift 'f7)

main :: IO ()
main = putStrLn "Test suite not yet implemented."

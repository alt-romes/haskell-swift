{-# LANGUAGE UndecidableInstances, DerivingVia, TemplateHaskell, DerivingStrategies, DeriveAnyClass, GHC2024 #-}
module Main (main) where

import Foreign.Swift
import Foreign.Swift.Marshal
import Foreign.Swift.Lib

data User = User
  { name :: String
  , age  :: Int
  }

newtype UserPtr = UserPtr User

swiftData ''User
swiftPtr ''UserPtr
swiftMarshal JSONKind ''User
swiftMarshal PtrKind ''UserPtr

birthday :: User -> User
birthday u = u{age = age u + 1}
$(foreignExportSwift 'birthday)

f1 :: User -> IO User
f1 = undefined
$(foreignExportSwift 'f1)

f2 :: User -> IO UserPtr
f2 = undefined
$(foreignExportSwift 'f2)

f3 :: UserPtr -> User
f3 = undefined
$(foreignExportSwift 'f3)

f4 :: User -> IO User
f4 = undefined
$(foreignExportSwift 'f4)

f5 :: UserPtr -> IO UserPtr
f5 = undefined
$(foreignExportSwift 'f5)

f6 :: User -> UserPtr -> User -> IO UserPtr
f6 = undefined
$(foreignExportSwift 'f6)

f7 :: User -> UserPtr -> User -> IO User
f7 = undefined
$(foreignExportSwift 'f7)

main :: IO ()
main = putStrLn "Test suite compiles!"

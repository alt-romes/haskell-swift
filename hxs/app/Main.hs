{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
module Main where
import Options.Generic
import System.Directory.OsPath (getXdgDirectory, createDirectoryIfMissing, XdgDirectory(..))
import qualified System.Directory.OsPath as OSDir
import System.OsPath (osp, OsPath)
import qualified System.OsPath as FP
import Data.Aeson

data Command w
  = Configure
    { xcodegen ::w::: OsPath <?> "Path to xcodegen executable"
    , cabal    ::w::: OsPath <?> "Path to the cabal executable"
    }

  | Init

  | Build

data Configuration
  = Configuration
    { xcodegen :: OsPath
    , cabal :: OsPath
    }

main :: IO ()
main = do
  command <- unwrapRecord "Haskell x Swift project tool"
  case command of
    Configure{..}
          -> configure Configuration{..}
    Init  -> initialize
    Build -> build
  putStrLn "Hello, Haskell!"

configure :: Configuration -> IO ()
configure config = do
  configPath <- getXdgDirectory XdgConfig [osp|hxs|]
  createDirectoryIfMissing True configPath

initialize :: IO ()
initialize = putStrLn "Init is undefined"

build :: IO ()
build = putStrLn "Build is undefined"

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- Command
deriving stock    instance Generic (Command w)
deriving anyclass instance ParseRecord (Command Wrapped)
-- Configuration
deriving stock    instance Generic Configuration
deriving anyclass instance ToJSON Configuration
deriving anyclass instance FromJSON Configuration

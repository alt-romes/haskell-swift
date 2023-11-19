module Build where

import Control.Exception
import Data.Aeson
import System.Directory.OsPath
import System.OsPath (osp, OsPath)
import qualified Data.ByteString.Lazy as BSL
import qualified System.OsPath as FP
import Data.Maybe

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Configure

--------------------------------------------------------------------------------
-- Project generation
--------------------------------------------------------------------------------

initialize :: Configuration -> IO ()
initialize Configuration{..} = shakeArgs shakeOptions{shakeFiles="_build", shakeThreads=jobs} do
  return ()

--------------------------------------------------------------------------------
-- Building
--------------------------------------------------------------------------------

build :: Configuration -> IO ()
build Configuration{..} = do
  return ()


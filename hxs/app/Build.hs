module Build where

import Control.Exception
import Data.Aeson
import System.Directory.OsPath
import System.OsPath (osp, OsPath)
import qualified Data.ByteString.Lazy as BSL
import qualified System.OsPath as FP
import Data.Maybe

import Configure

--------------------------------------------------------------------------------
-- Build
--------------------------------------------------------------------------------

build :: Configuration -> IO ()
build Configuration{..} = do
  return ()


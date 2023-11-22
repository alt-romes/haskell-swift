module Common where

import System.FilePath
import Development.Shake

--------------------------------------------------------------------------------
-- Shake
--------------------------------------------------------------------------------

shake' :: Rules () -> IO ()
shake' = shake shakeOptions{shakeFiles=shakeBuildDir, shakeVerbosity=Verbose}

shakeBuildDir :: FilePath
shakeBuildDir = "build"

--------------------------------------------------------------------------------
-- Haskell
--------------------------------------------------------------------------------

foreignIncludeDir :: FilePath
foreignIncludeDir = shakeBuildDir </> "include"

--------------------------------------------------------------------------------
-- .xcconfig
--------------------------------------------------------------------------------

xcConfigsDir, defaultDebugXCConfigFile, defaultReleaseXCConfigFile, dynamicXCConfigFile :: FilePath
xcConfigsDir = "configs"
defaultDebugXCConfigFile = xcConfigsDir </> "Debug.xcconfig"
defaultReleaseXCConfigFile = xcConfigsDir </> "Release.xcconfig"
dynamicXCConfigFile = shakeBuildDir </> xcConfigsDir </> "Dynamic.xcconfig"


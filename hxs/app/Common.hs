module Common where

import System.FilePath
import Development.Shake
import Data.String.Interpolate

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

-- | Returns the path to the cabal-built Haskell shared library, relative to the project dir
cabalForeignLibPath :: FilePath -> String -> Action FilePath
cabalForeignLibPath projDir projName = do
    Stdout flib_path <- cmd "cabal list-bin" [[i|--project-dir=#{projDir}|], [i|#{projName}-foreign|] :: String]
    return $ makeRelative projDir flib_path

--------------------------------------------------------------------------------
-- .xcconfig
--------------------------------------------------------------------------------

xcConfigsDir, defaultDebugXCConfigFile, defaultReleaseXCConfigFile, dynamicXCConfigFile :: FilePath
xcConfigsDir = "configs"
defaultDebugXCConfigFile = xcConfigsDir </> "Debug.xcconfig"
defaultReleaseXCConfigFile = xcConfigsDir </> "Release.xcconfig"
dynamicXCConfigFile = shakeBuildDir </> xcConfigsDir </> "Dynamic.xcconfig"


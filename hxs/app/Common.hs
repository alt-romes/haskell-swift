module Common where

import System.Process
import Data.Function
import Control.Monad.IO.Class
import System.FilePath
import Development.Shake
import Data.String.Interpolate

--------------------------------------------------------------------------------
-- Shake
--------------------------------------------------------------------------------

shake' :: FilePath -> Rules () -> IO ()
shake' projDir = shake shakeOptions{shakeFiles=projDir </> shakeBuildDir, shakeVerbosity=Verbose}

shakeBuildDir :: FilePath
shakeBuildDir = "build"

--------------------------------------------------------------------------------
-- Haskell
--------------------------------------------------------------------------------

foreignIncludeDir :: FilePath
foreignIncludeDir = shakeBuildDir </> "include"

foreignIncludeStubsDir :: FilePath
foreignIncludeStubsDir = foreignIncludeDir <> "-stubs"

-- | Returns the path to the cabal-built Haskell shared library, relative to the project dir
cabalForeignLibPath :: MonadIO m => FilePath -> String -> m FilePath
cabalForeignLibPath projDir projName = do
    out <- readProcess "cabal" ["list-bin", [i|--project-dir=#{projDir}|], [i|#{projName}-foreign|]] [] & liftIO
    case lines out of
      [flib_path]
        -> return $ makeRelative projDir flib_path
      _ -> error $ unlines
            [ "cabalForeignLibPath: Expecting exactly one path to the foreign library."
            , "Instead, got:"
            , out
            ]

--------------------------------------------------------------------------------
-- .xcconfig
--------------------------------------------------------------------------------

xcConfigsDir, defaultDebugXCConfigFile, defaultReleaseXCConfigFile, dynamicXCConfigFile :: FilePath
xcConfigsDir = "configs"
defaultDebugXCConfigFile = xcConfigsDir </> "Debug.xcconfig"
defaultReleaseXCConfigFile = xcConfigsDir </> "Release.xcconfig"
dynamicXCConfigFile = shakeBuildDir </> xcConfigsDir </> "Dynamic.xcconfig"

-- Cabal should write dist-newstyle to build

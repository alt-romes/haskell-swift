module Common where

import System.FilePath
import Development.Shake

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

shake' :: Rules () -> IO ()
shake' = shake shakeOptions{shakeFiles=shakeBuildDir, shakeVerbosity=Verbose}

shakeBuildDir :: FilePath
shakeBuildDir = "build"

foreignIncludeDir :: FilePath
foreignIncludeDir = shakeBuildDir </> "include"


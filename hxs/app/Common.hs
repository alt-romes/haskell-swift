module Common where

import Development.Shake

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

shake' :: Rules () -> IO ()
shake' = shake shakeOptions{shakeFiles=shakeBuildDir, shakeVerbosity=Verbose}

shakeBuildDir :: FilePath
shakeBuildDir = "build"


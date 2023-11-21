module Common where

import Development.Shake

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

shake' :: Rules () -> IO ()
shake' = shake shakeOptions{shakeFiles=buildDir, shakeVerbosity=Verbose}

buildDir :: FilePath
buildDir = "_build"


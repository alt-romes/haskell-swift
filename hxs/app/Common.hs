module Common where

import Development.Shake
import Development.Shake.FilePath

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

shake' :: Rules () -> IO ()
shake' = shake shakeOptions{shakeFiles=buildDir}

buildDir :: FilePath
buildDir = "_build"

xcConfigsDir, defaultStaticXCConfig :: FilePath
xcConfigsDir = "BuildSettings"
defaultStaticXCConfig = xcConfigsDir </> "Statixcconfig"


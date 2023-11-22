module Build where

import Development.Shake
import Development.Shake.FilePath
import Data.String.Interpolate
import Common

--------------------------------------------------------------------------------
-- Build
--------------------------------------------------------------------------------

build :: FilePath -> String -> IO ()
build projDir projName = shake' do

  want $ map (projDir </>)
       [ projName <.> "xcodeproj" </> "project.pbxproj"
       , dynamicXCConfigFile
       ]

  projDir </> dynamicXCConfigFile %> \out -> do
    Stdout (words -> [_ffi_headers, rts_headers]) <- cmd "ghc-pkg field rts include-dirs --simple-output"
    Stdout flib_path <- cmd "cabal list-bin" [[i|--project-dir=#{projDir}|], [i|#{projName}-foreign|] :: String]
    writeFile' out [__i'E|
        HEADER_SEARCH_PATHS=$(inherit) #{rts_headers}
        LIBRARY_SEARCH_PATHS=$(inherit) #{takeDirectory flib_path}
    |]

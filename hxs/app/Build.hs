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
       [ dynamicXCConfigFile
       ]

  projDir </> dynamicXCConfigFile %> \out -> do
    -- ghc-pkg will return an absolute path, but this is not a problem for relocatable (i.e. not always running on the same directory, same machine)
    -- since this is done at build time into dirs that are not checked in, not at init time.
    Stdout (words -> [_ffi_headers, rts_headers]) <- cmd "ghc-pkg field rts include-dirs --simple-output"
    flib_path <- cabalForeignLibPath projDir projName
    -- ROMES:TODO: Refactor to "Cabal" module both these functions?
    writeFile' out [__i'E|
        HEADER_SEARCH_PATHS=$(inherit) #{rts_headers}
        LIBRARY_SEARCH_PATHS=$(inherit) #{takeDirectory flib_path}
    |]

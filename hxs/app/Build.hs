{-# LANGUAGE BlockArguments #-}
module Build where

import Development.Shake
import Development.Shake.FilePath
import Data.String.Interpolate
import Common

--------------------------------------------------------------------------------
-- Build
--------------------------------------------------------------------------------

build :: FilePath -> String -> IO ()
build projDir projName = shake' projDir do

  flib <- cabalForeignLibPath projDir projName

  want [ projDir </> dynamicXCConfigFile
       , flib
       ]

  projDir </> dynamicXCConfigFile %> \out -> do
    -- ghc-pkg will return an absolute path, but this is not a problem for
    -- relocatable builds (i.e. not always running on the same directory, same
    -- machine) since this is done at build time into dirs that are not checked
    -- into source control, not at init time.
    Stdout (words -> [_ffi_headers, rts_headers]) <- cmd "ghc-pkg field rts include-dirs --simple-output"
    writeFile' out [__i'E|
        HEADER_SEARCH_PATHS=$(inherit) #{rts_headers}
        LIBRARY_SEARCH_PATHS=$(inherit) #{takeDirectory flib}
    |]

  -- The shared library doubles as a stamp for everything else cabal builds e.g. the headers
  flib %> \_out -> do
    cmd_ ([i|cabal build --project-dir=#{projDir} #{projName}-foreign|] :: String)

    -- Then copy all include-stubs//*_stubs.h to include//*.h
    stubs <- getDirectoryFiles (projDir </> foreignIncludeStubsDir) ["//*_stub.h"]
    _ <- forP stubs \stub -> do -- in parallel!
      let header_name = reverse $ drop (length "_stub.h") $ reverse (takeFileName stub)
      copyFileChanged (projDir </> foreignIncludeStubsDir </> stub) (projDir </> foreignIncludeDir </> header_name <.> "h")

    return ()


{-# LANGUAGE BlockArguments, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Build where

import GHC.Generics
import Data.Typeable
import Development.Shake.Classes
import Development.Shake
import Development.Shake.FilePath
import Data.String.Interpolate
import Common

data CabalFLibPath = CabalFLibPath deriving (Show,Typeable,Generic,Eq,Hashable,Binary,NFData)
type instance RuleResult CabalFLibPath = String

--------------------------------------------------------------------------------
-- Build
--------------------------------------------------------------------------------

build :: FilePath -> String -> IO ()
build projDir projName = shake' projDir do

  -- TODO: CreateDirectory if not exists for include and include-stubs

  getFLibPath <- addOracle $ \CabalFLibPath -> do
    cabalForeignLibPath projDir projName

  want [ projDir </> dynamicXCConfigFile
       , "foreign-shared-lib"
       ] 

  projDir </> dynamicXCConfigFile %> \out -> do
    flib <- getFLibPath CabalFLibPath
    -- ghc-pkg will return an absolute path, but this is not a problem for
    -- relocatable builds (i.e. not always running on the same directory, same
    -- machine) since this is done at build time into dirs that are not checked
    -- into source control, not at init time.
    -- ROMES:TODO: ghc-pkg invocation should also be an oracle
    Stdout (words -> [_ffi_headers, rts_headers]) <- cmd "ghc-pkg field rts include-dirs --simple-output"
    writeFile' out [__i'E|
        HEADER_SEARCH_PATHS=$(inherit) #{rts_headers}
        LIBRARY_SEARCH_PATHS=$(inherit) #{takeDirectory flib}
    |]

  -- The shared library doubles as a stamp for everything else cabal builds e.g. the headers
  "foreign-shared-lib" ~> do
    _flib <- getFLibPath CabalFLibPath
    cmd_ ([i|cabal build --project-dir=#{projDir} #{projName}-foreign|] :: String)

    -- Then copy all include-stubs//*_stubs.h to include//*.h
    stubs <- getDirectoryFiles (projDir </> foreignIncludeStubsDir) ["//*_stub.h"]
    _ <- forP stubs \stub -> do -- in parallel!
      let header_name = reverse $ drop (length "_stub.h") $ reverse (takeFileName stub)
      copyFileChanged (projDir </> foreignIncludeStubsDir </> stub) (projDir </> foreignIncludeDir </> header_name <.> "h")

    return ()


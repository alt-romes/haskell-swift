module Init where

import Development.Shake
import qualified System.Directory

import Common
import Development.Shake.FilePath
import Data.Function
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.List as S
import Control.Exception
import Control.Monad

--------------------------------------------------------------------------------
-- Initialize
--------------------------------------------------------------------------------

initialize :: FilePath -> String -> IO ()
initialize projDir projName = shake' do

  want $ map (projDir </>)
       [ projName <.> "xcodeproj" </> "project.pbxproj"

       , "module.modulemap"

       , defaultDebugXCConfigFile
       , defaultReleaseXCConfigFile

       , projName <.> "cabal"
       , "flib" </> "MyForeignLib.hs"
       , "cbits" </> "MyForeignLibRts.h"
       , "cbits" </> "MyForeignLibRts.c"

       , ".gitignore"
       ]

  -- xcode
  "//*.xcodeproj" </> "project.pbxproj" %> createOnly \out -> do
    throwIO (NoInitXCodeProj out) & liftIO

  -- xcode: Patch XML
  projDir </> projName <.> "xcodeproj" </> ".hxs-stamp" %> createOnly \out -> do
    let xcprojPath = takeDirectory out </> "project.pbxproj"
    xcproj <- readFile' xcprojPath
    _

  -- module.modulemap
  projDir </> "module.modulemap" %> createOnly \out -> do
    writeFile' out (defaultModuleMap projName)

  -- .xcconfig
  projDir </> xcConfigsDir </> "*.xcconfig" %> createOnly \out -> do
    putInfo [i|Writing #{out} with the default static .xcconfig settings|]
    T.writeFile out staticXCConfig & liftIO
    trackWrite [out]

  -- Cabal
  projDir </> projName <.> "cabal" %> createOnly \out -> do
    putInfo "Creating a cabal project"
    -- See Note [cabal init <projDir>]
    cmd_ "cabal init" [projDir] (cabalInitOpts projName)
    putInfo "Writing foreign-library stanza"
    appendFile out (foreignLibStanza projName) & liftIO
    trackWrite [out]

  projDir </> "flib" </> "MyForeignLib.hs" %> createOnly \out -> do
    writeFile' out stubMyForeignLib

  projDir </> "cbits" </> "MyForeignLibRts.c" %> createOnly \out -> do
    writeFile' out stubMyForeignLibRtsC

  projDir </> "cbits" </> "MyForeignLibRts.h" %> createOnly \out -> do
    writeFile' out stubMyForeignLibRtsH

  -- .gitignore
  "//.gitignore" %> createOnly \out ->
    writeFile' out [__i'E|
        #{shakeBuildDir}
        dist-newstyle
    |]

-- | In this Init module we never want to re-run the rules if things go out of date, we just want to generate them if they don't exist it all.
-- For example, if we generate a cabal file, and someone manually adds a dependency to it, we do not want to re-run the rules to generate a cabal file.
--
-- Prefix an action closure following a pattern with this function to only run the action when the file it constructs does not exist
createOnly :: (FilePath -> Action ()) -> (FilePath -> Action ())
createOnly act out = do
  -- We don't want these files's deletion to be tracked by the build system.
  -- In general, it's as simple as if it does not exist when init is run, then we create
  b <- System.Directory.doesFileExist out & liftIO
  unless b $
    act out

--------------------------------------------------------------------------------
-- Static .xcconfig config
--------------------------------------------------------------------------------

staticXCConfig :: Text
-- We don't do OTHER_LDFLAGS=-lhaskell-foreign-framework because we expect the `link` directive in the module map to have the same effect.
staticXCConfig = [__i|
    SWIFT_INCLUDE_PATHS=$(PROJECT_DIR)
    \#include "../#{dynamicXCConfigFile}"
|]

--------------------------------------------------------------------------------
-- Default .cabal file
--------------------------------------------------------------------------------

{-
Note [cabal init <projDir>]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Despite the documentation specifying --package-dir=#{projDir} as the flag to
determine where the cabal project is generated, it turns out that cabal will
use the first extra-argument (after `init`) as the directory for the cabal
project. See Cabal #9157.
-}


cabalInitOpts :: String -- ^ Project name
              -> [String]
cabalInitOpts projName =
  [ "--non-interactive"
  , "--language=GHC2021"
  , "--no-comments"
  , "--lib"
  , [i|--package-name=#{projName}|]
  , [i|--dependency=#{S.intercalate "," cabalDefaultDeps}|]
  , unwords . map (\x -> [i|--extension=#{x}|]) $
        cabalDefaultExtensions
  ]

cabalDefaultDeps :: [String]
cabalDefaultDeps = [ "base", "text", "bytestring", "aeson" ]

cabalDefaultExtensions :: [String]
cabalDefaultExtensions = [ "ForeignFunctionInterface" ]

foreignLibStanza :: String -> String
foreignLibStanza projName = [i|
foreign-library #{projName}-foreign
    type: native-shared

    -- This should work on Mac, despite being undefined behaviour
    -- See https://www.hobson.space/posts/haskell-foreign-library/
    options: standalone

    -- We copy the C stub headers to a folder in the root.
    -- If you have foreign-export declarations in the library
    -- be sure to add this flag there too (so all stubs get added
    -- to #{foreignIncludeDir}).
    ghc-options: -stubdir=#{foreignIncludeDir}

    other-modules: MyForeignLib
    hs-source-dirs: flib
    build-depends: #{projName}, #{S.intercalate ", " cabalDefaultDeps}
    other-extensions: #{S.intercalate ", " cabalDefaultExtensions}

    include-dirs: cbits
    c-sources: cbits/MyForeignLibRts.c
    install-includes: MyForeignLibRts.h
|]

stubMyForeignLib :: String
stubMyForeignLib = [__i'E|
    module MyForeignLib where
    import Foreign.C

    foreign export ccall hs_factorial :: CInt -> CInt

    hs_factorial :: CInt -> CInt
    hs_factorial x = product [1..x]
|]

stubMyForeignLibRtsH :: String
stubMyForeignLibRtsH = [__i'E|
  \#include <HsFFI.h>

  HsBool flib_init();
  void flib_end();
|]


stubMyForeignLibRtsC :: String
stubMyForeignLibRtsC = [__i'E|
    \#include <stdlib.h>
    \#include <stdio.h>
    \#include <HsFFI.h>

    HsBool flib_init() {

        printf("Initialising flib\n");

        // Initialise Haskell runtime
        hs_init(NULL, NULL);

        // Do other library initialisations here

        return HS_BOOL_TRUE;
    }

    void flib_end() {
        printf("Terminating flib\n");
        hs_exit();
    }
|]

--------------------------------------------------------------------------------
-- module.modulemap
--------------------------------------------------------------------------------
defaultModuleMap :: String -> String
defaultModuleMap projName = [__i'E|
        module #{projName} {
            umbrella "#{foreignIncludeDir}"

            explicit module * {
                export *
            }

            explicit module RTSManage {
                header "cbits/MyForeignLibRts.h"
            }

            link "#{projName}-foreign"
        }
    |]
--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

newtype InitExceptions
    = NoInitXCodeProj FilePath

instance Exception InitExceptions
instance Show InitExceptions where
    show (NoInitXCodeProj out)
        = [__i'E|
        Initializing XCode projects (.xcodeproj) programatically is not supported (by Apple, in general).
        Please create an XCode project manually, where the name matches the project root dir name
        (i.e. the project file must be #{out}).
    |]
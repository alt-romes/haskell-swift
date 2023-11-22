module Init where

import Development.Shake

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

       , defaultDebugXCConfigFile
       , defaultReleaseXCConfigFile

       , projName <.> "cabal"
       , "flib" </> "MyForeignLib.hs"

       , ".gitignore"
       ]

  "//*.xcodeproj" </> "project.pbxproj" %> \out -> do
    b <- doesFileExist out
    unless b $ throwIO (NoInitXCodeProj out) & liftIO

--   projDir </> projName <.> "xcodeproj" </> ".hxs-stamp" %> \out -> do
--     -- TODO: Patch XML
--     _

  projDir </> xcConfigsDir </> "*.xcconfig" %> \out -> do
    putInfo [i|Writing #{out} with the default static .xcconfig settings|]
    T.writeFile out staticXCConfig & liftIO
    trackWrite [out]

  projDir </> projName <.> "cabal" %> \out -> do
    putInfo "Creating a cabal project"
    -- See Note [cabal init <projDir>]
    cmd_ "cabal init" [projDir] (cabalInitOpts projName)
    putInfo "Writing foreign-library stanza"
    appendFile out (foreignLibStanza projName) & liftIO

  projDir </> "flib" </> "MyForeignLib.hs" %> \out -> do
    writeFile' out stubMyForeignLib

  "//.gitignore" %> \out ->
    writeFile' out [__i'E|
        #{shakeBuildDir}
        dist-newstyle
    |]

--------------------------------------------------------------------------------
-- Static .xcconfig config
--------------------------------------------------------------------------------

xcConfigsDir, defaultDebugXCConfigFile, defaultReleaseXCConfigFile :: FilePath
xcConfigsDir = "configs"
defaultDebugXCConfigFile = xcConfigsDir </> "Debug.xcconfig"
defaultReleaseXCConfigFile = xcConfigsDir </> "Release.xcconfig"

staticXCConfig :: Text
staticXCConfig = [__i|
    SWIFT_INCLUDE_PATHS=$(PROJECT_DIR)
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
|]

stubMyForeignLib :: String
stubMyForeignLib = [__i'E|
    module MyForeignLib where
    import Foreign.C

    foreign export ccall hs_double :: CInt -> CInt

    hs_double :: CInt -> CInt
    hs_double x = 2 * x
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
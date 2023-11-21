module Init where

import Data.Yaml
import Development.Shake

import Common
import Development.Shake.FilePath
import Data.Function
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.List as S
import Control.Exception

--------------------------------------------------------------------------------
-- Initialize
--------------------------------------------------------------------------------

initialize :: FilePath -> String -> IO ()
initialize projDir projName = shake' do

  want $ map (projDir </>)
       [ projName <.> "xcodeproj"
       , projName <.> "cabal"
       , defaultDebugXCConfigFile
       , defaultReleaseXCConfigFile
       ]

  "//*.xcodeproj" %> \out -> do
    throwIO NoInitXCodeProj & liftIO

  projDir </> xcConfigsDir </> "*.xcconfig" %> \out -> do
    putInfo [i|Writing #{out} with the default static .xcconfig settings|]
    T.writeFile out staticXCConfig & liftIO
    trackWrite [out]

  projDir </> projName <.> "cabal" %> \_out -> do
    putInfo "Creating a cabal project"
    -- See Note [cabal init <projDir>]
    cmd_ "cabal init" [projDir] (cabalInitOpts projName)

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
cabalDefaultDeps = [ "text", "bytestring", "aeson" ]

cabalDefaultExtensions :: [String]
cabalDefaultExtensions = [ "ForeignFunctionInterface" ]

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data InitExceptions
    = NoInitXCodeProj

instance Exception InitExceptions
instance Show InitExceptions where
    show NoInitXCodeProj
        = [__i'E|
        Initializing XCode projects (.xcodeproj) programatically is not supported (by Apple, in general).
        Please create an XCode project manually, where the name matches the project root dir name.
    |]
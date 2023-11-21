module Main where
import Control.Exception
import Data.Aeson
import Options.Generic
import System.Directory
import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import Data.String.Interpolate
import Options.Applicative.Builder (InfoMod, progDesc)

-- We are no longer configuring anything for now
import Configure()

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Common
import Init

infoMods :: InfoMod Command
infoMods = progDesc "hxs: Support tool for building projects using both Haskell and Swift"

data Command
  = Init
  | Build
  | Clean

main :: IO ()
main = do
  cmd' <- getRecordWith infoMods mempty
  projName <- takeBaseName <$> getCurrentDirectory
  case cmd' of
    Init -> do
      putStrLn [i|Initializing #{projName}|]
      initialize projName
    Build -> do
      putStrLn [i|Building #{projName}|]
      build projName
    Clean -> shake' do
      want ["clean"]
      phony "clean" do
        putInfo "Deleting files in _build"
        removeFilesAfter "_build" ["//*"]


--------------------------------------------------------------------------------
-- Build
--------------------------------------------------------------------------------

build :: String -> IO ()
build projName = shake' do

  xcprojRule projName

  -- cmd_ "cabal" "build"
  -- cmd_ "xcodebuild"
  -- return ()

xcprojRule :: String
           -- ^ Project name
           -> Rules ()
xcprojRule projName = do

  want [ projName <.> "xcodeproj" </> "project.pbxproj" ]

  projName <.> "xcodeproj" </> "project.pbxproj" %> \out -> do
    let spec = "project.yml"
    need [spec]
    cmd_ "xcodegen"

--------------------------------------------------------------------------------
-- Utils and Instances
--------------------------------------------------------------------------------

deriving instance Generic Command
deriving anyclass instance ParseRecord Command

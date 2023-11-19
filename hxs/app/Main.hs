module Main where
import Control.Exception
import Data.Aeson
import Options.Generic
import System.Directory
import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import Data.String.Interpolate

-- We are no longer configuring anything for now
import Configure()

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

buildDir :: FilePath
buildDir = "_build"

main :: IO ()
main = do
  projName <- takeBaseName <$> getCurrentDirectory
  putStrLn $ [i|Working with #{projName}|]
  shakeArgs shakeOptions{shakeFiles=buildDir} do

    phony "clean" do
      putInfo "Cleaning files in _build"
      removeFilesAfter "_build" ["//*"]

    phony "init" do

      need [ projName <.> "xcodeproj" </> "project.pbxproj"
           , projName <.> "cabal"
           ]

    phony "build" do
      need ["init"]
      cmd_ "cabal" "build"
      cmd_ "xcodebuild"
      return ()

    -- When initializing, we add the rules for generating project.yml 
    projectYmlRule

    xcprojRule projName


projectYmlRule :: Rules ()
projectYmlRule = do

  "project.yml" %> \out -> do
    cmd_ "touch" out


xcprojRule :: String
           -- ^ Project name
           -> Rules ()
xcprojRule projName = do

  projName <.> "xcodeproj" </> "project.pbxproj" %> \out -> do
    let spec = "project.yml"
    need [spec]
    cmd_ "xcodegen"


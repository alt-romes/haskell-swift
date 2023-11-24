module Main where
import Options.Generic
import System.Directory
import Data.String.Interpolate
import Options.Applicative.Builder (InfoMod, progDesc)

-- We are no longer configuring anything for now
import Configure()

import Development.Shake
import Development.Shake.FilePath

import Common
import Init  ( initialize )
import Build ( build )

infoMods :: InfoMod Command
infoMods = progDesc "hxs: Support tool for building projects using both Haskell and Swift"

data Command
  = Init  { root :: Maybe FilePath }
  | Build { root :: Maybe FilePath }
  | Clean { root :: Maybe FilePath }

main :: IO ()
main = do
  cmd'       <- getRecordWith infoMods mempty
  projectDir <- maybe getCurrentDirectory pure cmd'.root
  let projName = takeBaseName (dropTrailingPathSeparator projectDir)
  let buildDir = projectDir </> shakeBuildDir
  case cmd' of
    Init{} -> do
      initialize projectDir projName

      -- We also have to build a first time, otherwise opening the project will have a lot of errors
      -- Should we do this for them?
      build projectDir projName

      -- contents <- System.Directory.getDirectoryContents projectDir
      -- if null $ drop 2 contents
      --   then do
      --     putStrLn [i|Creating a new project "#{projName}"...|]
      --     initialize projectDir projName
      --     putStrLn "Done!"
      --   else
      --     putStrLn "The directory in which 'hxs init' is run must have no contents."
    Build{} -> do
      putStrLn [i|Building #{projName}|]
      build projectDir projName
    Clean{} -> shake' projectDir do
      want ["clean"]
      phony "clean" do
        putInfo "Deleting files in _build"
        removeFilesAfter buildDir ["//*"]


--------------------------------------------------------------------------------
-- Utils and Instances
--------------------------------------------------------------------------------

deriving instance Generic Command
deriving anyclass instance ParseRecord Command

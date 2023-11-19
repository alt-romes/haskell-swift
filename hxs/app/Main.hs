module Main where
import Control.Exception
import Data.Aeson
import Options.Generic
import System.Directory.OsPath
import System.OsPath (osp, OsPath)
import qualified Data.ByteString.Lazy as BSL
import qualified System.OsPath as FP
import Data.Maybe
import Options.Applicative.Builder (InfoMod, progDesc, briefDesc)

import Configure
import Build

type XCodeGenPath = Maybe OsPath <?> "Path to the xcodegen executable"
type CabalPath    = Maybe OsPath <?> "Path to the cabal executable"

data Command w
  = Init
    { xcodegen ::w::: XCodeGenPath
    , cabal    ::w::: CabalPath
    }
  | Build
    { xcodegen ::w::: XCodeGenPath
    , cabal    ::w::: CabalPath
    }

infoMods :: InfoMod (Command Wrapped)
infoMods = progDesc "hxs: Support tool for building projects using both Haskell and Swift"

main :: IO ()
main = do
  command <- unwrap <$> getRecordWith infoMods mempty
  user_config <- getConfig
  case command of
    Init{}  -> initialize
    Build{} -> do
      let config = applyToConfig user_config command
      build config
  where
  applyToConfig :: Configuration -> Command Unwrapped -> Configuration
  applyToConfig config cmd =
    config
      { Configure.xcodegen = fromMaybe config.xcodegen cmd.xcodegen
      , Configure.cabal    = fromMaybe config.cabal    cmd.cabal
      }

initialize :: IO ()
initialize = putStrLn "Init is undefined"


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- Command
deriving stock    instance Generic (Command w)
deriving anyclass instance ParseRecord (Command Wrapped)


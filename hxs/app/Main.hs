{-# LANGUAGE DuplicateRecordFields, OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}
{-# OPTIONS_GHC -Wno-orphans    #-} -- JSON paths
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

data Configuration
  = Configuration
    { xcodegen :: OsPath
    , cabal    :: OsPath
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

initialize :: IO ()
initialize = putStrLn "Init is undefined"

--------------------------------------------------------------------------------
-- Build
--------------------------------------------------------------------------------

build :: Configuration -> IO ()
build Configuration{..} = do

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

getConfig :: IO Configuration
getConfig = do
  configDir <- getXdgDirectory XdgConfig [osp|hxs|]
  createDirectoryIfMissing True configDir

  let configPath = configDir FP.</> [osp|config.json|]
  hasConfig <- doesPathExist configPath
  legacyFP  <- FP.decodeUtf configPath

  if hasConfig
    then do
      mconfig <- decode <$> BSL.readFile legacyFP
      case mconfig of
        Nothing     -> throwIO InvalidConfig
        Just config -> return config
    else do
      config <- defaultConfig
      BSL.writeFile legacyFP (encode config)
      return config
  where
    defaultConfig :: IO Configuration
    defaultConfig = do
      xcodegen <- findExecutable [osp|xcodegen|]
                    `orThrow` ProgNotFound "xcodegen" "brew install xcodegen"
      cabal    <- findExecutable [osp|cabal|]
                    `orThrow` ProgNotFound "cabal" "get cabal via GHCup"
      return Configuration{..}

orThrow :: Exception e => IO (Maybe a) -> e -> IO a
orThrow act ex = act >>= \case Nothing -> throwIO ex; Just x -> pure x

applyToConfig :: Configuration -> Command Unwrapped -> Configuration
applyToConfig config cmd =
  config
    { xcodegen = fromMaybe config.xcodegen cmd.xcodegen
    , cabal    = fromMaybe config.cabal    cmd.cabal
    }

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data HXSException
  = ProgNotFound String String
  | InvalidConfig

deriving instance Show HXSException
deriving instance Exception HXSException

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- Command
deriving stock    instance Generic (Command w)
deriving anyclass instance ParseRecord (Command Wrapped)

-- Configuration
deriving stock    instance Generic Configuration
deriving anyclass instance ToJSON Configuration
deriving anyclass instance FromJSON Configuration

-- Path (utility to serialize OsPath)
newtype Path = Path OsPath
instance ToJSON Path where
  toJSON (Path p) =
    case FP.decodeUtf p of
      Left e -> error (show e)
      Right t -> toJSON t
instance FromJSON Path where
  parseJSON = withText "Path" $ \t -> do
    case FP.encodeUtf (show t) of
      Left e -> fail (show e)
      Right p -> return $ Path p
deriving via Path instance ToJSON OsPath
deriving via Path instance FromJSON OsPath

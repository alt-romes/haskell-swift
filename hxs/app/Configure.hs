{-# OPTIONS_GHC -Wno-orphans    #-} -- JSON paths
module Configure where

import Control.Exception
import Data.Aeson
import GHC.Generics
import System.Directory.OsPath
import System.OsPath(osp, OsPath)
import qualified Data.ByteString.Lazy as BSL
import qualified System.OsPath as FP

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

data Configuration
  = Configuration
    { xcodegen :: OsPath
    , cabal    :: OsPath
    , jobs     :: Int
    -- ^ Maximum umber of threads to run in parallel
    }

getConfig :: IO Configuration
getConfig = do
  configDir <- getXdgDirectory XdgConfig [osp|hxs|]
  createDirectoryIfMissing True configDir

  let configPath = configDir FP.</> [osp|config.json|]
  -- hasConfig <- doesPathExist configPath
  let hasConfig = False -- for debugging
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
      let jobs = 1
      return Configuration{..}

orThrow :: Exception e => IO (Maybe a) -> e -> IO a
orThrow act ex = act >>= \case Nothing -> throwIO ex; Just x -> pure x

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

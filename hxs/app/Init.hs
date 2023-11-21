{-# LANGUAGE OverloadedLists #-}
module Init where

import Data.Yaml
import Development.Shake

import Common
import Development.Shake.FilePath
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Key (fromString)
import GHC.Generics
import Data.Function
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Initialize
--------------------------------------------------------------------------------

initialize :: String -> IO ()
initialize projName = shake' do

  want [ "project.yml"
       , projName <.> "cabal"
       , defaultStaticXCConfig
       ]

  projectYmlRule projName

--------------------------------------------------------------------------------
-- project.yml
--------------------------------------------------------------------------------

projectYmlRule :: String -> Rules ()
projectYmlRule projName =
  "project.yml" %> \out -> do
    encodeFile out (defaultProject projName) & liftIO

defaultProject :: String -> Project
defaultProject projName = Project
    { name = projName
    , options = defaultProjectOptions
    , configFiles = defaultConfigFiles
    , fileGroups = [xcConfigsDir]
    , targets = defaultTargets projName
    , packages = defaultPackages
    }

defaultProjectOptions :: ProjectOptions
defaultProjectOptions = ProjectOptions
    { bundleIdPrefix = Nothing
    , createIntermediateGroups = True
    , xcodeVersion = "15.1"
    , deploymentTarget = [(fromString "macOS", "14.0")]
    }

defaultConfigFiles :: ConfigFiles
defaultConfigFiles = ConfigFiles
    { debug = defaultStaticXCConfig
    , release = defaultStaticXCConfig
    }

defaultTargets :: String -> KeyMap Target
defaultTargets projName =
    [ (fromString targetName, defaultMacOSTarget)
    ]
    where targetName = projName <> "MacOS"

defaultMacOSTarget :: Target
defaultMacOSTarget = Target
    { _type = "application"
    , platform = MACOS
    , configFiles = Nothing
    , sources = []
    , scheme = TargetScheme{configVariants=[]}
    }

defaultPackages :: KeyMap SwiftPackage
defaultPackages = []

--------------------------------------------------------------------------------
-- project.yml specification
--------------------------------------------------------------------------------

data Project
    = Project
    { name :: String
    , options :: ProjectOptions
    , configFiles :: ConfigFiles
    , fileGroups :: [String]
    -- ^ A list of paths to add to the root of the project. These aren't files
    -- that will be included in your targets, but that you'd like to include in
    -- the project hierarchy anyway. For example a folder of xcconfig files
    -- that aren't already added by any target sources, or a Readme file.
    , targets :: KeyMap Target
    , packages :: KeyMap SwiftPackage
    }

data ProjectOptions
    = ProjectOptions
    { bundleIdPrefix :: Maybe String -- ^ Bundle id prefix from user configuration if specified
    , createIntermediateGroups :: Bool -- ^ Whether to create Vendor in Vendor/Foo/File
    , xcodeVersion :: String -- ^ e.g. "0910" or "9.1."
    , deploymentTarget:: KeyMap String
    -- ^ As in
    -- @
    -- deploymentTarget:
    --   watchOS: "2.0"
    --   tvOS: "10.0"
    -- @
    }

-- | Specifies @.onfig@ files for each configuration.
--
-- Example:
-- @
-- configFiles:
--   Debug: debug.xcconfig
--   Release: release.xcconfig
-- @
data ConfigFiles
    = ConfigFiles
    { debug :: FilePath
    , release :: FilePath
    }

data Target
    = Target
    { _type :: ProductType
    , platform :: Platform
    , configFiles :: Maybe ConfigFiles
    , sources :: [TargetSource]
    , scheme :: TargetScheme
    }

-- | This will provide default build settings for a certain product type. It
-- can be any of the following:
-- @
-- application, application.on-demand-install-capable,
-- application.messages, application.watchapp, application.watchapp2,
-- application.watchapp2-container, app-extension,
-- app-extension.intents-service, app-extension.messages,
-- app-extension.messages-sticker-pack, bundle, bundle.ocunit-test,
-- bundle.ui-testing, bundle.unit-test, extensionkit-extension, framework,
-- instruments-package, library.dynamic, library.static, framework.static,
-- tool, tv-app-extension, watchkit-extension, watchkit2-extension,
-- xcode-extension, driver-extension, system-extension, xpc-service
-- @
type ProductType = String

data Platform
    = IOS
    | TVOS
    | MACOS
    | WATCHOS
    | VISIONOS

data SwiftPackage

-- | Specifies the source directories for a target. This can either be a single
-- source or a list of sources. Applicable source files, resources, headers,
-- and .lproj files will be parsed appropriately.
data TargetSource
    = TargetSource
    { path :: FilePath
    , includes :: [String]
    , buildPhase :: TargetBuildPhase
    -- ^ A list of global patterns in the same format as excludes representing
    -- the files to include. These rules are relative to path and not the
    -- directory where project.yml resides. If excludes is present and file
    -- conflicts with includes, excludes will override the includes behavior.
    }

data TargetBuildPhase
    = BuildPhaseSources
    | BuildPhaseResources
    | BuildPhaseHeaders
    | BuildPhaseCopyFiles {destination :: CopyToDestination}
    | BuildPhaseNone

data CopyToDestination
    = CopyToAbsolutePath
    | CopyToProductsDirectory
    | CopyToWrapper
    | CopyToExecutables
    | CopyToResources
    | CopyToJavaResources
    | CopyToFrameworks
    | CopyToSharedFrameworks
    | CopyToSharedSupport
    | CopyToPlugins

newtype TargetScheme
    = TargetScheme
    { configVariants :: [String]
    }

instance ToJSON Platform where
    toJSON = \case
      IOS      -> String $ T.pack "iOS"
      TVOS     -> String $ T.pack "tvOS"
      MACOS    -> String $ T.pack "macOS"
      WATCHOS  -> String $ T.pack "watchOS"
      VISIONOS -> String $ T.pack "visionOS"

instance ToJSON TargetBuildPhase where
    toJSON = \case
      BuildPhaseSources   -> String $ T.pack "sources"
      BuildPhaseResources -> String $ T.pack "resources"
      BuildPhaseHeaders   -> String $ T.pack "headers"
      BuildPhaseCopyFiles{destination} -> Object [(fromString "copyFiles", toJSON destination)]
      BuildPhaseNone      -> String $ T.pack "none"

instance ToJSON CopyToDestination where
    toJSON = \case
      CopyToAbsolutePath      -> String $ T.pack "absolutePath"
      CopyToProductsDirectory -> String $ T.pack "productsDirectory"
      CopyToWrapper           -> String $ T.pack "wrapper"
      CopyToExecutables       -> String $ T.pack "executables"
      CopyToResources         -> String $ T.pack "resources"
      CopyToJavaResources     -> String $ T.pack "javaResources"
      CopyToFrameworks        -> String $ T.pack "frameworks"
      CopyToSharedFrameworks  -> String $ T.pack "sharedFrameworks"
      CopyToSharedSupport     -> String $ T.pack "sharedSupport"
      CopyToPlugins           -> String $ T.pack "plugins"


deriving stock    instance Generic Project
deriving anyclass instance ToJSON  Project
deriving stock    instance Generic ProjectOptions
deriving anyclass instance ToJSON  ProjectOptions
deriving stock    instance Generic ConfigFiles
deriving anyclass instance ToJSON  ConfigFiles
deriving stock    instance Generic Target
deriving anyclass instance ToJSON  Target
deriving stock    instance Generic SwiftPackage
deriving anyclass instance ToJSON  SwiftPackage
deriving stock    instance Generic TargetSource
deriving anyclass instance ToJSON  TargetSource
deriving stock    instance Generic TargetScheme
deriving anyclass instance ToJSON  TargetScheme

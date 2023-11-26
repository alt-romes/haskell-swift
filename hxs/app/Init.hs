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
import System.Exit (ExitCode(..))

--------------------------------------------------------------------------------
-- Initialize
--------------------------------------------------------------------------------

initialize :: FilePath -> String -> IO ()
initialize projDir projName = shake' projDir do

  want $ map (projDir </>)
       [ projName <.> "xcodeproj" </> "project.pbxproj"
       , projName <.> "xcodeproj" </> ".hxs-stamp.rb"
       , projName <.> "xcodeproj" </> ".hxs-stamp.swift"

       , "module.modulemap"

       , defaultDebugXCConfigFile
       , defaultReleaseXCConfigFile

       , projName <.> "cabal"
       , "flib" </> "MyForeignLib.hs"
       , "cbits" </> "MyForeignLibRts.h"
       , "cbits" </> "MyForeignLibRts.c"

       , "cabal.project"

       , ".gitignore"
       ]

  -- xcode
  "//*.xcodeproj" </> "project.pbxproj" %> createOnly \out -> do
    throwIO (NoInitXCodeProj out) & liftIO

  -- xcode: Patch .xcodeproj
  -- We use a stamp file (that is the ruby script) to mark that we've adapted the project file already.
  projDir </> projName <.> "xcodeproj" </> ".hxs-stamp.rb" %> createOnly \out -> do
    flib_path <- cabalForeignLibPath projDir projName
    writeFile' out (adaptXCProjRubyScript (takeDirectory out) flib_path)
    Exit exitCode <- cmd "ruby" out -- execute ruby script
    case exitCode of
      ExitSuccess
        -> putInfo "Successfully adjusted .xcodeproj using the ruby script"
      _ -> do
        -- Running the script failed. We delete the script so that we try again later.
        removeFiles "." [out] & liftIO
        throwIO FailXCodeProjEdit & liftIO

  -- swift: Patch main
  projDir </> projName <.> "xcodeproj" </> ".hxs-stamp.swift" %> createOnly \out -> do
    let appMainPath = projDir </> projName </> [i|#{projName}App.swift|]
        contentViewPath = projDir </> projName </> "ContentView.swift"
    appMain <- readFile' appMainPath
    contentView <- readFile' contentViewPath
    let adjustedMain = concatMap (injectSwiftMainApp projName) (lines appMain) & unlines
        adjustedContentView = concatMap (injectSwiftContentView projName) (lines contentView) & unlines
    writeFile' appMainPath adjustedMain
    writeFile' contentViewPath adjustedContentView
    writeFile' out $ unlines
      [ [i|// #{projName}App.swift|]
      , defaultRTSImport projName
      , defaultRTSInitApp
      , "// ContentView.swift"
      , defaultHaskelFFIImports projName
      , defaultForeignImportCV
      , defaultHaskellHelloUI
      ]
    putInfo "Successfully adjusted the App entry point to initialize the Haskell runtime system."

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

  projDir </> "cabal.project" %> createOnly \out -> do
    writeFile' out defaultCabalProject

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
    \#include "#{dynamicXCConfigFile}"
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
  ]
  ++
  -- The --extension flag adds to 'other-extensions' which I don't think adds
  -- the extension to all modules...
  map (\x -> [i|--extension=#{x}|])
    cabalDefaultExtensions

cabalDefaultDeps :: [String]
cabalDefaultDeps = [ "base", "bytestring", "aeson" ]

cabalDefaultExtensions :: [String]
cabalDefaultExtensions = [ "ForeignFunctionInterface", "DerivingStrategies", "DerivingVia", "DeriveGeneric", "DeriveAnyClass" ]

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
    -- to #{foreignIncludeStubsDir}).
    ghc-options: -stubdir=#{foreignIncludeStubsDir}

    other-modules: MyForeignLib
    hs-source-dirs: flib
    build-depends: #{projName}, swift-ffi, #{S.intercalate ", " cabalDefaultDeps}
    default-extensions: #{S.intercalate ", " cabalDefaultExtensions}

    include-dirs: cbits
    c-sources: cbits/MyForeignLibRts.c
    install-includes: MyForeignLibRts.h
|]

stubMyForeignLib :: String
stubMyForeignLib = [__i'E|
    {-\# LANGUAGE TemplateHaskell \#-}
    module MyForeignLib where
    import GHC.Generics
    import Data.Aeson
    import Data.ByteString.Lazy (toStrict)
    import Foreign.Swift

    data User
      = User
      { favouriteNumbers :: [Int]
      , name :: String
      }
      deriving stock Generic
      deriving anyclass (FromJSON, ToJSON)

    newUser :: String -> User
    newUser n = User{favouriteNumbers = take 42 fibs, name = n}

    fibs :: [Int]
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

    $(foreignExportSwift 'newUser)
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

        printf("Initialising flib\\n");

        // Initialise Haskell runtime
        hs_init(NULL, NULL);

        // Do other library initialisations here

        return HS_BOOL_TRUE;
    }

    void flib_end() {
        printf("Terminating flib\\n");
        hs_exit();
    }
|]

defaultCabalProject :: String
defaultCabalProject = [__i'E|
    packages: .

    source-repository-package
      type: git
      location: https://github.com/alt-romes/haskell-swift
      subdir: swift-ffi
|]

--------------------------------------------------------------------------------
-- Ruby script to adapt the default .xcodeproj
--------------------------------------------------------------------------------

adaptXCProjRubyScript :: FilePath -> FilePath -> String
adaptXCProjRubyScript xcodeprojPathRelativeToRunner haskellFLibRelativeToProj = [__i'E|
  require 'xcodeproj'
  project_path = '#{xcodeprojPathRelativeToRunner}'
  project = Xcodeproj::Project.open(project_path)

  \# Set `.xcconfig` files as configurations for Debug and Release
  project.build_configurations.each do |config|
    config.base_configuration_reference ||= project.new_file("#{xcConfigsDir}/\#{config.name}.xcconfig")
  end

  \# Add Copy Files build phase to copy the haskell foreign library to Frameworks to each target
  lib_file = project.new_file('#{haskellFLibRelativeToProj}')
  \# We need to set 'CodeSignOnCopy' for the library to be linked without issues!
  copy_files_phase = project.new(Xcodeproj::Project::Object::PBXCopyFilesBuildPhase)
  copy_files_phase.symbol_dst_subfolder_spec = :frameworks
  copy_files_phase.add_file_reference(lib_file)
  copy_files_phase.build_file(lib_file).settings = { 'ATTRIBUTES' => ['CodeSignOnCopy'] }
  project.native_targets.each do |tgt|
    tgt.build_phases << copy_files_phase
  end

  \# Create a (1) remote package reference, a (2) dependency on a product from that package, and a (3) build file from that product
  \# Then, (4) add the build_file to the files of the Framework phase of each target,
  \# (5) add the product dependency to the target, and (6) add the package reference to the main project dependencies

  \# (1)
  hsffi_pkg = project.new(Xcodeproj::Project::Object::XCRemoteSwiftPackageReference)
  hsffi_pkg.repositoryURL='https://github.com/alt-romes/hsffi-swiftpkg-mirror'
  hsffi_pkg.requirement = { 'kind' => 'upToNextMajorVersion', 'minimumVersion' => '0.1.0' }

  \# (2)
  hsffi_prod = project.new(Xcodeproj::Project::Object::XCSwiftPackageProductDependency)
  hsffi_prod.package = hsffi_pkg
  hsffi_prod.product_name = "HaskellFFI"

  \# (3)
  hsffi_buildfile = project.new(Xcodeproj::Project::Object::PBXBuildFile)
  hsffi_buildfile.product_ref = hsffi_prod

  project.native_targets.each do |tgt|
    \# (4)
    tgt.frameworks_build_phase.files << hsffi_buildfile
    \# (5)
    tgt.package_product_dependencies << hsffi_prod
  end

  \# (6)
  project.root_object.package_references << hsffi_pkg

  project.save
|]

--------------------------------------------------------------------------------
-- Injecting code into Swift Main App
--------------------------------------------------------------------------------

defaultRTSImport :: String -> String
defaultRTSImport projName = [i|import #{projName}HS.RTSManage|]

defaultHaskelFFIImports :: String -> String
defaultHaskelFFIImports projName = [__i'E|
    import HaskellFFI
    import #{projName}HS.MyForeignLib
|]

defaultRTSInitApp :: String
defaultRTSInitApp = [i|
    init() {
        flib_init()

        NotificationCenter.default.addObserver(forName: NSApplication.willTerminateNotification, object: nil, queue: .main) { _ in
            // terminating
            flib_end()
        }
    }
|]

defaultForeignImportCV :: String
defaultForeignImportCV = [i|
struct User: Codable {
    let favouriteNumbers: [Int]
    let name: String
}

@ForeignImportHaskell
func newUser(cconv: HsCallJSON, name: String) -> User { hstub() }
|]

defaultHaskellHelloUI :: String
defaultHaskellHelloUI = [i|
            let u = newUser(name: "Simon")
            Text("User \\(u.name) really likes \\(u.favouriteNumbers.map({"\\($0)"}).joined())")
|]

injectSwiftMainApp :: String -> String -> [String]
injectSwiftMainApp projName line
  = if | "import" `S.isInfixOf` line
       -> [ line
          , defaultRTSImport projName
          ]
       | [i|struct #{projName}App: App|] `S.isInfixOf` line
       -> [ line
          , defaultRTSInitApp
          ]
       | otherwise
       -> [ line ]

injectSwiftContentView :: String -> String -> [String]
injectSwiftContentView projName line
  = if | "import" `S.isInfixOf` line
       -> [ line
          , defaultHaskelFFIImports projName
          , defaultForeignImportCV
          ]
       | "Hello, world!" `S.isInfixOf` line
       -- Drop the "Hello, world!" line for our own
       -> [ defaultHaskellHelloUI
          ]
       | otherwise
       -> [ line ]


--------------------------------------------------------------------------------
-- module.modulemap
--------------------------------------------------------------------------------

defaultModuleMap :: String -> String
defaultModuleMap projName = [__i'E|
        module #{projName}HS {
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

data InitExceptions
    = NoInitXCodeProj FilePath
    | FailXCodeProjEdit

instance Exception InitExceptions
instance Show InitExceptions where
    show (NoInitXCodeProj out)
        = [__i'E|
        Initializing XCode projects (.xcodeproj) programatically is not supported (by Apple, in general).
        Please create an XCode project manually, where the name matches the project root dir name
        (i.e. the project file must be #{out}).
    |]

    show FailXCodeProjEdit
      = [__i'E|
      Editing the .xcodeproj programmatically via the ruby script using the `xcodeproj` library failed.
      This is likely because the `xcodeproj` ruby library is not installed in your system. You can get it with:
      
      gem install xcodeproj

      It is unlikely, but the error might have also been caused by the (lack of) a ruby installation.
    |]

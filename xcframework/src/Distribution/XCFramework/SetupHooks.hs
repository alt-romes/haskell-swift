{-# LANGUAGE RecordWildCards, OverloadedRecordDot #-}
-- | Module to automatically produce a XCFramework binary distribution package
-- from a Haskell library
module Distribution.XCFramework.SetupHooks (xcframeworkHooks) where

import System.Process
import System.FilePath
import Distribution.Simple.SetupHooks
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo(compiler, hostPlatform),
      interpretSymbolicPathLBI, withPrograms )
import Distribution.Simple.BuildPaths (mkSharedLibName)
import Distribution.Simple.Setup (setupVerbosity)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Flag (fromFlag)
import Distribution.Simple.Program

-- | Add these hooks to your 'setupHooks' in @SetupHooks.hs@ to automatically
-- produce at the given location an xcframework from the Haskell library
-- component being built.
--
-- Non-library components (tests and executables) are ignored.
--
-- The resulting XCFramework includes the RTS and FFI headers, and the dylib
-- (TODO: configurable?) resulting from building the library component.
xcframeworkHooks :: FilePath -- ^ XCFramework result output filepath (must end with .xcframework)
                 -> SetupHooks
xcframeworkHooks out = noSetupHooks
  { buildHooks = noBuildHooks
    { postBuildComponentHook = Just $ postBuild out
    }
  }

-- TODO: This library should eventually also include the header files produced
-- by the library compiled. Currently swift-ffi handles that part separately.

-- | A per-component post-build action which produces the *.xcframework.
postBuild :: FilePath -- ^ XCFramework result output filepath (must end with .xcframework)
          -> PostBuildComponentInputs
          -> IO ()
postBuild outFile PostBuildComponentInputs{..} = do
  let
    verbosity = fromFlag $ setupVerbosity $ buildCommonFlags buildFlags
    i = interpretSymbolicPathLBI localBuildInfo
    clbi = targetCLBI targetInfo
    platform = hostPlatform localBuildInfo
    compiler = Distribution.Simple.LocalBuildInfo.compiler localBuildInfo
    compiler_id = compilerId compiler
    uid = componentUnitId clbi
    progDb = withPrograms localBuildInfo

    do_it libHSname = do

      let libHS = i (componentBuildDir localBuildInfo clbi) </> libHSname

      -- Get ghc-pkg program
      (ghcPkgProg, _) <- requireProgram verbosity ghcPkgProgram progDb
      let ghcPkg = programPath ghcPkgProg

      includeDirsStr <- readProcess ghcPkg ["field", "rts", "include-dirs", "--simple-output"] ""
      -- TODO: `words` won't work if the include dirs have spaces in them.
      let includeDirs = words includeDirsStr

      let cmd = unwords $
            [ "xcodebuild", "-create-xcframework"
            , "-output", outFile
            , "-library", libHS
            ] ++ concat
            [ ["-headers", d]
            | d <- includeDirs
            ]

      putStrLn "Creating XCFramework..."
      putStrLn cmd

      callCommand cmd

  case targetCLBI targetInfo of
    LibComponentLocalBuildInfo{}
      -> do_it (mkSharedLibName platform compiler_id uid)
    FLibComponentLocalBuildInfo{componentLocalName=CFLibName flibName}
      -> do_it ("lib" ++ prettyShow flibName ++ ".dylib")
    other ->
      putStrLn $
        "Ignoring xcframeworkHooks for non-library component "
          ++ prettyShow (componentLocalName other)

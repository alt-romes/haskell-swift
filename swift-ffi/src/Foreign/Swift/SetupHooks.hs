-- | A 'SetupHooks' module for when using 'Foreign.Swift.Lib' to automatically
-- produce a Swift library when building a Haskell library
module Foreign.Swift.SetupHooks where

import Distribution.Simple.SetupHooks

foreignSwiftSetupHooks :: SetupHooks
foreignSwiftSetupHooks = noSetupHooks
  { buildHooks = noBuildHooks
    { preBuildComponentRules = Nothing -- todo: delete existing distribution files before writing on top? Or just make sure to always write to a temp dir...
    }
  }

-- TODO:
-- Add "import Foundation"
-- Add (not public!):
-- enum HsFFIError: Error {
--     case requiredSizeIs(Int)
-- }
-- Add "import ModMap.Name.Mod_stub

# xcframework

Cabal hooks for producing an
[XCFramework](https://developer.apple.com/documentation/xcode/creating-a-multi-platform-binary-framework-bundle)
from a Haskell library bundling the library binary artifact, the RTS and
foreign-exports headers, and a modulemap exporting the headers as Swift
modules.

Refer also to the [release blogpost](https://alt-romes.github.io/posts/2025-07-05-packaging-a-haskell-library-as-a-swift-binary-xcframework.html).

## How to Use

In your cabal file, change the `build-type` to `Hooks` (and set `cabal-version:
3.14` if not set already):

```diff
- build-type:     Simple
+ build-type:     Hooks
```

And add a `setup-depends` stanza with a dependency on `xcframework`:

```diff
+ custom-setup
+   setup-depends:
+     base        >= 4.18 && < 5,
+     xcframework >= 0.1
```

Finally, create a file called `SetupHooks.hs` in the root of your Cabal package
with the following contents, substituting the `_build/MyHaskellLib.xcframework` string for the
filepath to where the `.xcframework` should be written:

```haskell
module SetupHooks ( setupHooks ) where

import Distribution.XCFramework.SetupHooks

setupHooks :: SetupHooks
setupHooks = xcframeworkHooks "_build/MyHaskellLib.xcframework"
```

Now, whenever you run `cabal build`, the libraries will also be bundled into an `.xcframework`.

## How to use the XCFramework in XCode

In XCode:

1. Navigate to the target settings of your project.
2. Find under "General" the "Frameworks, Libraries, and Embedded Content" (or similar) section.
3. Click the add button and add the `.xcframework` framework outputted at the specified path by Cabal

Now, in the entry Swift module, import the RTS and init/exit the RTS. For
instance, in a sample SwiftUI app:

```diff
  import SwiftUI
+ import Haskell.Foreign.Rts
  
  @main
  struct MyExample: App {
+ 
+     init() {
+         hs_init(nil, nil)
+
+         NotificationCenter.default
+           .addObserver(forName: NSApplication.willTerminateNotification,
+                        object: nil, queue: .main) { _ in
+           hs_exit()
+         }
+     }
+ 
      var body: some Scene {
          WindowGroup {
              ContentView()
          }
      }
  }
```

Finally, in any Swift module, do `import Haskell.Foreign.Exports`. For now, the
name `Haskell.Foreign.Exports` is fixed and exports all foreign-exported
functions, but it could be improved in the future (perhaps it's a good task to
contribute a patch for!)

For example, if your Haskell module looked like:

```
module MyLib (doSomething) where

fib :: Integral b => Int -> b
fib n = round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

doSomething :: IO Int
doSomething = do
  putStrLn "doing some thing"
  return $ fib 42

foreign export ccall doSomething :: IO Int
```

In your Swift module you can now

```swift
import Haskell.Foreign.Exports

let x = doSomething()
```

## Building simple Swift package

The `.xcframework` can also be easily used in a standalone swift package built
with `swift build`.

In your `Package.swift`, add `MyHaskellLib.xcframework` as a binary target and
make it a dependency of your main target. For instance, a simple library would
look like:

```swift
// swift-tools-version: 6.1
import PackageDescription

let package = Package(
    name: "MySwiftLib",
    platforms: [
        .macOS(.v15)
    ],
    products: [
        .library(name: "MySwiftLib", targets: ["MySwiftLib"])
    ],
    targets: [
        .target(name: "MySwiftLib", dependencies: ["MyHaskellLib"], path: "Swift"),
        .binaryTarget(
            name: "MyHaskellLib",
            path: "haskell/_build/MyHaskellLib.xcframework"
        )
    ]
)
```

Now you can use the `Haskell.Foreign.Export` import in any module in the
package as explained above, for instance in `Swift/MySwiftLib.hs`:

```swift
import Foundation
import Haskell.Foreign.Exports

public struct Fib {
  var val: Int64
}

public func mkFib() -> Fib {
    let x = doSomething()
    return Fib(val: x)
}
```

Build the Swift package using `swift build` in the project root.


## Must use Cabal Foreign Library stanza

Unfortunately, while I don't figure out how to link the right amount of things
into the `.xcframework` after building a normal `library` component in Cabal,
the `foreign export`s must be exported from a `foreign-library` Cabal stanza:

```
foreign-library myexample
    type: native-shared
    options: standalone
    other-modules:    MyLib
    build-depends:    base ^>=4.20.0.0
    hs-source-dirs:   src
    default-language: GHC2021
```

To clarify the instructions, I put together [a small demo
project](https://github.com/alt-romes/hs-xcframework-simple-demo/) with a
working setup -- if you want to try it out. Remember to build the Cabal library first!


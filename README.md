# haskell-swift

Home to haskell-swift interoperability libraries and tools

- **hxs**: a build-tool to setup and build an xcode project that links against a
    Haskell library as explained in [the first part of the guide](https://alt-romes.github.io/posts/2023-11-10-creating-a-macos-app-with-haskell-and-swift.html)
    - Usage instructions coming soon
    - Currently missing automatically adding as dependencies the to-be
        `haskell-swift` Haskell library and `swift-haskell` Swift package
        (As these libraries are not yet complete)
    - Preliminary bleeding edge installation instructions:
    ```
    # After cloning the repo
    cd haskell-swift/hxs
    cabal install
    # Create a new empty XCode project in a new directory, and run inside it:
    hxs init
    hxs build
    # Open XCode: in `ContentView.swift` and you should be able to
    import NameOfAppHS.MyForeignLib
    # and call hs_factorial
    Text("Factorial: \(hs_factorial(3))")
    ```
    - As I've said, this is still missing all the `@ForeingImportHaskell`,
        `$(foreignExportSwift ...)` niceties from the libraries that are WIP.



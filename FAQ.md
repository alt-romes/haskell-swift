Things are going wrong, help!

* Failed to find HsFFI.h

This is likely to be because the dynamic configuration is out of date (and you
have updated the compiler and deleted the old version).
For some reason, the Build Settings become stale despite the dynamic
configuration being updated after `hxs build`. In this case, I suggest manually
going to the `Build Settings` in XCode and editing the `Headers Search Path` key
to use the new version of the compiler (e.g. update paths from `9.6.3` to `9.6.4`).

* Failed to find the Haskell foreign library

This may also be caused by a compiler upgrade. We configured at initialization
time a path to the dynlib (see in `xxx.xcodeproj/.hsx-stamp.rb` the path to the
library and see if it refers the old GHC version).
To solve this outdated file that needs to be copied into the Frameworks at
build/install time, you can go to `Build Phases > Copy Files` and delete the
outdated path to the library. Then, introduce a new file to be copied to
`Frameworks` and click `Add Other` and select the path to the library under the
new compiler version...

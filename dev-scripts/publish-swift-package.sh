#!/usr/bin/env bash

# This script copies the Swift package in /haskell-ffi to the root of a
# separate repository from which the package is distributed (because Swift
# packages must be in the root of the repository, unfortunately)
# The version tags for the Swift package are also kept in that repository (we may duplicate some of the releases on this repo too)


if ! test -d haskell-ffi; then
    echo "./dev-scripts/publish-swift-package.sh must be run from the root of haskell-swift!"
    exit 1
fi

if test -z $1; then
    echo "./dev-scripts/publish-swift-package.sh expects the first argument to be a sem-version \"major.minor.patch\""
    exit 2
fi

PKG_ROOT=$(pwd)/haskell-ffi
TMP_DIR=$(mktemp -d)
echo $TMP_DIR

pushd . > /dev/null

cd $TMP_DIR

git clone git@github.com:alt-romes/hsffi-swiftpkg-mirror.git

# Update files
rm -rf hsffi-swiftpkg-mirror/* -v
cp -r $PKG_ROOT/* hsffi-swiftpkg-mirror/

# Update repo
cd hsffi-swiftpkg-mirror
git status
git add .
git commit -m "Version $1"
git tag $1
git push
git push --tags

popd > /dev/null

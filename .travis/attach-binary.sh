#!/bin/sh
set -o errexit -o verbose

if test ! "$BUILD_BINARY" || test ! "$TRAVIS_TAG"
then
  echo 'This is not a release build.'
elif test ! "$GITHUB_TOKEN"
then
  echo 'The GITHUB_TOKEN environment variable is not set!'
  exit 1
else
  echo "Building binary for $TRAVIS_OS_NAME to $TRAVIS_TAG..."
  stack build --ghc-options -O2 --pedantic
  echo "Attaching binary for $TRAVIS_OS_NAME to $TRAVIS_TAG..."
  OWNER="$(echo "$TRAVIS_REPO_SLUG" | cut -f1 -d/)"
  REPO="$(echo "$TRAVIS_REPO_SLUG" | cut -f2 -d/)"
  BIN="$(stack path --local-install-root)/bin/$REPO"
  BUNDLE_NAME="$REPO-$TRAVIS_TAG-$TRAVIS_OS_NAME.tar.gz"
  cp "$BIN" "./$REPO"
  chmod +x "./$REPO"
  tar -czf "$BUNDLE_NAME" "$REPO"
  echo "SHA256:"
  shasum -a 256 "$BUNDLE_NAME"
  ghr -t "$GITHUB_TOKEN" -u "$OWNER" -r "$REPO" --replace "$(git describe --tags)" "$BUNDLE_NAME"
fi

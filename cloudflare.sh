#!/usr/bin/env bash
set -euxo pipefail

curl https://sh.rustup.rs -sSf | sh -s -- -yt wasm32-unknown-unknown
source "$HOME/.cargo/env"

# as mentioned in CONTRIBUTING.md, we need the --keep-lld-exports flag, which
# will be available in the next wasm-bindgen release, but for now the latest
# release is 0.2.83 which does not have it, so we are using our own Linux binary
# which we built and uploaded to a GitHub Gist
wget -P ~/.cargo/bin \
  https://gist.github.com/samestep/2cf703cfd81691f2cb3c23422fce7e56/raw/f27692e7269f798faaf259b9c75942b151d8b69b/wasm-bindgen
chmod +x ~/.cargo/bin/wasm-bindgen

yarn build:docs-site

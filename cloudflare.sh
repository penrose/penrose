#!/usr/bin/env bash
set -euxo pipefail
curl https://sh.rustup.rs -sSf | sh -s -- -yt wasm32-unknown-unknown
source "$HOME/.cargo/env"
wget -P ~/.cargo/bin \
  https://gist.github.com/samestep/2cf703cfd81691f2cb3c23422fce7e56/raw/f27692e7269f798faaf259b9c75942b151d8b69b/wasm-bindgen
chmod +x ~/.cargo/bin/wasm-bindgen
yarn build:docs-site

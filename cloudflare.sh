#!/usr/bin/env bash
set -euxo pipefail

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source "$HOME/.cargo/env"

wget https://github.com/rustwasm/wasm-bindgen/releases/download/0.2.84/wasm-bindgen-0.2.84-x86_64-unknown-linux-musl.tar.gz
tar -xzf wasm-bindgen-0.2.84-x86_64-unknown-linux-musl.tar.gz
mv wasm-bindgen-0.2.84-x86_64-unknown-linux-musl/wasm-bindgen ~/.cargo/bin
rm -r wasm-bindgen-0.2.84-x86_64-unknown-linux-musl*

yarn build:docs-site

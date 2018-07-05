#!/bin/bash
# Used to generate haddock documentation side. Assunes `stack` is used and the OS is macOS
stack exec -- haddock --html src/*.hs  --hyperlinked-source --odir=dist/docs
open dist/docs/index.html

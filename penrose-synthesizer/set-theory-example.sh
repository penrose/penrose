#!/bin/bash

# Clean up output dir
rm -rf synthesized-progs

# Synthesize n Substance programs
penrose-synthesizer ../examples/set-theory-domain/setTheory.dsl --spec=sets-spec.dsl --num-programs=$1 --max-length=10

# Generate registries for Substance, Style, and Domain programs
python3 generateRegistry.py synthesized-progs $1 ./venn-synth.sty venn ../examples/set-theory-domain/setTheory.dsl set-theory

# Move the registries + synthesized programs to the automator dir for rendering
rsync -a -v --delete-after synthesized-progs/ ../automator/synthesized-progs

# Move to automator
cd ../automator

# CLean up output dir
rm -rf ../../penrose-artifacts/artifacts/*

# Render all diagrams
npm start -- batch substance.json style.json domain.json ../../penrose-artifacts/artifacts --folders --src-prefix="synthesized-progs"

# Go to artifact site generator
cd ../../penrose-artifacts

# Generate a static artifact page
npm start -- ./artifacts ./browser

# Open the page
open browser/index.html
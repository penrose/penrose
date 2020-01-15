#!/bin/bash

rm -rf synthesized-progs

# penrose-synthesizer ../examples/set-theory-domain/setTheory.dsl --spec=spec.dsl --num-programs=$1 --max-length=10
 penrose-synthesizer ../examples/set-theory-domain/setTheory.dsl --substance=../examples/set-theory-domain/threesets.sub --spec=spec.dsl --num-programs=$1 --max-length=10 --arg-mode=existing

python3 generateRegistry.py synthesized-progs $1 ./venn-synth.sty venn ../examples/set-theory-domain/setTheory.dsl set-theory
# cp -r synthesized-progs ../automator
rsync -a -v --delete-after synthesized-progs/ ../automator/synthesized-progs
cd ../automator
rm -rf ../../penrose-artifacts/artifacts/*
npm start -- batch substance.json style.json domain.json ../../penrose-artifacts/artifacts --folders --src-prefix="synthesized-progs"
cd ../../penrose-artifacts
npm start -- ./artifacts ./browser
open browser/index.html
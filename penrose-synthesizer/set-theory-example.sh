#!/bin/bash

penrose-synthesizer ../examples/set-theory-domain/setTheory.dsl --spec=spec.dsl --num-programs=$1 --max-length=10
python3 generateRegistry.py synthesized-progs $1 ../examples/set-theory-domain/venn.sty venn ../examples/set-theory-domain/setTheory.dsl set-theory
cp -r synthesized-progs ../automator
cd ../automator
npm start -- batch substance.json style.json domain.json ../../penrose-artifacts/artifacts --folders --src-prefix="synthesized-progs"
cd ../../penrose-artifacts
npm start -- ./artifacts ./browser
open browser/index.html
#!/bin/bash 
EXAMPLE='../../examples'
AUTOMATOR='../../automator'
ARTIFACT='../../penrose-artifacts'
NAME='figures'

# CLean up output dir
rm -rf $ARTIFACT/artifacts/$NAME
rm -rf $ARTIFACT/browser/*
mkdir $ARTIFACT/artifacts/$NAME

# Render all diagrams
npm start -- batch figures-substance.json figures-style.json figures-domain.json $ARTIFACT/artifacts/figures  --folders

# Go to artifact site generator
cd $ARTIFACT

# Generate a static artifact page
npm start -- ./artifacts/$NAME ./browser

# Open the page
open browser/index.html
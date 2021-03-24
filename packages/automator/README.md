# `automator`: a headless renderer for Penrose

## Getting started

- Make sure you have Node installed on your machine.
- Build everything: run `yarn build` in the project root directory.
- Make a folder for the ourput: go to `packages/automator` and run `mkdir out/`
- Start making diagrams: run `yarn start batch registry.json out/ --src-prefix=../../examples --folders` in this directory. The output will appear in `out` if successful.

## Viewing performance data

- Clone `penrose-artifacts`.
- Run `npm start -- <path-to-out> ./browser`
- Go to `browser/index.html` to view the result

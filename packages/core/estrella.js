#!/usr/bin/env node
const { build } = require("estrella");
build({
  entry: "./src/index.ts",
  outfile: "./build/dist/index.esm.js",
  bundle: true,
  sourcemap: true,
  format: "esm",
  tsconfig: "./tsconfig.json",
  platform: "node",
  //   incremental: true
});

build({
  entry: "./src/index.ts",
  outfile: "./build/dist/index.js",
  bundle: true,
  sourcemap: true,
  format: "cjs",
  tsconfig: "./tsconfig.json",
  platform: "node",
  //   incremental: true
});

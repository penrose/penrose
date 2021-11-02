#!/usr/bin/env node
const { build } = require("estrella");

build({
  sourcemap: "inline",
  keepNames: true,
  tsconfig: "./tsconfig.json",
  entry: "./src/index.ts",
  outfile: "./dist/index.js",
  bundle: true,
});

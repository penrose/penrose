#!/usr/bin/env node
const { build } = require("estrella");

build({
  sourcemap: "inline",
  format: "esm",
  keepNames: true,
  minify: false,
  tsconfig: "./tsconfig.json",
  entry: "./src/index.ts",
  outfile: "./dist/index.js",
  platform: "browser",
  bundle: true,
});

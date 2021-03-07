#!/usr/bin/env node
const { build } = require("estrella");
const common = {
  entry: "./src/index.ts",
  bundle: true,
  sourcemap: true,
  tsconfig: "./tsconfig.json",
  platform: "browser",
  external: ["path", "fs", "crypto"],
  //   incremental: true
};
build({ ...common, outfile: "./build/dist/index.esm.js", format: "esm" });

build({
  ...common,
  outfile: "./build/dist/index.js",
  format: "cjs",
  silent: true,
});

#!/usr/bin/env node
const { build, file } = require("estrella");
const common = {
  entry: "./src/index.ts",
  bundle: true,
  sourcemap: "inline",
  tsconfig: "./tsconfig.json",
  platform: "browser",
  external: ["path", "fs", "crypto"],
};
build({
  ...common,
  outfile: "./build/dist/index.esm.js",
  format: "esm",
  minify: false,
  tslint: "off",
  run: "yarn run build-decls",
});

build({
  ...common,
  outfile: "./build/dist/index.js",
  platform: "node",
  minify: false,
  format: "cjs",
  sourcemap: "both",
});

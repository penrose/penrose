#!/usr/bin/env node
const { build, file } = require("estrella");
const common = {
  entry: "./src/index.ts",
  bundle: true,
  keepNames: true,
  tsconfig: "./tsconfig.json",
};
build({
  ...common,
  outfile: "./build/dist/index.esm.js",
  platform: "browser",
  format: "esm",
  run: "yarn run build-decls",
  sourcemap: "inline",
  external: ["path", "fs", "crypto"],
});

build({
  ...common,
  outfile: "./build/dist/index.js",
  platform: "node",
  minify: false,
  format: "cjs",
  // silent: true,
  sourcemap: "external",
});

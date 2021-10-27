#!/usr/bin/env node
const { build, file } = require("estrella");
const common = {
  entry: "./src/index.ts",
  bundle: true,
  sourcemap: "inline",
  keepNames: true,
  tsconfig: "./tsconfig.json",
  platform: "browser",
  external: ["path", "fs", "crypto"],
};
build({
  ...common,
  outfile: "./build/dist/index.esm.js",
  format: "esm",
  minify: false,
  run: "yarn run build-decls",
});

build({
  ...common,
  outfile: "./build/dist/index.js",
  platform: "node",
  minify: false,
  format: "cjs",
  // silent: true,
  sourcemap: "both",
});

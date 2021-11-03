#!/usr/bin/env node
const { build } = require("estrella");
const common = {
  entry: "./src/index.ts",
  bundle: true,
  sourcemap: "inline",
  keepNames: true,
  tsconfig: "./tsconfig.json",
  // platform: "browser",
};
build({
  ...common,
  outfile: "./dist/index.esm.js",
  format: "esm",
  minify: false,
});

build({
  ...common,
  outfile: "./dist/index.js",
  format: "cjs",
  minify: false,
  define: {
    global: "globalThis", // HACK: `eigen` somehow uses `global`, overwriting it with anything will make it work (?). Need to figure out how to do polyfill
  },
});

#!/usr/bin/env node

import { build } from "esbuild";

const common = {
  entryPoints: ["./src/index.ts"],
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
});

build({
  ...common,
  outfile: "./build/dist/index.js",
  platform: "node",
  // https://github.com/evanw/esbuild/pull/2067#issuecomment-1324171716
  banner: {
    js:
      "import { createRequire } from 'module'; const require = createRequire(import.meta.url);",
  },
  minify: false,
  format: "esm",
  sourcemap: "both",
});

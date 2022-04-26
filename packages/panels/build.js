#!/usr/bin/env node

const { build, cliopts } = require("estrella");

build({
  entry: "./src/index.tsx",
  outfile: "public/app.js",
  bundle: true,
  minify: true,
  sourcemap: true,
  platform: "browser",
  external: ["path", "crypto", "fs"],
  run: "tsc",
  tslint: "off",
  define: {
    "process.env.NODE_ENV": cliopts.watch ? '"development"' : '"production"',
    global: "window",
  },
  loader: {
    ".ttf": "dataurl",
  },
});

cliopts.watch &&
  require("serve-http").createServer({
    port: 3000,
    pubdir: "./public",
    indexFilename: "index.html",
  });

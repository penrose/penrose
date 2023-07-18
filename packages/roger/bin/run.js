#!/usr/bin/env node
console.log("process.cwd()", process.cwd());
console.log("process.env.INIT_CWD", process.env.INIT_CWD);
import("../dist/index.js");

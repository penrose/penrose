import * as fs from "fs/promises";

// in order to get SolidJS to actually do stuff rather than compiling away all
// the interesting optimization code, we set the "browser" condition for our
// tests; see `vite.config.ts` in this same directory

// however, Rose interprets the "browser" condition to mean "fetch the Wasm
// directly rather than embedding it as Base64," which Vite build dev mode and
// build mode are both fine with, but Vitest chokes without this extra config

global.fetch = (url) => fs.readFile(url.href.replace("file://", ""));

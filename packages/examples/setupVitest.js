import * as fs from "fs/promises";

global.fetch = (url) => fs.readFile(url.href.replace("file://", ""));

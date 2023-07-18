import fs from "fs";
import path from "path";
export default {
  load() {
    return fs.readFileSync(
      path.resolve("./public/vanilla-js-demo.html"),
      "utf-8",
    );
  },
};

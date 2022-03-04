#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const extensions = new Set([".dsl", ".sty", ".sub", ".svg"]);

const build = (root) => {
  const children = [];
  for (const child of fs.readdirSync(root)) {
    const p = path.join(root, child);
    if (fs.statSync(p).isDirectory()) {
      build(p);
      children.push({ n: child, p: `./${child}/index` });
    } else if (extensions.has(path.extname(p))) {
      fs.writeFileSync(
        `${p}.json`,
        JSON.stringify(fs.readFileSync(p).toString())
      );
      children.push({ n: child, p: `./${child}.json` });
    }
  }

  const lines = [];
  for (const [i, { p }] of children.entries()) {
    lines.push(`import _${i} from ${JSON.stringify(p)};`);
  }
  lines.push("", "export default {");
  for (const [i, { n }] of children.entries()) {
    lines.push(`  ${JSON.stringify(n)}: _${i},`);
  }
  lines.push("};", "");
  fs.writeFileSync(path.join(root, "index.ts"), lines.join("\n"));
};

build("examples");

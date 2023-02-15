#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const src = "src";
const dist = "dist";
const extensions = new Set([".domain", ".style", ".substance", ".svg"]);

const build = (dir) => {
  const srcDir = path.join(src, dir);
  const distDir = path.join(dist, dir);
  fs.mkdirSync(distDir);

  const children = [];
  for (const child of fs.readdirSync(srcDir)) {
    const dirChild = path.join(dir, child);
    const srcDirChild = path.join(src, dirChild);
    if (fs.statSync(srcDirChild).isDirectory()) {
      build(dirChild);
      children.push({ n: child, p: `./${child}/index` });
    } else if (extensions.has(path.extname(child))) {
      fs.writeFileSync(
        `${path.join(dist, dirChild)}.json`,
        JSON.stringify(fs.readFileSync(srcDirChild).toString())
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
  fs.writeFileSync(path.join(distDir, "index.ts"), lines.join("\n"));
};

fs.rmSync(dist, { force: true, recursive: true });
build("");

#!/usr/bin/env node

const { execFileSync } = require("child_process");
const fs = require("fs");
const Handlebars = require("handlebars");
const path = require("path");

const slurp = (p) => fs.readFileSync(p, "utf8");

const dir = "packages/examples/src/set-theory-domain";
const trio = path.join(dir, "tree-venn.trio.json");
const { substance, style, domain, variation } = JSON.parse(slurp(trio));
if (style.length !== 1)
  throw Error(`expected exactly one Style program, got ${style.length}`);

execFileSync("roger", ["trio", "--out=docs/assets/output.svg", trio], {
  stdio: "inherit",
});

for (const name of ["domain", "substance", "style"])
  Handlebars.registerPartial(name, `{{{${name}}}}`);

fs.writeFileSync(
  "README.md",
  Handlebars.compile(slurp(".github/readme_template.hbs"))({
    variation,
    dsl: path.basename(domain),
    sub: path.basename(substance),
    sty: path.basename(style[0]),
    domain: slurp(path.join(dir, domain)),
    substance: slurp(path.join(dir, substance)),
    style: slurp(path.join(dir, style[0])),
  })
);

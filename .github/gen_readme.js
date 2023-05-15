#!/usr/bin/env node

const { execFileSync } = require("child_process");
const fs = require("fs");
const Handlebars = require("handlebars");
const path = require("path");

const slurp = (p) => fs.readFileSync(p).toString();
const example = (p) => slurp(path.join("packages/examples/src", p));

const sub = "tree";
const sty = "venn";

const { trios, domains, styles, substances } = JSON.parse(
  example("registry.json")
);

const matching = trios.filter(
  ({ substance, style }) => substance === sub && style === sty
);
if (matching.length !== 1)
  throw Error(`expected exactly one matching trio, got ${matching.length}`);
const [{ substance, style, domain, variation }] = matching;

const dslURI = domains[domain].URI;
const subURI = substances[substance].URI;
const styURI = styles[style].URI;

execFileSync(
  "yarn",
  [
    "start",
    "trio",
    subURI,
    styURI,
    dslURI,
    "../../docs/assets/diagram.svg",
    "--path",
    "../examples/src",
    "--variation",
    variation,
  ],
  { cwd: "packages/roger", stdio: "inherit" }
);

for (const name of ["domain", "substance", "style"])
  Handlebars.registerPartial(name, `{{{${name}}}}`);

fs.writeFileSync(
  "README.md",
  Handlebars.compile(slurp(".github/readme_template.hbs"))({
    variation,
    dsl: path.basename(dslURI),
    sub: path.basename(subURI),
    sty: path.basename(styURI),
    domain: example(dslURI),
    substance: example(subURI),
    style: example(styURI),
  })
);

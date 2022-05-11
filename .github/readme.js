#!/usr/bin/env node

// check that the `## Example` section in `README.md` matches the registry

const commonmark = require("commonmark");
const fs = require("fs");
const path = require("path");

const exampleSection = (doc) => {
  let node = doc.firstChild;
  if (node.level !== 1) {
    throw Error("expected level 1 heading first");
  }
  do {
    node = node.next;
    if (node.level === 1) {
      throw Error(`unexpected additional level 1 heading at ${node.sourcepos}`);
    }
  } while (!(node.level === 2 && node.firstChild.literal === "Example"));

  const nodes = [];
  node = node.next;
  while (node !== null && (node.level === null || node.level > 2)) {
    nodes.push(node);
    node = node.next;
  }
  return nodes;
};

const reader = new commonmark.Parser();
const doc = reader.parse(fs.readFileSync("README.md").toString());
const nodes = exampleSection(doc);

const sub = "tree";
const sty = "venn";
let domain, variation, dslURI, styURI, subURI;
let foundVariation = false;
let found = { ".dsl": false, ".sty": false, ".sub": false };
for (const node of nodes) {
  if (node.type === "html_block" && node.literal.startsWith("<img ")) {
    if (variation !== undefined) {
      throw Error(`unexpected additional image at ${node.sourcepos}`);
    }
    const src = `"diagrams/${sub}-${sty}.svg"`;
    if (!node.literal.startsWith(`<img src=${src}`)) {
      throw Error(`expected image at ${node.sourcepos} to be named ${src}`);
    }

    const { trios, domains, styles, substances } = JSON.parse(
      fs.readFileSync("packages/examples/src/registry.json").toString()
    );

    const matching = trios.filter(
      ({ substance, style }) => substance === sub && style === sty
    );
    if (matching.length !== 1) {
      throw Error(`expected exactly one matching trio, got ${matching.length}`);
    }

    [{ domain, variation }] = matching;
    dslURI = domains[domain].URI;
    styURI = styles[sty].URI;
    subURI = substances[sub].URI;
  } else if (node.type === "paragraph") {
    for (let child = node.firstChild; child !== null; child = child.next) {
      if (child.type === "code" && child.literal === variation) {
        foundVariation = true;
      }
    }
  } else if (node.type === "list") {
    for (let item = node.firstChild; item !== null; item = item.next) {
      const paragraph = item.firstChild;
      const codeBlock = paragraph.next;
      if (codeBlock.next !== null) {
        throw Error(`more than 2 nodes in list item at ${item.sourcepos}`);
      }

      if (paragraph.type !== "paragraph") {
        throw Error(
          `expected paragraph at ${paragraph.sourcepos}, got ${paragraph.type}`
        );
      }
      const filename = paragraph.firstChild;
      if (filename.type !== "code") {
        throw Error(
          `expected code child at ${paragraph.sourcepos}, got ${filename.type}`
        );
      }
      const ext = path.extname(filename.literal);
      const uri = { ".dsl": dslURI, ".sty": styURI, ".sub": subURI }[ext];
      if (uri === undefined) {
        throw Error(`unexpected extension ${ext} at ${filename.sourcepos}`);
      }
      if (!uri.endsWith(filename.literal)) {
        throw Error(`expected ${filename.literal} to match ${uri}`);
      }
      found[ext] = true;

      const contents = fs
        .readFileSync(path.join("packages/examples/src", uri))
        .toString();
      if (codeBlock.type !== "code_block") {
        throw Error(
          `expected code block at ${codeBlock.sourcepos}, got ${codeBlock.type}`
        );
      }
      if (codeBlock.literal !== contents) {
        throw Error(
          `expected code at ${codeBlock.sourcepos} to match contents of ${uri}`
        );
      }
    }
  }
}

if (variation === undefined) {
  throw Error("couldn't find SVG");
}
if (!foundVariation) {
  throw Error("couldn't find variation");
}
if (!found[".dsl"]) {
  throw Error("couldn't find domain");
}
if (!found[".sub"]) {
  throw Error("couldn't find substance");
}
if (!found[".sty"]) {
  throw Error("couldn't find style");
}

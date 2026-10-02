/** Check source provenance, placement rectangles and actual SVG XML exports. */
import { JSDOM } from "jsdom";
import assert from "node:assert/strict";
import { access, readFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { substanceSourceLines } from "./program-sources.mjs";

const root = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const auditDirectory = path.join(root, "docs/elementary-topology");
const manifest = JSON.parse(
  await readFile(path.join(auditDirectory, "figures.json"), "utf8"),
);
const audit = JSON.parse(
  await readFile(path.join(auditDirectory, "source-audit.json"), "utf8"),
);
assert.equal(manifest.sourceSha256, audit.source.sha256);
const ids = new Set();
let reviewed = 0;
for (const figure of manifest.figures) {
  assert(!ids.has(figure.id), `Duplicate figure ${figure.id}`);
  ids.add(figure.id);
  const sourcePage = audit.pdfPages[figure.pdfPage - 1];
  assert(sourcePage, `Missing source page for ${figure.id}`);
  assert.equal(
    String(figure.printedPage),
    String(sourcePage.printedPage),
    `Printed-page mismatch for ${figure.id}`,
  );
  if (figure.status !== "reviewed") continue;
  reviewed++;
  const [x, y, w, h] = figure.sourceBox ?? [];
  assert(
    [x, y, w, h].every(Number.isFinite) &&
      x >= 0 &&
      y >= 0 &&
      w > 0 &&
      h > 0 &&
      x + w <= 1 &&
      y + h <= 1,
    `Invalid source placement for ${figure.id}`,
  );
  await access(path.join(root, figure.implementation));
  await access(path.join(root, figure.style));
  assert(
    figure.buildFactory && figure.domainModule && figure.substanceFactory,
    `Missing interactive program metadata for ${figure.id}`,
  );
  await access(path.join(root, figure.domainModule));
  assert.deepEqual(
    figure.substanceLines,
    await substanceSourceLines(root, figure),
    `Stale source range for ${figure.id}`,
  );
  if (figure.review.record)
    await access(path.join(auditDirectory, figure.review.record));
  const svg = await readFile(
    path.join(
      root,
      "packages/docs-site/public/elementary-topology/figures",
      figure.svg,
    ),
    "utf8",
  );
  const parsed = new JSDOM(svg, { contentType: "image/svg+xml" });
  const element = parsed.window.document.documentElement;
  assert.equal(element.localName, "svg", `Not SVG: ${figure.id}`);
  assert.equal(
    element.namespaceURI,
    "http://www.w3.org/2000/svg",
    `Missing SVG namespace: ${figure.id}`,
  );
  assert.equal(
    element.getAttribute("role"),
    "img",
    `Missing image role: ${figure.id}`,
  );
  assert(
    element.getAttribute("aria-label"),
    `Missing accessible description: ${figure.id}`,
  );
  assert(!svg.includes("NaN"), `Nonfinite SVG geometry: ${figure.id}`);
  parsed.window.close();
}
const additions = JSON.parse(
  await readFile(
    path.join(auditDirectory, "original-illustrations.json"),
    "utf8",
  ),
);
for (const figure of additions.illustrations) {
  assert(
    !ids.has(figure.id),
    `Original illustration overlaps the source inventory: ${figure.id}`,
  );
  assert.equal(figure.status, "reviewed-original");
  assert.deepEqual(
    figure.substanceLines,
    await substanceSourceLines(root, figure),
    `Stale original Substance range: ${figure.id}`,
  );
  for (const filename of [
    figure.implementation,
    figure.style,
    figure.domainModule,
  ])
    await access(path.join(root, filename));
  const svg = await readFile(
    path.join(
      root,
      "packages/docs-site/public/elementary-topology/figures",
      figure.svg,
    ),
    "utf8",
  );
  const parsed = new JSDOM(svg, { contentType: "image/svg+xml" });
  const element = parsed.window.document.documentElement;
  assert.equal(element.localName, "svg");
  assert.equal(element.namespaceURI, "http://www.w3.org/2000/svg");
  assert.equal(element.getAttribute("role"), "img");
  assert(element.getAttribute("aria-label"));
  assert(!/NaN|Infinity/.test(svg));
  parsed.window.close();
}
console.log(
  `Verified ${manifest.figures.length} inventory entries, ${reviewed} reviewed SVGs and their source placements.`,
);
console.log(
  `Verified ${additions.illustrations.length} original illustrations and their actual Substance declarations.`,
);

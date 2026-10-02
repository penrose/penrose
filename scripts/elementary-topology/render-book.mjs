/** Export the reviewed textbook figures through their audited Penrose programs. */
import { JSDOM } from "jsdom";
import assert from "node:assert/strict";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const root = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const manifest = JSON.parse(
  await readFile(
    path.join(root, "docs/elementary-topology/figures.json"),
    "utf8",
  ),
);
const additions = JSON.parse(
  await readFile(
    path.join(root, "docs/elementary-topology/original-illustrations.json"),
    "utf8",
  ),
);
const available = [...manifest.figures, ...additions.illustrations];
const output = path.resolve(
  process.argv[2] ?? path.join(root, "tmp/elementary-topology/recreated"),
);
const selectedIds = process.argv[3]?.split(",");
const figures = available.filter((figure) =>
  selectedIds
    ? selectedIds.includes(figure.id)
    : ["reviewed", "reviewed-original"].includes(figure.status),
);
if (selectedIds) {
  for (const id of selectedIds)
    assert(
      figures.some((figure) => figure.id === id),
      `Unknown figure ${id}`,
    );
}
await mkdir(output, { recursive: true });
const browser = new JSDOM("<!doctype html><html><body></body></html>", {
  pretendToBeVisual: true,
});
globalThis.window = browser.window;
globalThis.document = browser.window.document;
globalThis.XMLSerializer = browser.window.XMLSerializer;
globalThis.requestAnimationFrame = browser.window.requestAnimationFrame.bind(
  browser.window,
);
globalThis.cancelAnimationFrame = browser.window.cancelAnimationFrame.bind(
  browser.window,
);

for (const figure of figures) {
  assert(
    figure.buildFactory && figure.implementation && figure.svg,
    `Missing build program for ${figure.id}`,
  );
  const modulePath = figure.implementation
    .replace("/src/", "/dist/")
    .replace(/\.tsx?$/, ".js");
  const program = await import(pathToFileURL(path.join(root, modulePath)).href);
  assert(
    typeof program[figure.buildFactory] === "function",
    `Missing ${figure.buildFactory} in ${modulePath}`,
  );
  const drawing = await program[figure.buildFactory](
    ...(figure.buildArguments ?? []),
  );
  try {
    while (await drawing.optimizationStep()) {}
    const { svg } = await drawing.render();
    svg.setAttribute("role", "img");
    svg.setAttribute(
      "aria-label",
      /^\d/.test(figure.id)
        ? `Figure ${figure.id}: ${figure.description}`
        : figure.description,
    );
    await writeFile(
      path.join(output, figure.svg),
      new XMLSerializer().serializeToString(svg),
    );
    console.log(`${figure.id}: ${path.join(output, figure.svg)}`);
  } finally {
    drawing.discard();
  }
}
browser.window.close();
// Bloom's browser scheduling channels keep Node alive after completed exports.
process.exit(0);

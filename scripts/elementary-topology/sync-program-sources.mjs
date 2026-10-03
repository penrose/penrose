/** Keep source-view metadata tied to the actual exported Substance declarations. */
import assert from "node:assert/strict";
import { readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { substanceSourceLines } from "./program-sources.mjs";

const root = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../..",
);
let count = 0;
for (const registry of ["figures", "original-illustrations"]) {
  const filename = path.join(root, `docs/elementary-topology/${registry}.json`);
  const manifest = JSON.parse(await readFile(filename, "utf8"));
  const figures =
    registry === "figures"
      ? manifest.figures.filter((f) => f.status === "reviewed")
      : manifest.illustrations;
  for (const figure of figures) {
    assert(
      figure.implementation && figure.substanceFactory && figure.domainModule,
      `Missing program provenance for ${figure.id}`,
    );
    const lines = await substanceSourceLines(root, figure);
    if (process.argv.includes("--check"))
      assert.deepEqual(
        figure.substanceLines,
        lines,
        `Stale Substance source range for ${figure.id}; run sync-program-sources.mjs`,
      );
    else figure.substanceLines = lines;
    count++;
  }
  if (!process.argv.includes("--check"))
    await writeFile(filename, JSON.stringify(manifest, null, 2) + "\n");
}
console.log(
  `${
    process.argv.includes("--check") ? "Verified" : "Updated"
  } ${count} Substance source ranges.`,
);

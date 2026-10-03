/** Reproduce two native solver matrices; source-inspired hints are initialization. */
import { JSDOM } from "jsdom";
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const root = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const output = path.resolve(
  process.argv[2] ??
    path.join(root, "tmp/elementary-topology/constraint-topology/revisions"),
);
const requested = process.argv[3] ?? "all";
if (!["before", "after", "final", "both", "all"].includes(requested))
  throw new Error("Revision must be before, after, final, both, or all");
const revisions =
  requested === "all"
    ? ["before", "after", "final"]
    : requested === "both"
    ? ["before", "after"]
    : [requested];
const browser = new JSDOM("<!doctype html><html><body></body></html>", {
  pretendToBeVisual: true,
});
Object.assign(globalThis, {
  window: browser.window,
  document: browser.window.document,
  XMLSerializer: browser.window.XMLSerializer,
  requestAnimationFrame: browser.window.requestAnimationFrame.bind(
    browser.window,
  ),
  cancelAnimationFrame: browser.window.cancelAnimationFrame.bind(
    browser.window,
  ),
});
const program = await import(
  pathToFileURL(
    path.join(
      root,
      "packages/bloom/dist/examples/constraint-topology-experiment.js",
    ),
  )
);
const sourceFiles = [
  "packages/bloom/src/styles/constraint-topology.tsx",
  "packages/bloom/src/examples/constraint-topology.ts",
  "packages/bloom/src/examples/constraint-topology-experiment.ts",
];
const hashes = Object.fromEntries(
  await Promise.all(
    sourceFiles.map(async (file) => [
      file,
      createHash("sha256")
        .update(await readFile(path.join(root, file)))
        .digest("hex"),
    ]),
  ),
);
const nonfinite = (_key, value) =>
  typeof value === "number" && !Number.isFinite(value) ? String(value) : value;
const examples = [
  "book-separation",
  "book-nested",
  "neighborhood-triangle",
  "bipartite-map",
  "L-membership",
  "unhinted-composition",
  "inconsistent",
];
const seeds = ["book", "cedar", "violet"],
  perturbations = [0, 5, 90, 160];
for (const revision of revisions) {
  const directory = path.join(output, revision);
  await mkdir(directory, { recursive: true });
  const visual =
    revision === "before"
      ? { labelDistanceBound: false, arrowheadSize: 5 }
      : {
          labelDistanceBound: 24,
          arrowheadSize: revision === "after" ? 0.7 : 1.3,
          labelBoundForm:
            revision === "after" ? "distance" : "squared-distance",
        };
  const results = [];
  async function run(example, seed, perturbation, geometryPriors) {
    const id = [
      example,
      seed,
      perturbation,
      geometryPriors ? "weak-priors" : "geometry-priors-off",
    ].join("--");
    const options = {
      ...visual,
      seed,
      perturbation,
      ...(geometryPriors ? {} : { positionPrior: 0, sizePrior: 0 }),
    };
    const result = await program.measureConstraintTopology(example, options);
    await writeFile(path.join(directory, id + ".svg"), result.svg);
    await writeFile(
      path.join(directory, id + ".json"),
      JSON.stringify(result.metrics, nonfinite, 2) + "\n",
    );
    results.push({ ...result.metrics, id });
    console.log(
      revision,
      id,
      JSON.stringify({
        finished: result.metrics.optimizationFinished,
        feasible: result.metrics.feasible,
        calls: result.metrics.calls,
        maxViolation: result.metrics.maxViolation,
        semanticMax: result.metrics.semanticMaxViolation,
        labelDistance: result.metrics.maxLabelAssociationDistance,
        crossings: result.metrics.crossings.length,
      }),
    );
  }
  for (const example of examples)
    for (const seed of seeds)
      for (const perturbation of perturbations)
        await run(example, seed, perturbation, true);
  // Remove all absolute geometry/size preferences; label-relative objectives remain.
  for (const example of examples.slice(2, -1))
    for (const seed of seeds) await run(example, seed, 90, false);
  await writeFile(
    path.join(directory, "results.json"),
    JSON.stringify(
      {
        schemaVersion: 1,
        generatedAt: new Date().toISOString(),
        nodeVersion: process.version,
        gitBase: execFileSync("git", ["rev-parse", "HEAD"], {
          cwd: root,
          encoding: "utf8",
        }).trim(),
        sourceSha256: hashes,
        revision,
        visualOptions: visual,
        solver: "native Penrose exterior-point L-BFGS; default engine settings",
        coordinates:
          "720×540 Penrose y-up geometry; independent checks read rendered SVG y-down geometry",
        tolerance: 1e-3,
        cases: results,
      },
      nonfinite,
      2,
    ) + "\n",
  );
}
browser.window.close();
// Bloom's browser scheduling channels keep Node alive after completed exports.
process.exit(0);

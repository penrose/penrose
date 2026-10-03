/**
 * Test reuse of the released constraint styles without per-instance geometry.
 * Build first: yarn --cwd packages/core build && yarn --cwd packages/bloom tsc
 * Run: node scripts/elementary-topology/constraint-generalization-experiments.mjs
 * Optional: --out=<json> --svg-dir=<directory> --step-limit=<positive integer>
 * Paths resolve from the repository, independently of the current directory.
 * Failures are recorded rather than hidden or repaired by changing Substance.
 */
import { JSDOM } from "jsdom";
import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { diagram } from "../../packages/bloom/dist/core/program.js";
import { canvas } from "../../packages/bloom/dist/core/utils.js";
import { setTheory } from "../../packages/bloom/dist/domains/set-theory.js";
import { independentTopologyClearances } from "../../packages/bloom/dist/examples/constraint-topology-experiment.js";
import { constraintTopologyStyles } from "../../packages/bloom/dist/styles/constraint-topology.js";

const root = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const arguments_ = process.argv.slice(2);
const accepted = ["out", "svg-dir", "step-limit"];
const argumentsMap = new Map();
for (const argument of arguments_) {
  const match = argument.match(/^--([^=]+)=(.+)$/);
  assert(match && accepted.includes(match[1]), "Unknown argument: " + argument);
  assert(!argumentsMap.has(match[1]), "Duplicate argument: " + match[1]);
  argumentsMap.set(match[1], match[2]);
}
const output = path.resolve(
  root,
  argumentsMap.get("out") ??
    "docs/elementary-topology/constraint-experiments/generalization.json",
);
const svgDirectory = path.resolve(
  root,
  argumentsMap.get("svg-dir") ??
    "tmp/elementary-topology/constraint-generalization",
);
const stepLimit = Number(argumentsMap.get("step-limit") ?? 800);
assert(
  Number.isSafeInteger(stepLimit) && stepLimit > 0 && stepLimit <= 2500,
  "step-limit must be an integer between1 and2500",
);
const tolerance = 1e-3;
const options = {
  seed: "generalization",
  perturbation: 45,
  positionPrior: 0,
  sizePrior: 0,
  padding: 8,
  arrowheadSize: 1.3,
};
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

/** Declarative mathematical family. The logical order of facts stays fixed. */
function family(count, variant) {
  const declarations = setTheory.substance();
  const renamed = variant === "renamed";
  const specs = [
    ...Array.from({ length: count }, (_, i) => ({
      id: `region${i}`,
      type: "Set",
      label: `${renamed ? "V" : "A"}_{${i + 1}}`,
    })),
    ...Array.from({ length: 2 * count }, (_, i) => ({
      id: `point${i}`,
      type: "Point",
      label: `${renamed ? "q" : "x"}_{${i + 1}}`,
    })),
    { id: "relation", type: "BinaryRelation", label: renamed ? "S" : "R" },
  ];
  const byId = new Map();
  const ordered = variant === "reversed" ? [...specs].reverse() : specs;
  for (const spec of ordered)
    byId.set(spec.id, declarations[spec.type]({ label: spec.label }));
  for (let i = 0; i < count; i++) {
    for (let p = 0; p < 2; p++)
      declarations.Member(
        byId.get(`point${2 * i + p}`),
        byId.get(`region${i}`),
      );
    for (let j = i + 1; j < count; j++)
      declarations.Disjoint(byId.get(`region${i}`), byId.get(`region${j}`));
    if (i + 1 < count)
      declarations.RelatedUnder(
        byId.get("relation"),
        byId.get(`point${2 * i}`),
        byId.get(`point${2 * (i + 1)}`),
      );
  }
  return { substance: declarations.make(), specs, byId };
}

function renderedObjects(svg, specs) {
  const objects = new Map();
  for (const element of svg.querySelectorAll("[aria-label]")) {
    const label = element.getAttribute("aria-label");
    for (const spec of specs) {
      const prefix = spec.type === "Set" ? "Set " : "Point ";
      if (spec.type === "BinaryRelation" || label !== prefix + spec.label)
        continue;
      assert(
        element.tagName.toLowerCase() === "circle",
        "Expected native circle",
      );
      objects.set(spec.id, {
        id: spec.id,
        label: spec.label,
        center: [
          Number(element.getAttribute("cx")),
          Number(element.getAttribute("cy")),
        ],
        radius: Number(element.getAttribute("r")),
      });
    }
  }
  return objects;
}

/** Independent straight-route check. Crossing-free routing is not enforced. */
function routeMeasurements(svg, objects, byId, count) {
  const edges = [];
  for (let i = 0; i + 1 < count; i++) {
    const source = byId.get(`point${2 * i}`),
      target = byId.get(`point${2 * (i + 1)}`);
    const label = `${byId.get("relation").label}: ${source.label} to ${
      target.label
    }`;
    const native = [...svg.querySelectorAll("[aria-label]")].find(
      (element) => element.getAttribute("aria-label") === label,
    );
    assert(
      native?.querySelector("line"),
      "Expected rendered native relation " + label,
    );
    edges.push({
      label,
      source: `point${2 * i}`,
      target: `point${2 * (i + 1)}`,
      from: objects.get(`point${2 * i}`).center,
      to: objects.get(`point${2 * (i + 1)}`).center,
    });
  }
  const orient = (a, b, c) =>
    (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0]);
  const crossings = [];
  for (let i = 0; i < edges.length; i++)
    for (let j = i + 1; j < edges.length; j++) {
      const a = edges[i],
        b = edges[j];
      if ([a.source, a.target].some((id) => id === b.source || id === b.target))
        continue;
      if (
        orient(a.from, a.to, b.from) * orient(a.from, a.to, b.to) < 0 &&
        orient(b.from, b.to, a.from) * orient(b.from, b.to, a.to) < 0
      )
        crossings.push([a.label, b.label]);
    }
  const renderedEdgeCount = [...svg.querySelectorAll("[aria-label]")].filter(
    (element) =>
      element
        .getAttribute("aria-label")
        .startsWith(byId.get("relation").label + ": "),
  ).length;
  assert.equal(
    renderedEdgeCount,
    count - 1,
    "Unexpected rendered relation count",
  );
  return { expectedEdgeCount: count - 1, renderedEdgeCount, crossings };
}

await mkdir(svgDirectory, { recursive: true });
// Reuse these very same two style programs, not merely their factory code.
const bundle = constraintTopologyStyles(options);
const results = [];
for (const count of [2, 3, 5])
  for (const variant of ["baseline", "renamed", "reversed"]) {
    const id = `${count}-regions--${variant}`;
    const program = family(count, variant);
    let drawing;
    const started = performance.now();
    const inputStart = bundle.inputs.length;
    const record = {
      id,
      count,
      variant,
      expectedMembershipCount: 2 * count,
      expectedDisjointCount: (count * (count - 1)) / 2,
      declarationOrder: program.substance.entities.map(
        (entity) => entity.label,
      ),
    };
    try {
      drawing = await diagram({
        sub: program.substance,
        sty: [bundle.regions, bundle.relations],
        canvas: canvas(720, 540),
        variation: options.seed,
      });
      record.buildMs = performance.now() - started;
      record.initialDiagnostics = drawing.getConstraintDiagnostics();
      let calls = 0,
        stopped = false;
      const optimizationStart = performance.now();
      for (; calls < stepLimit; ) {
        calls++;
        if (!(await drawing.optimizationStep())) {
          stopped = true;
          break;
        }
      }
      const diagnostics = drawing.getConstraintDiagnostics();
      const { svg } = await drawing.render();
      const svgText = new XMLSerializer().serializeToString(svg);
      const objects = renderedObjects(svg, program.specs);
      assert.equal(
        objects.size,
        3 * count,
        "Missing rendered mathematical object",
      );
      const checks = independentTopologyClearances(
        svg,
        program.substance,
        options.padding,
      );
      assert.equal(
        checks.length,
        record.expectedMembershipCount + record.expectedDisjointCount,
      );
      const semanticMaxViolation = Math.max(
        0,
        ...checks.map((check) => check.residual),
      );
      const finiteSvg = !/NaN|Infinity/.test(svgText);
      const route = routeMeasurements(svg, objects, program.byId, count);
      record.optimizationMs = performance.now() - optimizationStart;
      record.calls = calls;
      record.stepLimit = stepLimit;
      record.stopped = stopped;
      record.optimizationFinished = diagnostics.optimizationFinished;
      record.registeredFeasible = diagnostics.feasible;
      record.registeredMaxViolation = diagnostics.maxViolation;
      record.semanticMaxViolation = semanticMaxViolation;
      record.independentSemanticFeasible = semanticMaxViolation <= tolerance;
      record.finiteSvg = finiteSvg;
      record.success =
        diagnostics.optimizationFinished &&
        diagnostics.feasible &&
        record.independentSemanticFeasible &&
        finiteSvg;
      record.checks = checks;
      record.routes = route;
      record.objects = [...objects.values()];
      record.explicitInputs = bundle.inputs.slice(inputStart).map((input) => ({
        ...input,
        final: drawing.getInput(input.name),
      }));
      record.residuals = diagnostics.constraints
        .filter((c) => c.active)
        .map(({ label, value, violation }) => ({ label, value, violation }));
      record.svg = path.relative(root, path.join(svgDirectory, id + ".svg"));
      await writeFile(path.join(svgDirectory, id + ".svg"), svgText);
      console.log(
        id,
        JSON.stringify({
          success: record.success,
          calls,
          semanticMaxViolation,
          registeredMaxViolation: diagnostics.maxViolation,
          crossings: route.crossings.length,
        }),
      );
    } catch (error) {
      record.success = false;
      record.error = String(error?.stack ?? error);
      console.log(id, "FAILED", String(error));
    } finally {
      drawing?.discard();
    }
    results.push(record);
  }

const composition = {
  expected: "rejected",
  sameBundleStylesReusedAcrossCases: true,
};
try {
  const a = constraintTopologyStyles(options),
    b = constraintTopologyStyles(options);
  const drawing = await diagram({
    sub: family(2, "baseline").substance,
    sty: [a.regions, b.relations],
    canvas: canvas(720, 540),
    variation: options.seed,
  });
  drawing.discard();
  composition.observed = "accepted";
  composition.matchesExpected = false;
} catch (error) {
  composition.observed = "rejected";
  composition.message = String(error?.message ?? error);
  composition.matchesExpected = composition.message.includes("same bundle");
}
composition.limit =
  "Region and relation modules communicate through a factory-private WeakMap keyed by the style context. Separate factory bundles do not share views. This experiment demonstrates reuse within one bundle, not a general style-module composition contract.";

const sourceFiles = [
  "packages/bloom/src/styles/constraint-topology.tsx",
  "packages/bloom/src/domains/set-theory.ts",
  "packages/bloom/src/examples/constraint-topology-experiment.ts",
];
const sourceSha256 = Object.fromEntries(
  await Promise.all(
    sourceFiles.map(async (file) => [
      file,
      createHash("sha256")
        .update(await readFile(path.join(root, file)))
        .digest("hex"),
    ]),
  ),
);
const result = {
  schemaVersion: 1,
  generatedAt: new Date().toISOString(),
  nodeVersion: process.version,
  runner:
    "scripts/elementary-topology/constraint-generalization-experiments.mjs",
  runCommand:
    "node scripts/elementary-topology/constraint-generalization-experiments.mjs",
  buildCommand:
    "yarn --cwd packages/core build && yarn --cwd packages/bloom tsc",
  sourceSha256,
  canvas: [720, 540],
  tolerance,
  stepLimit,
  styleOptions: options,
  family:
    "n pairwise-disjoint circular regions, two member points each, relation chain between first points of adjacent regions",
  geometry:
    "No per-instance coordinates, radii, or region/point hints. Style's generic index-based initial row/radius defaults plus seeded jitter remain; position/size objectives are disabled. Relative label objectives and visual constraints remain.",
  comparison:
    "Renaming changes labels only. Reversal changes every entity's declaration order while keeping logical memberships, disjoint pairs, and chain direction. Mathematical correctness is compared independently of exact layout equality.",
  summary: {
    cases: results.length,
    succeeded: results.filter((r) => r.success).length,
    failed: results.filter((r) => !r.success).length,
    failures: results.filter((r) => !r.success).map((r) => r.id),
    semanticFeasible: results.filter((r) => r.independentSemanticFeasible)
      .length,
    crossingCases: results
      .filter((r) => r.routes?.crossings.length)
      .map((r) => r.id),
  },
  composition,
  cases: results,
  limitations: [
    "One seed and three small object counts are a bounded smoke experiment, not statistical evidence of convergence or scalability.",
    "Independent rendered clearance checks cover circular region disjointness and point membership, excluding stroke. Positive facts only; unasserted membership remains unknown.",
    "Straight-edge crossings are recorded but not prohibited by this style; routes may cross region boundaries. Label/edge constraints use the library's geometry approximations.",
    "SVG files are reproducible private outputs under tmp, separate from the reviewed textbook illustrations. No canonical book SVG is regenerated.",
  ],
};
const nonfinite = (_key, value) =>
  typeof value === "number" && !Number.isFinite(value) ? String(value) : value;
await mkdir(path.dirname(output), { recursive: true });
await writeFile(output, JSON.stringify(result, nonfinite, 2) + "\n");
console.log(
  "Recorded generalization results:",
  path.relative(root, output),
  JSON.stringify(result.summary),
);
browser.window.close();
// Bloom's completed browser scheduling channels otherwise keep Node alive.
process.exit(0);

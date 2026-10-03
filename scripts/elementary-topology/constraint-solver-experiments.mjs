/**
 * Small native Penrose experiments separating feasibility, initial-state
 * sensitivity, and units. Build core and emit Bloom first. No solver settings
 * are changed; rendered geometry is checked independently of AD expressions.
 */
import { JSDOM } from "jsdom";
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { DiagramBuilder } from "../../packages/bloom/dist/core/builder.js";
import * as constraints from "../../packages/bloom/dist/core/constraints.js";
import * as objectives from "../../packages/bloom/dist/core/objectives.js";
import { canvas } from "../../packages/bloom/dist/core/utils.js";
import { div, mul } from "../../packages/core/dist/index.js";

const root = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const destination = path.join(
  root,
  "docs/elementary-topology/constraint-experiments",
);
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
await mkdir(destination, { recursive: true });

async function run({
  id,
  scale = 1,
  initial,
  normalized = false,
  normalizedVariables = false,
  prior = 0,
  limitedCanvas = false,
}) {
  // Keep the unused default shape sampler in its supported canvas size range.
  const canvasScale = Math.max(1, scale);
  const builder = new DiagramBuilder(
    canvas((limitedCanvas ? 50 : 300) * canvasScale, 180 * canvasScale),
    id,
  );
  const inputs = initial.map(([x, y], index) => [
    builder.input({
      name: `${index}.x`,
      init: x * (normalizedVariables ? 1 : scale),
    }),
    builder.input({
      name: `${index}.y`,
      init: y * (normalizedVariables ? 1 : scale),
    }),
  ]);
  const positions = inputs.map((pair) =>
    pair.map((value) => (normalizedVariables ? mul(value, scale) : value)),
  );
  const circles = positions.map((center, index) =>
    builder.circle({
      name: `disk-${index}`,
      center,
      r: 35 * scale,
      fillColor: index === 0 ? [0.13, 0.45, 0.5, 0.2] : [0.75, 0.4, 0.2, 0.2],
      strokeColor: index === 0 ? [0.13, 0.45, 0.5, 1] : [0.75, 0.4, 0.2, 1],
      strokeWidth: scale,
      // Exclude implicit canvas terms so this isolates a single semantic residual.
      ensureOnCanvas: limitedCanvas,
    }),
  );
  const separation = constraints.disjoint(circles[0], circles[1], 10 * scale);
  builder.ensure(
    normalized ? div(separation, scale) : separation,
    undefined,
    normalized ? "normalized disk clearance" : "disk clearance",
  );
  if (prior > 0)
    positions.forEach((pair, i) =>
      pair.forEach((value, axis) =>
        builder.encourage(
          objectives.equal(div(value, scale), initial[i][axis]),
          prior,
        ),
      ),
    );
  const builtAt = performance.now();
  const drawing = await builder.build();
  const buildMs = performance.now() - builtAt;
  const before = drawing.getConstraintDiagnostics();
  const initialState = Reflect.get(drawing, "state");
  const gradient = new Float64Array(initialState.varyingValues.length);
  initialState.gradient(
    initialState.constraintSets.values().next().value,
    new Float64Array(initialState.varyingValues),
    1,
    gradient,
  );
  const initialGradientNorm = Math.hypot(...gradient);
  const started = performance.now();
  let calls = 0;
  let stopped = false;
  let error = null;
  try {
    while (calls < 2500 && performance.now() - started < 15000) {
      calls++;
      if (!(await drawing.optimizationStep())) {
        stopped = true;
        break;
      }
    }
    const optimizationMs = performance.now() - started;
    const diagnostics = drawing.getConstraintDiagnostics();
    const values = positions.map((_, i) => [
      drawing.getInput(`${i}.x`),
      drawing.getInput(`${i}.y`),
    ]);
    // Read actual native SVG output as an independent circle-clearance oracle.
    const { svg } = await drawing.render();
    const geometry = Array.from(svg.querySelectorAll("circle")).map(
      (element) => ({
        x: Number(element.getAttribute("cx")),
        y: Number(element.getAttribute("cy")),
        r: Number(element.getAttribute("r")),
      }),
    );
    const clearance =
      Math.hypot(geometry[0].x - geometry[1].x, geometry[0].y - geometry[1].y) -
      geometry[0].r -
      geometry[1].r;
    const relativeViolation = Math.max(0, (10 * scale - clearance) / scale);
    const size = drawing.getCanvas();
    const canvasViolation =
      Math.max(
        0,
        ...geometry.flatMap((c) => [
          c.r - c.x,
          c.x + c.r - size.width,
          c.r - c.y,
          c.y + c.r - size.height,
        ]),
      ) / scale;
    svg.setAttribute("aria-label", id);
    const margin = 20 * scale;
    const minX = Math.min(...geometry.map((c) => c.x - c.r)) - margin;
    const minY = Math.min(...geometry.map((c) => c.y - c.r)) - margin;
    const maxX = Math.max(...geometry.map((c) => c.x + c.r)) + margin;
    const maxY = Math.max(...geometry.map((c) => c.y + c.r)) + margin;
    svg.setAttribute(
      "viewBox",
      `${minX} ${minY} ${maxX - minX} ${maxY - minY}`,
    );
    svg.setAttribute("width", "300");
    svg.setAttribute("height", "180");
    await writeFile(
      path.join(destination, `${id}.svg`),
      new XMLSerializer().serializeToString(svg),
    );
    const state = Reflect.get(drawing, "state");
    return {
      id,
      scale,
      normalized,
      normalizedVariables,
      prior,
      limitedCanvas,
      initial,
      final: values.map(([x, y]) => [
        x / (normalizedVariables ? 1 : scale),
        y / (normalizedVariables ? 1 : scale),
      ]),
      buildMs,
      optimizationMs,
      calls,
      stopped,
      status: state.params.optStatus,
      initialMaxViolation: before.maxViolation,
      initialPenaltyGradientNormAtUnitWeight: initialGradientNorm,
      initiallyFeasibleAtDefaultTolerance: before.feasible,
      optimizationFinished: diagnostics.optimizationFinished,
      feasibleAtDefaultTolerance: diagnostics.feasible,
      maxRegisteredViolation: diagnostics.maxViolation,
      independentRelativeViolation: relativeViolation,
      independentCanvasViolation: canvasViolation,
      independentFeasible:
        relativeViolation <= 0.001 &&
        (!limitedCanvas || canvasViolation <= 0.001),
      svg: `constraint-experiments/${id}.svg`,
      error,
    };
  } catch (caught) {
    error = String(caught);
    return { id, scale, normalized, initial, buildMs, calls, stopped, error };
  } finally {
    drawing.discard();
  }
}

const cases = [
  {
    id: "already-feasible",
    initial: [
      [-45, 0],
      [45, 0],
    ],
  },
  {
    id: "overlapping-distinct",
    initial: [
      [-10, 0],
      [10, 0],
    ],
  },
  {
    id: "coincident",
    initial: [
      [0, 0],
      [0, 0],
    ],
  },
  {
    id: "tiny-asymmetry",
    initial: [
      [0, 0],
      [1e-4, 0],
    ],
  },
  {
    id: "diagonal-asymmetry",
    initial: [
      [0, 0],
      [0.1, 0.2],
    ],
  },
  {
    id: "overlapping-with-prior",
    initial: [
      [-10, 0],
      [10, 0],
    ],
    prior: 0.002,
  },
  {
    id: "coincident-with-prior",
    initial: [
      [0, 0],
      [0, 0],
    ],
    prior: 0.002,
  },
  {
    id: "tiny-asymmetry-with-prior",
    initial: [
      [0, 0],
      [1e-4, 0],
    ],
    prior: 0.002,
  },
  {
    id: "representation-limited-canvas",
    initial: [
      [-45, 0],
      [45, 0],
    ],
    limitedCanvas: true,
    prior: 0.002,
  },
  ...[1e-6, 1e-4, 0.01, 1, 100].flatMap((scale) =>
    [false, true].map((normalized) => ({
      id: `scale-${scale}-${normalized ? "normalized" : "raw"}`,
      scale,
      normalized,
      initial: [
        [-10, 0],
        [10, 0],
      ],
    })),
  ),
  ...[1e-6, 1e-4, 0.01, 1, 100].flatMap((scale) =>
    [false, true].map((normalizedVariables) => ({
      id: `scale-${scale}-prior-${
        normalizedVariables ? "normalized-variables" : "raw-variables"
      }`,
      scale,
      normalized: true,
      normalizedVariables,
      prior: 0.002,
      initial: [
        [-10, 0],
        [10, 0],
      ],
    })),
  ),
  ...[false, true].map((normalizedVariables) => ({
    id: `scale-1e-9-prior-${
      normalizedVariables ? "normalized-variables" : "raw-variables"
    }`,
    scale: 1e-9,
    normalized: true,
    normalizedVariables,
    prior: 0.002,
    initial: [
      [-10, 0],
      [10, 0],
    ],
  })),
];
const results = [];
for (const experiment of cases) {
  const result = await run(experiment);
  results.push(result);
  process.stdout.write(
    `${result.id}: ${result.status ?? result.error}, independent residual ${
      result.independentRelativeViolation
    }\n`,
  );
}
await writeFile(
  path.join(destination, "solver.json"),
  JSON.stringify(
    {
      generatedAt: new Date().toISOString(),
      nodeVersion: process.version,
      gitHead: execFileSync("git", ["rev-parse", "HEAD"], {
        cwd: root,
        encoding: "utf8",
      }).trim(),
      fingerprints: Object.fromEntries(
        await Promise.all(
          [
            "scripts/elementary-topology/constraint-solver-experiments.mjs",
            "packages/core/dist/engine/AutodiffFunctions.js",
            "packages/core/dist/engine/Optimizer.js",
            "packages/bloom/dist/core/builder.js",
            "packages/bloom/dist/core/diagram.js",
          ].map(async (file) => [
            file,
            createHash("sha256")
              .update(await readFile(path.join(root, file)))
              .digest("hex"),
          ]),
        ),
      ),
      methodology:
        "Two native circles, four optimized center coordinates, fixed radii35, required clearance10, one semantic inequality; canvas constraints appear only in the representation-limited fixture. Initial positions, units, residual normalization, variable normalization, and explicitly declared weak squared stability priors vary. Native exterior-penalty solver unchanged. Independent numeric clearance checks rendered SVG circles in common reference units. SVG viewBoxes fit the final geometry; they do not indicate canvas feasibility.",
      stepLimit: 2500,
      timeLimitMs: 15000,
      results,
    },
    null,
    2,
  ) + "\n",
);
browser.window.close();
// Bloom's MessageChannel scheduler retains Node handles after diagrams discard.
process.exit(0);

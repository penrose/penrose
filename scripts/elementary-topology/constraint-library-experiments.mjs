/**
 * Reproduce the linked containment/distance bugs and outstanding geometry limits.
 * Build core first: yarn --cwd packages/core build
 * Run: node scripts/elementary-topology/constraint-library-experiments.mjs
 * Optional: --out=tmp/library-counterexamples.json
 *
 * Legacy functions reconstruct the baf76d69f formulas explicitly; they do not
 * check out old code or replace current functions. Convex partition/half-plane
 * and rectangle-distance helpers are unchanged by these fixes. Neither the
 * solver nor any textbook programs/SVGs are modified by this experiment.
 */
import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import {
  genGradient,
  ops,
  problem,
  variable,
} from "../../packages/core/dist/engine/Autodiff.js";
import {
  add,
  div,
  max,
  maxN,
  min,
  mul,
  squared,
  sub,
} from "../../packages/core/dist/engine/AutodiffFunctions.js";
import {
  bboxFromPath,
  bboxFromPoints,
} from "../../packages/core/dist/engine/BBox.js";
import {
  containsCircleRect,
  containsPolyPoint,
  containsPolys,
  onCanvas,
} from "../../packages/core/dist/lib/Constraints.js";
import {
  signedDistanceLine,
  signedDistancePolygon,
} from "../../packages/core/dist/lib/Functions.js";
import {
  containsConvexPolygonPoints,
  convexPartitions,
} from "../../packages/core/dist/lib/Minkowski.js";
import { shapeDistanceRects } from "../../packages/core/dist/lib/Queries.js";
import { numOf } from "../../packages/core/dist/lib/Utils.js";
import { makePath } from "../../packages/core/dist/shapes/Path.js";
import {
  makeCanvas,
  simpleContext,
} from "../../packages/core/dist/shapes/Samplers.js";
import { pathDataV, toScreen } from "../../packages/core/dist/utils/Util.js";

const root = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const arguments_ = process.argv.slice(2);
assert(
  arguments_.every((argument) => argument.startsWith("--out=")),
  "Only --out=<file> is supported",
);
assert(arguments_.length <= 1, "Pass at most one output path");
const destination = path.resolve(
  root,
  arguments_[0]?.slice(6) ??
    "docs/elementary-topology/constraint-experiments/library-counterexamples.json",
);
const baseline = "baf76d69f";
const feasibilityTolerance = 1e-3;
const elbow = [
  [0, 0],
  [4, 0],
  [4, 1],
  [1, 1],
  [1, 4],
  [0, 4],
];

// Old point containment intersected all convex pieces rather than testing the
// original polygon's filled region. Internal partition edges also consumed
// padding, making positive-clearance membership impossible in adjacent pieces.
const legacyContainsPolyPoint = (points, point, padding) =>
  maxN(
    convexPartitions(points).map((piece) =>
      containsConvexPolygonPoints(piece, point, padding),
    ),
  );

// Old Circle→Rectangle containment replaced the rectangle with a centered disk
// whose radius was its longest full side, and did not use the padding argument.
const legacyContainsCircleRect = (center, radius, points, _padding) => {
  const box = bboxFromPoints(points);
  const rectangleRadius = max(box.width, box.height);
  return sub(ops.vdist(center, box.center), sub(radius, rectangleRadius));
};

// Old closed-segment distance divided by zero when the two endpoints coincided.
const legacySegmentDistance = (start, end, point) => {
  const offset = ops.vsub(point, start),
    direction = ops.vsub(end, start);
  const t = max(
    0,
    min(1, div(ops.vdot(offset, direction), ops.vdot(direction, direction))),
  );
  return ops.vnorm(ops.vsub(offset, ops.vmul(t, direction)));
};

const squareCorners = (x, y, radius) => [
  [add(x, radius), add(y, radius)],
  [sub(x, radius), add(y, radius)],
  [sub(x, radius), sub(y, radius)],
  [add(x, radius), sub(y, radius)],
];
// Old generic Polygon↔Circle dispatch compared both shapes' AABBs. This is the
// unmodified rectangle distance with the explicit bounds of our zero-stroke
// L-region and radius-.2 circle; it includes the L's empty notch as solid ink.
const legacyPolygonCircleDistance = (point) =>
  shapeDistanceRects(
    [
      [4, 4],
      [0, 4],
      [0, 0],
      [4, 0],
    ],
    squareCorners(point[0], point[1], 0.2),
  );
const correctedPolygonCircleDistance = (point) =>
  sub(signedDistancePolygon(elbow, point), 0.2);

async function solveMembership(body) {
  const x = variable(3),
    y = variable(0.4);
  const residual = body(elbow, [x, y], 0.1);
  const preference = add(squared(sub(x, 3)), squared(sub(y, 0.4)));
  // Fixed-weight penalty experiment: this is intentionally not a native
  // exterior-penalty constraint. Exactly the same objective is used before/after.
  const objective = add(preference, mul(1000, squared(max(0, residual))));
  const run = (await problem({ objective })).start({}).run({});
  const position = [run.vals.get(x), run.vals.get(y)];
  const finalResidual = numOf(body(elbow, position, 0.1));
  return {
    initialResidual: numOf(residual),
    converged: run.converged,
    position,
    finalResidual,
    feasible: finalResidual <= feasibilityTolerance,
    trueBoundaryClearance: -numOf(signedDistancePolygon(elbow, position)),
  };
}

async function solveSeparation(body) {
  const x = variable(2),
    y = variable(2);
  const residual = sub(0.1, body([x, y]));
  const objective = add(squared(sub(x, 2)), squared(sub(y, 2)));
  // Native exterior-penalty experiment: current core's unmodified penalty-
  // weight schedule and L-BFGS implementation handle the registered constraint.
  const run = (await problem({ objective, constraints: [residual] }))
    .start({})
    .run({});
  const position = [run.vals.get(x), run.vals.get(y)];
  const finalResidual = numOf(sub(0.1, body(position)));
  return {
    initialResidual: numOf(residual),
    converged: run.converged,
    position,
    finalResidual,
    feasible: finalResidual <= feasibilityTolerance,
    trueCircleClearance: numOf(correctedPolygonCircleDistance(position)),
  };
}

const membership = {
  polygon: elbow,
  preferred: [3, 0.4],
  padding: 0.1,
  experiment: "fixed-weight penalty",
  weight: 1000,
  objective: "distanceSquaredToPreferred + 1000*relu(containment)^2",
  before: await solveMembership(legacyContainsPolyPoint),
  after: await solveMembership(containsPolyPoint),
};
const separation = {
  polygon: elbow,
  preferred: [2, 2],
  circleRadius: 0.2,
  padding: 0.1,
  experiment: "native exterior-penalty",
  objective:
    "distanceSquaredToPreferred, with a native exterior-penalty disjoint constraint",
  before: await solveSeparation(legacyPolygonCircleDistance),
  after: await solveSeparation(correctedPolygonCircleDistance),
};
const rectangleCorners = [
  [3, 4],
  [-3, 4],
  [-3, -4],
  [3, -4],
];
const collapsedLegacy = numOf(legacySegmentDistance([1, 2], [1, 2], [4, 6]));
const circleRectangle = {
  rectangleCorners,
  circleRadius: 5,
  beforeResidual: numOf(
    legacyContainsCircleRect([0, 0], 5, rectangleCorners, 0),
  ),
  afterResidual: numOf(containsCircleRect([0, 0], 5, rectangleCorners, 0)),
  padding: 0.25,
  beforeWithPadding: numOf(
    legacyContainsCircleRect([0, 0], 5, rectangleCorners, 0.25),
  ),
  afterWithPadding: numOf(
    containsCircleRect([0, 0], 5, rectangleCorners, 0.25),
  ),
};
const collapsedSegment = {
  start: [1, 2],
  end: [1, 2],
  point: [4, 6],
  // JSON has no NaN literal; record it explicitly rather than silently turning
  // a failed measurement into an unexplained null.
  before: {
    value: Number.isFinite(collapsedLegacy) ? collapsedLegacy : null,
    finite: Number.isFinite(collapsedLegacy),
    representation: String(collapsedLegacy),
  },
  after: {
    value: numOf(signedDistanceLine([1, 2], [1, 2], [4, 6])),
    finite: true,
  },
};

// This remains a limitation: checking contained polygon vertices alone misses
// an edge that crosses the exterior notch of a concave container.
const innerTriangle = [
  [0.5, 3],
  [3, 0.5],
  [0.5, 0.5],
];
const tinyY = variable(1e-8);
const tinyDistance = signedDistanceLine([0, 0], [1e-8, 0], [0.5e-8, tinyY]);
const gradient = new Float64Array(1);
const tinyFunction = await genGradient([tinyY], [tinyDistance], []);
const tinyResult = tinyFunction(
  { inputMask: [true], objMask: [true], constrMask: [] },
  new Float64Array([1e-8]),
  1,
  gradient,
);

// A native cubic is fully inside this canvas although its control-point hull
// extends beyond it. onCanvas currently uses that conservative hull.
const coordinate = (contents) => ({ tag: "CoordV", contents });
const commands = [
  { cmd: "M", contents: [coordinate([-1, 0])] },
  {
    cmd: "C",
    contents: [coordinate([-1, 10]), coordinate([1, -10]), coordinate([1, 0])],
  },
];
const cubic = makePath(
  simpleContext("cubic geometry limitation"),
  makeCanvas(10, 6),
  { d: pathDataV(commands) },
);
const cubicBox = bboxFromPath(cubic);
const outstanding = {
  concavePolygonContainsPolygon: {
    innerTriangle,
    vertexOnlyResidual: numOf(containsPolys(elbow, innerTriangle, 0)),
    edgeMidpoint: [1.75, 1.75],
    edgeMidpointSignedDistance: numOf(
      signedDistancePolygon(elbow, [1.75, 1.75]),
    ),
  },
  tinyScaleSqrtDerivative: {
    distance: tinyResult.phi,
    ADDerivative: gradient[0],
    mathematicalDerivative: 1,
    sqrtDerivativeFloor: 1e-5,
  },
  cubicOnCanvas: {
    controlPoints: [
      [-1, 0],
      [-1, 10],
      [1, -10],
      [1, 0],
    ],
    canvas: [10, 6],
    bboxWidth: numOf(cubicBox.width),
    bboxHeight: numOf(cubicBox.height),
    actualCurveYBounds: [-5 / Math.sqrt(3), 5 / Math.sqrt(3)],
    onCanvasResidual: numOf(onCanvas(cubic, 10, 6)),
    actualCurveFits: true,
    limitation:
      "bboxFromPath uses the Bezier control-point hull, not curve extrema",
  },
};

const canvas = [100, 80],
  vertex = [1, 1],
  scaleFactor = 2;
const scale = {
  canvas,
  vertex,
  scale: scaleFactor,
  beforeRenderedVertex: toScreen(vertex, canvas).map((x) => scaleFactor * x),
  beforeQueryVertex: vertex,
  bboxMathematicalVertex: vertex.map((x) => scaleFactor * x),
  afterRenderedVertex: toScreen(
    vertex.map((x) => scaleFactor * x),
    canvas,
  ),
  afterQueryVertex: vertex.map((x) => scaleFactor * x),
  scale0Before: "renderer coerced to1; BBox collapsed; queries unscaled",
  scale0After: "all collapse about Penrose origin",
  strokeConvention:
    "native Polygon/Polyline scale changes coordinate geometry while stroke width stays unchanged; geometry BBox and queries exclude stroke",
};

// Validate corrections independently of optimizer's convergence flag. The
// remaining failures are measurements, not expected guarantees of the library.
assert(membership.after.feasible && membership.after.converged);
assert(
  Math.hypot(
    ...membership.after.position.map((x, i) => x - membership.preferred[i]),
  ) < 1e-6,
);
assert(!membership.before.feasible);
assert(separation.after.feasible && separation.after.converged);
assert(
  Math.hypot(
    ...separation.after.position.map((x, i) => x - separation.preferred[i]),
  ) < 1e-6,
);
assert(
  Math.hypot(
    ...separation.before.position.map((x, i) => x - separation.preferred[i]),
  ) > 1,
);
assert(Math.abs(circleRectangle.afterResidual) < 1e-12);
assert(Math.abs(circleRectangle.afterWithPadding - 0.25) < 1e-12);
assert(!collapsedSegment.before.finite && collapsedSegment.after.value === 5);

const sourceFiles = [
  "lib/Constraints.ts",
  "lib/Functions.ts",
  "lib/Queries.ts",
  "renderer/AttrHelper.ts",
  "renderer/Polygon.ts",
  "renderer/Polyline.ts",
];
const sourceHashes = Object.fromEntries(
  await Promise.all(
    sourceFiles.map(async (file) => {
      const relative = `packages/core/src/${file}`;
      return [
        relative,
        createHash("sha256")
          .update(await readFile(path.join(root, relative)))
          .digest("hex"),
      ];
    }),
  ),
);
const result = {
  schemaVersion: 1,
  generatedAt: new Date().toISOString(),
  runner: "scripts/elementary-topology/constraint-library-experiments.mjs",
  runCommand:
    "node scripts/elementary-topology/constraint-library-experiments.mjs",
  buildCommand: "yarn --cwd packages/core build",
  baseline: `${baseline} (legacy formulas reconstructed; unchanged convex-partition/half-plane and rectangle-distance helpers reused)`,
  sourceHashes,
  feasibilityTolerance,
  membership,
  separation,
  circleRectangle,
  collapsedSegment,
  scale,
  outstanding,
};
await mkdir(path.dirname(destination), { recursive: true });
await writeFile(destination, JSON.stringify(result, null, 2) + "\n");
console.log(
  `Validated library counterexamples: ${path.relative(root, destination)}`,
);
console.log(
  `Membership residual: ${membership.before.finalResidual} → ${membership.after.finalResidual}`,
);
console.log(
  `Notch placement: ${separation.before.position.join(
    ", ",
  )} → ${separation.after.position.join(", ")}`,
);

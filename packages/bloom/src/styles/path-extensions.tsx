/** @jsxImportSource @penrose/bloom */

import type { PlaneCoordinates } from "../domains/point-set-topology.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

export type PlanePathSampler = (s: number) => PlaneCoordinates;
export type SquarePathSampler = (s: number, t: number) => PlaneCoordinates;

/** A continuous convex extension of two continuous paths, with optional interior deformation. */
export function interpolateBoundaryPaths(
  lower: PlanePathSampler,
  upper: PlanePathSampler,
  displacement: SquarePathSampler = () => [0, 0],
): SquarePathSampler {
  return (s, t) => {
    if (![s, t].every((n) => Number.isFinite(n) && n >= 0 && n <= 1))
      throw new Error(
        "A boundary-path extension is sampled on the closed unit square",
      );
    const a = lower(s),
      b = upper(s),
      d = displacement(s, t);
    if (![...a, ...b, ...d].every(Number.isFinite))
      throw new Error("Path extension samples must be finite");
    return [
      (1 - t) * a[0] + t * b[0] + t * (1 - t) * d[0],
      (1 - t) * a[1] + t * b[1] + t * (1 - t) * d[1],
    ];
  };
}

const bezier =
  (
    p: readonly [
      PlaneCoordinates,
      PlaneCoordinates,
      PlaneCoordinates,
      PlaneCoordinates,
    ],
  ): PlanePathSampler =>
  (s) => {
    const q = 1 - s;
    return [0, 1].map(
      (i) =>
        q ** 3 * p[0][i] +
        3 * q * q * s * p[1][i] +
        3 * q * s * s * p[2][i] +
        s ** 3 * p[3][i],
    ) as [number, number];
  };
/** The generic source curves are Style realizations, not formulas attributed to the book. */
export const illustrativeLowerPath = bezier([
  [0.0625, 0],
  [0.4, -0.08],
  [0.75, -0.04],
  [1.025, 0.075],
]);
export const illustrativeUpperPath = bezier([
  [-0.2, 0.8],
  [0, 1.14],
  [0.35, 1.05],
  [0.875, 1],
]);

/** A two-edge boundary function and its extension across the entire square. */
export function pathExtensionStyle(
  options: TopologyStyleOptions & {
    lowerPath?: PlanePathSampler;
    upperPath?: PlanePathSampler;
    interiorDisplacement?: SquarePathSampler;
  } = {},
) {
  const extension = interpolateBoundaryPaths(
    options.lowerPath ?? illustrativeLowerPath,
    options.upperPath ?? illustrativeUpperPath,
    options.interiorDisplacement ?? ((s) => [0.52 - 0.26 * s, 0]),
  );
  return topology.style((ctx) => {
    const facts = ctx.facts(topology.ExtensionOf);
    if (facts.length !== 1)
      throw new Error("The path panel needs one continuous extension");
    const [extended, original, domain, fullDomain] = facts[0];
    const square = ctx
        .entities(topology.ClosedProductRectangle)
        .find((s) => s === fullDomain),
      boundary = ctx
        .entities(topology.HorizontalBoundaryPair)
        .find((b) => b === domain);
    const targets = ctx
      .facts(topology.MapBetween)
      .filter(([f]) => f === original)
      .map(([, , p]) => p);
    const paths = ctx
      .facts(topology.BoundaryPathsOf)
      .filter(([f, , , b]) => f === original && b === boundary);
    if (
      !square ||
      !boundary ||
      square.bounds.some((p, i) => p !== [0, 0, 1, 1][i]) ||
      !ctx.test(topology.HorizontalEdgesOf, boundary, square) ||
      paths.length !== 1 ||
      targets.length !== 1 ||
      !ctx.test(topology.MapBetween, extended, square, targets[0])
    )
      throw new Error(
        "The map must extend the two horizontal edges of the closed unit square",
      );
    const topologies = ctx.facts(topology.TopologyOn);
    const tauSquare = topologies.find(([, s]) => s === square)?.[0],
      tauBoundary = topologies.find(([, s]) => s === boundary)?.[0],
      tauTarget = topologies.find(([, s]) => s === targets[0])?.[0];
    if (
      !tauSquare ||
      !tauBoundary ||
      !tauTarget ||
      !ctx.test(topology.ClosedIn, boundary, tauSquare) ||
      !ctx.test(
        topology.SubspaceTopologyOf,
        tauBoundary,
        boundary,
        tauSquare,
      ) ||
      !ctx.test(topology.ContinuousMap, original, tauBoundary, tauTarget) ||
      !ctx.test(topology.ContinuousMap, extended, tauSquare, tauTarget)
    )
      throw new Error(
        "The boundary map and extension need their continuous topology witnesses",
      );
    const draw = topologyDrawing(options);
    const rectangle: [string, ...number[]][] = [
      ["M", -160.5, -73],
      ["L", -46.5, -73],
      ["L", -46.5, 58],
      ["L", -160.5, 58],
      ["Z"],
    ];
    draw.hatchedArea("extension.unit-square", rectangle, [-161, -74, 115, 133]);
    draw.outline("extension.square-boundary", rectangle);
    draw.label("\\{(x,y)\\mid y=1\\}", [-103.5, 74]);
    draw.label("\\{(x,y)\\mid y=0\\}", [-103.5, -87]);
    draw.line("extension.arrow", [-36.5, -8], [-0.5, -8]);
    draw.line("extension.arrow-upper", [-0.5, -8], [-9.5, -3]);
    draw.line("extension.arrow-lower", [-0.5, -8], [-9.5, -13]);
    draw.line("extension.x-axis", [13.5, -50], [147.5, -50]);
    draw.line("extension.y-axis", [78.5, -74], [78.5, 76]);
    draw.label("x", [156.5, -50]);
    draw.label("y", [81.5, 86]);
    const project = ([x, y]: PlaneCoordinates): [number, number] => [
      35.5 + 80 * x,
      -24 + 80 * y,
    ];
    const boundaryPoints = [
      ...Array.from({ length: 33 }, (_, i) => extension(i / 32, 0)),
      ...Array.from({ length: 32 }, (_, i) => extension(1, (i + 1) / 32)),
      ...Array.from({ length: 32 }, (_, i) => extension(1 - (i + 1) / 32, 1)),
      ...Array.from({ length: 31 }, (_, i) => extension(0, 1 - (i + 1) / 32)),
    ].map(project);
    const contour: [string, ...number[]][] = boundaryPoints.map((p, i) => [
      i ? "L" : "M",
      ...p,
    ]);
    contour.push(["Z"]);
    const minX = Math.min(...boundaryPoints.map(([x]) => x)),
      minY = Math.min(...boundaryPoints.map(([, y]) => y)),
      maxX = Math.max(...boundaryPoints.map(([x]) => x)),
      maxY = Math.max(...boundaryPoints.map(([, y]) => y));
    draw.hatchedArea("extension.path-image", contour, [
      minX,
      minY,
      maxX - minX,
      maxY - minY,
    ]);
    draw.outline("extension.image-boundary", contour);
  });
}

/** @jsxImportSource @penrose/bloom */

import type { PathData, Vec2 } from "../core/types.js";
import { metricSpaces } from "../domains/metric-spaces.js";
import { HatchedMetricBall, METRIC_INK } from "./metric-primitives.js";

type Point = readonly [number, number];

/** Source-shaped blobs belong to the visual program, never to a Substance object. */
const blobPath = (
  points: readonly Point[],
  origin: Point,
  center: Point,
): PathData => {
  const at = (point: Point) => ({
    tag: "CoordV" as const,
    contents: [
      center[0] + point[0] - origin[0],
      center[1] - point[1] + origin[1],
    ],
  });
  const commands: PathData = [{ cmd: "M", contents: [at(points[0])] }];
  for (let i = 1; i < points.length; i += 3)
    commands.push({ cmd: "C", contents: points.slice(i, i + 3).map(at) });
  commands.push({ cmd: "Z", contents: [] });
  return commands;
};
const LEFT_BLOB: readonly Point[] = [
  [135, 243],
  [134, 290],
  [173, 301],
  [215, 312],
  [280, 336],
  [323, 311],
  [346, 278],
  [370, 242],
  [378, 215],
  [362, 168],
  [350, 130],
  [307, 101],
  [282, 114],
  [265, 117],
  [267, 128],
  [260, 145],
  [243, 164],
  [212, 166],
  [182, 187],
  [151, 203],
  [134, 210],
  [135, 243],
];
const RIGHT_BLOB: readonly Point[] = [
  [422, 254],
  [419, 282],
  [447, 289],
  [478, 297],
  [504, 300],
  [518, 307],
  [543, 314],
  [586, 324],
  [626, 304],
  [650, 281],
  [668, 255],
  [684, 202],
  [665, 161],
  [650, 123],
  [610, 108],
  [577, 113],
  [556, 113],
  [564, 135],
  [535, 131],
  [511, 127],
  [500, 135],
  [497, 158],
  [493, 190],
  [463, 193],
  [447, 209],
  [427, 222],
  [421, 235],
  [422, 254],
];

/** Figure 2.15: a chosen source neighborhood is mapped inside the target one. */
export function metricContinuityStyle(
  options: {
    fontSize?: string;
  } = {},
) {
  // Each abstract metric space has its own schematic display scale.
  const sourceRadius = 60,
    targetRadius = 51;
  return metricSpaces.style((ctx) => {
    const inclusions = ctx.facts(metricSpaces.MapsNeighborhoodInto);
    if (inclusions.length !== 1)
      throw new Error(
        "The continuity panel needs one neighborhood-image inclusion",
      );
    const [map, sourceBall, targetBall] = inclusions[0];
    const maps = ctx
      .facts(metricSpaces.MapBetweenSpaces)
      .filter(([f]) => f === map);
    const centers = ctx.facts(metricSpaces.SpaceNeighborhoodAt);
    const sourcePoints = centers
      .filter(([ball]) => ball === sourceBall)
      .map(([, point]) => point);
    const targetPoints = centers
      .filter(([ball]) => ball === targetBall)
      .map(([, point]) => point);
    if (
      maps.length !== 1 ||
      sourcePoints.length !== 1 ||
      targetPoints.length !== 1
    )
      throw new Error(
        "The map and neighborhoods need unique spaces and centers",
      );
    const [, source, target] = maps[0],
      a = sourcePoints[0],
      fa = targetPoints[0];
    if (
      !(sourceBall.rho > 0) ||
      !(targetBall.rho > 0) ||
      ![sourceBall.rho, targetBall.rho].every(Number.isFinite)
    )
      throw new Error(
        "Selected neighborhood radii must be finite and positive",
      );
    if (
      !ctx.test(metricSpaces.SpaceNeighborhoodInSpace, sourceBall, source) ||
      !ctx.test(metricSpaces.SpaceNeighborhoodInSpace, targetBall, target) ||
      !ctx.test(metricSpaces.SpacePointInSpace, a, source) ||
      !ctx.test(metricSpaces.SpacePointInSpace, fa, target) ||
      !ctx.test(metricSpaces.MapsPoint, map, a, fa) ||
      !ctx.test(metricSpaces.ContinuousAt, map, a)
    )
      throw new Error(
        "The selected neighborhoods must witness continuity at the mapped center",
      );
    const witnesses = ctx
      .facts(metricSpaces.MapsPoint)
      .filter(
        ([f, x, fx]) =>
          f === map &&
          x !== a &&
          ctx.test(metricSpaces.InsideSpaceNeighborhood, x, sourceBall) &&
          ctx.test(metricSpaces.InsideSpaceNeighborhood, fx, targetBall),
      );
    if (witnesses.length !== 1)
      throw new Error(
        "The panel needs one source point and its image inside the chosen neighborhoods",
      );
    const [, x, fx] = witnesses[0];
    if (
      !ctx.test(metricSpaces.SpacePointInSpace, x, source) ||
      !ctx.test(metricSpaces.SpacePointInSpace, fx, target)
    )
      throw new Error(
        "Witness points must belong to the map's source and target",
      );
    const left: Point = [-145, 2],
      right: Point = [145, 2];
    <path
      name="continuity.source-space"
      d={blobPath(LEFT_BLOB, [250, 230], left)}
      fill-color={[0, 0, 0, 0]}
      stroke-color={METRIC_INK}
      stroke-width={1.1}
    />;
    <path
      name="continuity.target-space"
      d={blobPath(RIGHT_BLOB, [550, 230], right)}
      fill-color={[0, 0, 0, 0]}
      stroke-color={METRIC_INK}
      stroke-width={1.1}
    />;
    const aPos: [number, number] = [-150, -14],
      faPos: [number, number] = [171, -3],
      xPos: [number, number] = [-167, 17],
      fxPos: [number, number] = [144, -6];
    <HatchedMetricBall
      name="continuity.source-neighborhood"
      center={aPos}
      r={sourceRadius}
    />;
    <HatchedMetricBall
      name="continuity.target-neighborhood"
      center={faPos}
      r={targetRadius}
    />;
    <line
      name="continuity.q"
      start={aPos}
      end={[aPos[0] + sourceRadius * 0.95, aPos[1] + sourceRadius * 0.3]}
      stroke-color={METRIC_INK}
      stroke-width={0.75}
    />;
    <line
      name="continuity.rho"
      start={faPos}
      end={[faPos[0] + targetRadius * 0.85, faPos[1] + targetRadius * 0.52]}
      stroke-color={METRIC_INK}
      stroke-width={0.75}
    />;
    for (const point of [aPos, faPos, xPos, fxPos])
      <circle center={point} r={2} fill-color={METRIC_INK} stroke-width={0} />;
    const label = (text: string, center: Vec2) => (
      <equation
        center={center}
        font-size={options.fontSize ?? "14px"}
        fill-color={METRIC_INK}
      >
        {text}
      </equation>
    );
    label(source.label, [-145, -113]);
    label(target.label, [157, -113]);
    label(a.label, [-150, -26]);
    label(x.label, [-167, 5]);
    label(fa.label, [180, -17]);
    label(fx.label, [145, 12]);
    label(sourceBall.radiusLabel ?? "q", [-113, -10]);
    label(targetBall.radiusLabel ?? "\\rho", [208, 6]);
  });
}

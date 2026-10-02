/** @jsxImportSource @penrose/bloom */

import type { Vec2 } from "../core/types.js";
import {
  metricSpaces,
  separatedLimitRadius,
} from "../domains/metric-spaces.js";
import { HatchedMetricBall, METRIC_INK } from "./metric-primitives.js";

/** Figure 2.14 depicts distinct proposed limits in a proof by contradiction. */
export function limitUniquenessStyle(
  options: { unit?: number; fontSize?: string } = {},
) {
  const unit = options.unit ?? 78;
  if (!(unit > 0) || !Number.isFinite(unit))
    throw new Error("A finite positive scale is required");
  return metricSpaces.style((ctx) => {
    const disjoint = ctx.facts(metricSpaces.DisjointNeighborhoods);
    if (disjoint.length !== 1)
      throw new Error("The uniqueness panel needs two disjoint neighborhoods");
    const [leftBall, rightBall] = disjoint[0];
    const at = ctx.facts(metricSpaces.NeighborhoodAt);
    const left = at
      .filter(([ball]) => ball === leftBall)
      .map(([, point]) => point);
    const right = at
      .filter(([ball]) => ball === rightBall)
      .map(([, point]) => point);
    const spaces = ctx
      .facts(metricSpaces.InSpace)
      .filter(([ball]) => ball === leftBall)
      .map(([, space]) => space);
    if (
      left.length !== 1 ||
      right.length !== 1 ||
      spaces.length !== 1 ||
      spaces[0].metric !== "euclidean" ||
      !ctx.test(metricSpaces.InSpace, rightBall, spaces[0])
    )
      throw new Error(
        "The circular proof panel needs two centers in one Euclidean realization",
      );
    const yPrime = left[0],
      y = right[0],
      space = spaces[0];
    if (
      !ctx.test(metricSpaces.DistinctMetricPoints, yPrime, y) &&
      !ctx.test(metricSpaces.DistinctMetricPoints, y, yPrime)
    )
      throw new Error("The two proposed limits must be declared distinct");
    if (
      leftBall.point.some((v, i) => v !== yPrime.point[i]) ||
      rightBall.point.some((v, i) => v !== y.point[i])
    )
      throw new Error(
        "The neighborhoods must be centered at their proposed limits",
      );
    const candidates = ctx.facts(metricSpaces.ProposedLimitOf);
    const sequence = candidates.find(
      ([, point, s]) => point === y && s === space,
    )?.[0];
    if (
      !sequence ||
      !ctx.test(metricSpaces.ProposedLimitOf, sequence, yPrime, space) ||
      !ctx.test(metricSpaces.SequenceInPlane, sequence, space)
    )
      throw new Error(
        "One sequence must have both proposed limits in this proof hypothesis",
      );
    if (
      ctx.test(metricSpaces.ConvergesTo, sequence, y, space) &&
      ctx.test(metricSpaces.ConvergesTo, sequence, yPrime, space)
    )
      throw new Error(
        "Distinct proposed limits are hypotheses, not two actual convergence facts",
      );
    const rho = separatedLimitRadius("euclidean", y.point, yPrime.point);
    if (
      ![leftBall.rho, rightBall.rho].every(
        (r) => Number.isFinite(r) && Math.abs(r - rho) <= rho * 1e-12,
      )
    )
      throw new Error(
        "Both radii must be half the distance between the proposed limits",
      );
    const midpoint: [number, number] = [
      y.point[0] / 2 + yPrime.point[0] / 2,
      y.point[1] / 2 + yPrime.point[1] / 2,
    ];
    const project = (point: readonly [number, number]): [number, number] => [
      (point[0] - midpoint[0]) * unit,
      (point[1] - midpoint[1]) * unit,
    ];
    const l = project(yPrime.point),
      r = project(y.point),
      radius = rho * unit;
    <HatchedMetricBall name="uniqueness.left" center={l} r={radius} />;
    <HatchedMetricBall name="uniqueness.right" center={r} r={radius} />;
    <line
      name="uniqueness.separation"
      start={l}
      end={r}
      stroke-color={METRIC_INK}
      stroke-width={0.8}
      stroke-dasharray="3 3"
    />;
    const label = (text: string, center: Vec2) => (
      <equation
        center={center}
        font-size={options.fontSize ?? "14px"}
        fill-color={METRIC_INK}
      >
        {text}
      </equation>
    );
    // The contact marker is a boundary location, not a member of either open ball.
    for (const point of [l, r, [0, 0] as [number, number]])
      <circle center={point} r={2} stroke-width={0} fill-color={METRIC_INK} />;
    label(yPrime.label, [l[0] + 8, l[1] - 9]);
    label(y.label, [r[0] + 9, r[1] + 4]);
    label(`N(${yPrime.label},\\rho)`, [l[0] + radius * 0.8, l[1] - radius - 9]);
    label(`N(${y.label},\\rho)`, [r[0] + radius * 0.72, r[1] - radius - 9]);
    label(`\\tfrac12 D(${y.label},${yPrime.label})`, [
      r[0] * 0.75,
      r[1] * 0.5 - 16,
    ]);
    const points: [number, number][] = [
      [-0.26 * unit, 1.1 * unit],
      [-0.43 * unit, 0.72 * unit],
      [-0.57 * unit, 0.46 * unit],
      [-0.65 * unit, 0.28 * unit],
      [-0.74 * unit, 0.09 * unit],
      [-0.83 * unit, -0.06 * unit],
    ];
    for (const point of points)
      <circle
        center={point}
        r={1.9}
        fill-color={METRIC_INK}
        stroke-width={0}
      />;
    <polyline
      name="uniqueness.sequence-guide"
      points={[points[2], points[3], points[4], points[5], l]}
      fill-color={[0, 0, 0, 0]}
      stroke-color={METRIC_INK}
      stroke-width={0.65}
      stroke-dasharray="3 4"
    />;
    label("s_1", [points[0][0] + 12, points[0][1]]);
    label("s_2", [points[1][0] + 12, points[1][1]]);
    label("s_n", [points[4][0] + 12, points[4][1] + 3]);
  });
}

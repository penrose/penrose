/** @jsxImportSource @penrose/bloom */

import type { Vec2 } from "../core/types.js";
import { metricSpaces } from "../domains/metric-spaces.js";

export interface MetricComparisonStyleOptions {
  unit?: number;
  fontSize?: string;
  regionColor?: [number, number, number, number];
}

const INK: [number, number, number, number] = [0.08, 0.08, 0.08, 1];
const AXIS: [number, number, number, number] = [0.25, 0.25, 0.25, 1];

/**
 * Draw Euclidean ⊆ taxicab ⊆ Euclidean neighborhood facts at a common center.
 * Layout is translated to place that center at the canvas origin.
 *
 * Figure 2.5, printed page 20: the inner radius is ρ/√2, the diamond's
 * vertices have distance ρ along the axes, and the outer circle has radius ρ.
 * The printed axis legends are retained in their original positions.
 */
export function metricComparisonStyle(
  options: MetricComparisonStyleOptions = {},
) {
  const unit = options.unit ?? 76;
  const fontSize = options.fontSize ?? "14px";
  const color: [number, number, number, number] = options.regionColor ?? [
    0.95, 0.41, 0.12, 0.09,
  ];
  if (!(unit > 0) || !Number.isFinite(unit)) {
    throw new Error("Metric comparison requires a finite positive unit scale");
  }

  return metricSpaces.style((ctx) => {
    const pairs = ctx.facts(metricSpaces.NeighborhoodContainedIn);
    const chains = pairs.flatMap(([inner, middle]) =>
      pairs
        .filter(([next]) => next === middle)
        .map(([, outer]) => ({ inner, middle, outer })),
    );
    if (chains.length !== 1) {
      throw new Error(
        "Metric comparison requires one chain of three neighborhoods",
      );
    }
    const { inner, middle, outer } = chains[0];
    const membership = ctx.facts(metricSpaces.InSpace);
    const metric = (ball: typeof inner) => {
      const spaces = membership
        .filter(([member]) => member === ball)
        .map(([, space]) => space);
      if (spaces.length !== 1)
        throw new Error("Each neighborhood needs exactly one metric space");
      return spaces[0].metric;
    };
    if (
      metric(inner) !== "euclidean" ||
      metric(middle) !== "taxicab" ||
      metric(outer) !== "euclidean"
    ) {
      throw new Error(
        "Metric comparison expects Euclidean, taxicab, Euclidean neighborhoods",
      );
    }
    for (const ball of [inner, middle, outer]) {
      if (
        !(ball.rho > 0) ||
        !Number.isFinite(ball.rho) ||
        !ball.point.every(Number.isFinite)
      ) {
        throw new Error(
          "Neighborhood centers must be finite and radii positive",
        );
      }
      if (ball.point.some((coordinate, i) => coordinate !== outer.point[i])) {
        throw new Error("Metric comparison neighborhoods must be concentric");
      }
    }
    if (
      inner.rho > (middle.rho / Math.SQRT2) * (1 + 4 * Number.EPSILON) ||
      middle.rho > outer.rho * (1 + 4 * Number.EPSILON)
    ) {
      throw new Error("Asserted metric neighborhood containment is false");
    }

    const r = outer.rho * unit;
    const ri = inner.rho * unit;
    const rd = middle.rho * unit;
    const extent = 1.48 * r;
    if (![r, ri, rd, extent].every(Number.isFinite)) {
      throw new Error("Scaled neighborhood geometry must remain finite");
    }
    const center: Vec2 = [0, 0];
    <circle
      name="comparison.outer"
      center={center}
      r={r}
      fill-color={[0, 0, 0, 0]}
      stroke-color={INK}
      stroke-width={0.9}
    />;
    <circle
      name="comparison.inner"
      center={center}
      r={ri}
      fill-color={color}
      stroke-color={INK}
      stroke-width={0.9}
    />;
    <polygon
      name="comparison.taxicab"
      points={[
        [0, rd],
        [rd, 0],
        [0, -rd],
        [-rd, 0],
      ]}
      fill-color={[0, 0, 0, 0]}
      stroke-color={INK}
      stroke-width={0.9}
      stroke-dasharray="5 4"
    />;
    <line
      name="comparison.axis-x"
      start={[-extent, 0]}
      end={[extent, 0]}
      stroke-color={AXIS}
      stroke-width={0.8}
    />;
    <line
      name="comparison.axis-y"
      start={[0, -extent]}
      end={[0, extent]}
      stroke-color={AXIS}
      stroke-width={0.8}
    />;
    <line
      name="comparison.radius"
      start={center}
      end={[ri / Math.SQRT2, ri / Math.SQRT2]}
      stroke-color={INK}
      stroke-width={0.8}
      stroke-dasharray="5 3"
    />;
    for (const point of [
      [0, rd],
      [rd, 0],
      [0, -rd],
      [-rd, 0],
    ] as Vec2[]) {
      <circle center={point} r={2.1} fill-color={INK} stroke-width={0} />;
    }

    const [x, y] = outer.coordinateNames ?? outer.point.map(String);
    const rho = outer.radiusLabel ?? "\\rho";
    const label = (text: string, point: Vec2, name?: string) => (
      <equation
        name={name}
        center={point}
        font-size={fontSize}
        fill-color={INK}
      >
        {text}
      </equation>
    );
    label(`(${x},${y})`, [24, -12]);
    label(`(${x},${y}+${rho})`, [30, rd + 12]);
    label(`(${x}+${rho},${y})`, [rd + 42, 12]);
    label(`(${x},${y}-${rho})`, [32, -rd - 14]);
    label(`(${x}-${rho},${y})`, [-rd - 42, 12]);
    label(
      inner.radiusLabel ?? `${rho}/\\sqrt{2}`,
      [ri * 0.58, ri * 0.18],
      "comparison.radius-label",
    );
    label(`x=${x}`, [extent - 9, -12]);
    label(`y=${y}`, [0, extent + 10]);
  });
}

/** @jsxImportSource @penrose/bloom */

import type { Vec2 } from "../core/types.js";
import { metricSpaces, planeDistance } from "../domains/metric-spaces.js";
import {
  HatchedMetricBall,
  METRIC_INK,
  MetricBrace,
} from "./metric-primitives.js";

/** Figure 2.8: the triangle inequality gives N(w, ρ−D(x,w)) ⊆ N(x,ρ). */
export function neighborhoodInclusionStyle(
  options: { unit?: number; fontSize?: string } = {},
) {
  const unit = options.unit ?? 144;
  if (!(unit > 0) || !Number.isFinite(unit))
    throw new Error("A positive finite unit scale is required");
  return metricSpaces.style((ctx) => {
    const inclusions = ctx.facts(metricSpaces.NeighborhoodContainedIn);
    if (inclusions.length !== 1)
      throw new Error("The inclusion panel needs one neighborhood containment");
    const [inner, outer] = inclusions[0];
    const spaces = ctx.facts(metricSpaces.InSpace);
    const outerSpaces = spaces
      .filter(([ball]) => ball === outer)
      .map(([, space]) => space);
    if (
      outerSpaces.length !== 1 ||
      outerSpaces[0].metric !== "euclidean" ||
      !ctx.test(metricSpaces.InSpace, inner, outerSpaces[0])
    )
      throw new Error(
        "The circular inclusion style requires a shared Euclidean space",
      );
    const at = ctx.facts(metricSpaces.NeighborhoodAt);
    const x = at.find(([ball]) => ball === outer)?.[1];
    const w = at.find(([ball]) => ball === inner)?.[1];
    if (
      !x ||
      !w ||
      x.point.some((v, i) => v !== outer.point[i]) ||
      w.point.some((v, i) => v !== inner.point[i])
    )
      throw new Error("Each neighborhood needs its declared center point");
    const distance = planeDistance("euclidean", outer.point, inner.point);
    if (
      !(inner.rho > 0) ||
      !(outer.rho > 0) ||
      ![...x.point, ...w.point, inner.rho, outer.rho].every(Number.isFinite) ||
      Math.abs(distance + inner.rho - outer.rho) > outer.rho * 1e-12
    )
      throw new Error("The inner radius must equal ρ−D(x,w)");
    if (!ctx.test(metricSpaces.InsideNeighborhood, w, outer))
      throw new Error(
        "The small center must be declared inside the outer neighborhood",
      );
    const center: [number, number] = [15, 8];
    const small: [number, number] = [
      center[0] + (w.point[0] - x.point[0]) * unit,
      center[1] + (w.point[1] - x.point[1]) * unit,
    ];
    const radius = outer.rho * unit;
    <HatchedMetricBall name="inclusion.outer" center={center} r={radius} />;
    <HatchedMetricBall
      name="inclusion.inner"
      center={small}
      r={inner.rho * unit}
      opacity={0.16}
      crossHatch
    />;
    const boundary: [number, number] = [
      center[0] + radius * Math.cos(0.31),
      center[1] + radius * Math.sin(0.31),
    ];
    <line
      name="inclusion.distance"
      start={small}
      end={center}
      stroke-color={METRIC_INK}
      stroke-width={0.9}
    />;
    <line
      name="inclusion.radius"
      start={center}
      end={boundary}
      stroke-color={METRIC_INK}
      stroke-width={0.9}
    />;
    <MetricBrace name="inclusion.distance-brace" start={small} end={center} />;
    <MetricBrace name="inclusion.radius-brace" start={center} end={boundary} />;
    for (const p of [center, small])
      <circle center={p} r={2.2} fill-color={METRIC_INK} stroke-width={0} />;
    const label = (s: string, p: Vec2) => (
      <equation
        center={p}
        font-size={options.fontSize ?? "14px"}
        fill-color={METRIC_INK}
      >
        {s}
      </equation>
    );
    label(x.label, [center[0] + 8, center[1] - 8]);
    label(w.label, [small[0] + 7, small[1] - 7]);
    label(`D(${x.label},${w.label})`, [
      (small[0] + center[0]) / 2 - 20,
      (small[1] + center[1]) / 2 + 14,
    ]);
    label("\\rho", [center[0] + radius * 0.47, center[1] + radius * 0.31]);
    label(`N(${x.label},\\rho)`, [
      center[0] + radius * 0.8,
      center[1] - radius - 11,
    ]);
    const end: [number, number] = [
      small[0] - inner.rho * unit - 13,
      small[1] - 23,
    ];
    <polyline
      name="inclusion.q-leader"
      points={[
        [small[0] - inner.rho * unit, small[1]],
        [end[0], small[1]],
        end,
      ]}
      fill-color={[0, 0, 0, 0]}
      stroke-color={METRIC_INK}
      stroke-width={0.7}
    />;
    label(`q=\\rho-D(${x.label},${w.label})`, [end[0] + 10, end[1] - 10]);
  });
}

/** @jsxImportSource @penrose/bloom */

import type { Vec2 } from "../core/types.js";
import {
  intervalComplementRadius,
  metricSpaces,
} from "../domains/metric-spaces.js";
import { METRIC_AXIS, METRIC_INK, MetricBrace } from "./metric-primitives.js";

/** Figure 2.9: a neighborhood in the complement proves that [0,1] is closed. */
export function closedIntervalComplementStyle(
  options: { unit?: number; fontSize?: string } = {},
) {
  const unit = options.unit ?? 94;
  if (!(unit > 0) || !Number.isFinite(unit))
    throw new Error("A positive finite unit scale is required");
  return metricSpaces.style((ctx) => {
    const avoids = ctx.facts(metricSpaces.LineNeighborhoodAvoidsInterval);
    if (avoids.length !== 1)
      throw new Error(
        "The interval panel requires one complement neighborhood",
      );
    const [ball, interval] = avoids[0];
    const [a, b] = interval.bounds;
    const point = ctx
      .facts(metricSpaces.LineNeighborhoodAt)
      .find(([n]) => n === ball)?.[1];
    const line = ctx
      .facts(metricSpaces.IntervalInLine)
      .find(([i]) => i === interval)?.[1];
    if (
      !point ||
      !line ||
      line.metric !== "absolute" ||
      !ctx.test(metricSpaces.LinePointInSpace, point, line) ||
      !ctx.test(metricSpaces.LineNeighborhoodInSpace, ball, line) ||
      !ctx.test(metricSpaces.OutsideInterval, point, interval) ||
      point.position !== ball.center
    )
      throw new Error(
        "The point, interval, and neighborhood must share the absolute-value line",
      );
    const rho = intervalComplementRadius(point.position, interval.bounds);
    if (!(ball.rho > 0) || Math.abs(ball.rho - rho) > rho * 1e-12)
      throw new Error(
        "The neighborhood radius must be the distance to the closed interval",
      );
    const left = Math.min(a, ball.center - ball.rho),
      right = Math.max(b, ball.center + ball.rho);
    const project = (x: number) => (x - (left + right) / 2) * unit;
    const lo = project(a),
      hi = project(b),
      x = project(point.position);
    const openLo = project(ball.center - ball.rho),
      openHi = project(ball.center + ball.rho);
    <rect
      name="interval.closed"
      center={[(lo + hi) / 2, 0]}
      width={hi - lo}
      height={12}
      fill-color={[0.95, 0.41, 0.12, 0.17]}
      stroke-width={0}
    />;
    <rect
      name="interval.open"
      center={[x, 0]}
      width={openHi - openLo}
      height={12}
      fill-color={[0.95, 0.41, 0.12, 0.06]}
      stroke-width={0}
    />;
    for (const [start, end, opacity] of [
      [lo, hi, 0.45],
      [openLo, openHi, 0.25],
    ]) {
      for (let p = start; p < end; p += 4) {
        <line
          start={[p, -6]}
          end={[Math.min(p + 9, end), 6]}
          stroke-color={[0.25, 0.25, 0.25, opacity]}
          stroke-width={0.6}
        />;
      }
    }
    <line
      name="interval.axis"
      start={[project(left) - 16, 0]}
      end={[project(right) + 16, 0]}
      stroke-color={METRIC_AXIS}
      stroke-width={0.8}
    />;
    <polyline
      name="interval.left-closed"
      points={[
        [lo + 5, 8],
        [lo, 8],
        [lo, -8],
        [lo + 5, -8],
      ]}
      fill-color={[0, 0, 0, 0]}
      stroke-color={METRIC_INK}
      stroke-width={0.85}
    />;
    <polyline
      name="interval.right-closed"
      points={[
        [hi - 5, 8],
        [hi, 8],
        [hi, -8],
        [hi - 5, -8],
      ]}
      fill-color={[0, 0, 0, 0]}
      stroke-color={METRIC_INK}
      stroke-width={0.85}
    />;
    <circle center={[x, 0]} r={2.1} fill-color={METRIC_INK} stroke-width={0} />;
    const label = (
      text: string,
      center: Vec2,
      size = options.fontSize ?? "14px",
    ) => (
      <equation center={center} font-size={size} fill-color={METRIC_INK}>
        {text}
      </equation>
    );
    label("(", [openLo + 1, 0], "24px");
    label(")", [openHi - 1, 0], "24px");
    label(point.label, [x, 15]);
    label(String(a), [lo, 15]);
    label(String(b), [hi, 15]);
    const nearest = point.position < a ? lo : hi;
    <MetricBrace
      name="interval.radius-brace"
      start={[Math.min(x, nearest), 0]}
      end={[Math.max(x, nearest), 0]}
      offset={-11}
      depth={-4}
    />;
    const radius =
      a === 0 && b === 1
        ? `\\min(|${point.label}|,|1-${point.label}|)`
        : `\\min(|${point.label}-${a}|,|${b}-${point.label}|)`;
    label(radius, [(x + nearest) / 2, -34]);
  });
}

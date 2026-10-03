/** @jsxImportSource @penrose/bloom */

import type { Vec2 } from "../core/types.js";
import { metricSpaces } from "../domains/metric-spaces.js";
import { METRIC_AXIS, METRIC_INK } from "./metric-primitives.js";

/** Figure 2.16: a finite view of the unbounded open strip f⁻¹(N(a,ρ)). */
export function projectionPreimageStyle(
  options: { unit?: number; clipHeight?: number; fontSize?: string } = {},
) {
  const unit = options.unit ?? 65,
    clipHeight = options.clipHeight ?? 202;
  if (![unit, clipHeight].every((n) => Number.isFinite(n) && n > 0))
    throw new Error(
      "Projection style needs a finite scale and visual clip height",
    );
  return metricSpaces.style((ctx) => {
    const preimages = ctx.facts(metricSpaces.ProjectionInverseImage);
    if (preimages.length !== 1)
      throw new Error(
        "The projection panel needs one neighborhood inverse image",
      );
    const [strip, map, ball] = preimages[0];
    const spaces = ctx
      .facts(metricSpaces.ProjectionBetween)
      .filter(([f]) => f === map);
    if (spaces.length !== 1 || map.coordinate !== 0)
      throw new Error(
        "The vertical strip requires the first coordinate projection",
      );
    const [, plane, line] = spaces[0];
    if (
      plane.metric !== "euclidean" ||
      line.metric !== "absolute" ||
      !ctx.test(metricSpaces.StripInPlane, strip, plane) ||
      !ctx.test(metricSpaces.LineNeighborhoodInSpace, ball, line)
    )
      throw new Error(
        "Projection source and target must have their stated metrics",
      );
    const centers = ctx
      .facts(metricSpaces.LineNeighborhoodAt)
      .filter(([n]) => n === ball)
      .map(([, p]) => p);
    if (
      centers.length !== 1 ||
      centers[0].position !== ball.center ||
      !ctx.test(metricSpaces.LinePointInSpace, centers[0], line)
    )
      throw new Error(
        "The target neighborhood needs its center in the real line",
      );
    if (
      ![strip.center, strip.halfWidth, ball.center, ball.rho].every(
        Number.isFinite,
      ) ||
      !(ball.rho > 0) ||
      strip.center !== ball.center ||
      strip.halfWidth !== ball.rho
    )
      throw new Error(
        "The inverse strip must have the target neighborhood's center and radius",
      );
    const origin: [number, number] = [-118, 0];
    const left = origin[0] + (strip.center - strip.halfWidth) * unit,
      right = origin[0] + (strip.center + strip.halfWidth) * unit;
    if (![left, right, right - left].every(Number.isFinite) || !(right > left))
      throw new Error(
        "Scaled projection geometry must remain finite and nondegenerate",
      );
    <rect
      name="projection.strip-clip"
      center={[(left + right) / 2, 0]}
      width={right - left}
      height={clipHeight}
      fill-color={[0.95, 0.41, 0.12, 0.12]}
      stroke-width={0}
    />;
    // Clip the hatches analytically; these horizontal limits are a viewport, not a bounded subset.
    const halfHeight = clipHeight / 2;
    for (
      let diagonal = left - halfHeight;
      diagonal < right + halfHeight;
      diagonal += 4
    ) {
      const low = Math.max(-halfHeight, left - diagonal),
        high = Math.min(halfHeight, right - diagonal);
      if (low < high)
        <line
          start={[diagonal + low, low]}
          end={[diagonal + high, high]}
          stroke-color={[0.25, 0.25, 0.25, 0.4]}
          stroke-width={0.6}
        />;
    }
    <line
      name="projection.axis-x"
      start={[origin[0] - 43, 0]}
      end={[right + 69, 0]}
      stroke-color={METRIC_AXIS}
      stroke-width={0.8}
    />;
    <line
      name="projection.axis-y"
      start={[origin[0], -halfHeight]}
      end={[origin[0], halfHeight + 8]}
      stroke-color={METRIC_AXIS}
      stroke-width={0.8}
    />;
    for (const x of [left, right])
      <circle
        center={[x, 0]}
        r={1.9}
        fill-color={METRIC_INK}
        stroke-width={0}
      />;
    const label = (text: string, center: Vec2) => (
      <equation
        center={center}
        font-size={options.fontSize ?? "13px"}
        fill-color={METRIC_INK}
      >
        {text}
      </equation>
    );
    const a = centers[0].label,
      rho = "\\rho";
    label("y", [origin[0], halfHeight + 17]);
    label("x", [right + 77, 2]);
    label(`(${a}-${rho},0)`, [left - 31, -13]);
    label(`(${a}+${rho},0)`, [right + 33, -13]);
    <rect
      name="projection.label-backing"
      center={[(left + right) / 2, 19]}
      width={108}
      height={20}
      fill-color={[1, 1, 1, 1]}
      stroke-width={0}
    />;
    label(`${map.label}^{-1}(N(${a},${rho}))`, [(left + right) / 2, 19]);
  });
}

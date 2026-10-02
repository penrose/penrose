/** @jsxImportSource @penrose/bloom */

import type { Path } from "../core/types.js";
import {
  reciprocalSpiralPoint,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

export function sampleReciprocalSpiral(
  limitRadius: number,
  coefficient: number,
  endParameter = 8 * Math.PI,
  step = Math.PI / 40,
): readonly (readonly [number, number])[] {
  const lower = coefficient / limitRadius,
    start = lower + Math.max(lower * 1e-5, 1e-6);
  reciprocalSpiralPoint(start, limitRadius, coefficient);
  if (
    ![endParameter, step].every(Number.isFinite) ||
    !(endParameter > start && step > 0 && step <= Math.PI / 4)
  )
    throw new Error(
      "A spiral view needs a finite parameter window and resolved angular steps",
    );
  const n = Math.ceil((endParameter - start) / step);
  if (n > 20000)
    throw new Error("Spiral sampling exceeds the finite viewport budget");
  return Array.from({ length: n + 1 }, (_, i) =>
    reciprocalSpiralPoint(
      start + ((endParameter - start) * i) / n,
      limitRadius,
      coefficient,
    ),
  );
}
/** A strictly increasing radial display chart preserving the origin and limit circle.
 * The default fourth-power odds chart reproduces the book's schematic coil spacing.
 * An explicit power selects a power chart; power 1 is the metric view.
 */
export function spiralDisplayRadius(
  radius: number,
  limitRadius: number,
  power?: number,
): number {
  if (
    ![radius, limitRadius].every(Number.isFinite) ||
    !(limitRadius > 0) ||
    (power !== undefined && (!Number.isFinite(power) || !(power > 0))) ||
    radius < 0 ||
    radius > limitRadius
  )
    throw new Error(
      "A spiral display radius must lie between zero and a positive limit radius",
    );
  if (radius === 0 || radius === limitRadius) return radius;
  const r = radius / limitRadius;
  const displayed =
    power === undefined ? 1 / (1 + 5000 * ((1 - r) / r) ** 4) : r ** power;
  // Keep omitted endpoints omitted when an asymptotic display rounds to one.
  return limitRadius * Math.min(displayed, 1 - Number.EPSILON);
}
/** A connected spiral and its limiting circular boundary; the omitted initial origin remains a closure point. */
export function spiralClosureStyle(
  options: TopologyStyleOptions & {
    endParameter?: number;
    radialPower?: number;
  } = {},
) {
  return topology.style((ctx) => {
    const curves = ctx.entities(topology.ReciprocalPolarSpiral);
    if (curves.length !== 1)
      throw new Error(
        "A spiral closure view requires one reciprocal polar curve",
      );
    const curve = curves[0],
      circle = ctx
        .facts(topology.SpiralLimitCircleOf)
        .find(([, s]) => s === curve)?.[0];
    const initial = ctx
      .facts(topology.SpiralInitialLimitOf)
      .find(([, s]) => s === curve)?.[0];
    const closure = ctx.facts(topology.ClosureOf).find(([, s]) => s === curve);
    if (
      !circle ||
      !initial ||
      !closure ||
      !ctx.test(topology.ConnectedIn, curve, closure[2]) ||
      !ctx.test(topology.Member, initial, closure[0]) ||
      !ctx.test(topology.Outside, initial, curve)
    )
      throw new Error(
        "Keep both the circular and initial endpoint limits in the true closure",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "12px",
    });
    const center: [number, number] = [0, -0.5],
      unit = 104 / curve.limitRadius;
    <circle
      name="closure.spiral-limit-circle"
      center={draw.xy(center)}
      r={104 * draw.scale}
      fill-color={[0, 0, 0, 0]}
      stroke-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={1.4}
      stroke-dasharray="5 4"
    />;
    const commands: [string, ...number[]][] = sampleReciprocalSpiral(
      curve.limitRadius,
      curve.coefficient,
      options.endParameter ?? 10 * Math.PI,
    ).map(([x, y], i) => {
      const radius = Math.hypot(x, y),
        display = spiralDisplayRadius(
          radius,
          curve.limitRadius,
          options.radialPower,
        ),
        factor = display / radius;
      return [
        i ? "L" : "M",
        center[0] + unit * x * factor,
        center[1] + unit * y * factor,
      ];
    });
    (
      <path
        name="closure.reciprocal-spiral"
        d={draw.data(commands)}
        fill-color={[0, 0, 0, 0]}
        stroke-color={[0.08, 0.08, 0.08, 1]}
        stroke-width={1.4}
        ensure-on-canvas={false}
      />
    ) as Path;
    draw.label("0", [8, -2]);
  });
}

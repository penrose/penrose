/** @jsxImportSource @penrose/bloom */

import type { Vec2 } from "../core/types.js";
import { metricSpaces, powerSequenceTerm } from "../domains/metric-spaces.js";
import { METRIC_ACCENT, METRIC_AXIS, METRIC_INK } from "./metric-primitives.js";

export interface FunctionSequenceStyleOptions {
  size?: number;
  samples?: number;
  fontSize?: string;
}

/** The family y=x^n. Exponents and sequence membership belong to the substance. */
export function powerFunctionSequenceStyle(
  options: FunctionSequenceStyleOptions = {},
) {
  const size = options.size ?? 204,
    samples = options.samples ?? 144;
  if (
    !(size > 0) ||
    !Number.isFinite(size) ||
    !Number.isSafeInteger(samples) ||
    samples < 16
  )
    throw new Error(
      "A function graph needs finite positive size and at least sixteen samples",
    );
  return metricSpaces.style((ctx) => {
    const sequences = ctx.entities(metricSpaces.FunctionSequence);
    if (sequences.length !== 1)
      throw new Error("A function sequence panel needs exactly one sequence");
    const spaces = ctx
      .facts(metricSpaces.FunctionSequenceInSpace)
      .filter(([sequence]) => sequence === sequences[0]);
    if (
      spaces.length !== 1 ||
      spaces[0][1].domain.some((v, i) => v !== i) ||
      spaces[0][1].codomain.some((v, i) => v !== i)
    )
      throw new Error(
        "The power sequence panel represents functions [0,1] into [0,1]",
      );
    const terms = ctx
      .facts(metricSpaces.FunctionTerm)
      .filter(([, sequence]) => sequence === sequences[0])
      .map(([term]) => term);
    if (!terms.length)
      throw new Error("A function sequence graph needs displayed terms");
    const point = (x: number, y: number): [number, number] => [
      (x - 0.5) * size,
      (y - 0.5) * size,
    ];
    const label = (text: string, position: Vec2) => (
      <equation
        center={position}
        font-size={options.fontSize ?? "14px"}
        fill-color={METRIC_INK}
      >
        {text}
      </equation>
    );
    <rect
      center={[0, 0]}
      width={size}
      height={size}
      fill-color={[0, 0, 0, 0]}
      stroke-color={METRIC_AXIS}
      stroke-width={0.75}
    />;
    for (const term of terms) {
      const points = Array.from({ length: samples + 1 }, (_, i) => {
        const x = i / samples;
        return point(x, powerSequenceTerm(term.exponent, x));
      });
      <polyline
        name={`power.term-${term.exponent}`}
        points={points}
        fill-color={[0, 0, 0, 0]}
        stroke-color={METRIC_INK}
        stroke-width={1.05}
      />;
      if (term.exponent <= 3) {
        const x = term.exponent === 1 ? 0.5 : term.exponent === 2 ? 0.57 : 0.74;
        const position = point(x, powerSequenceTerm(term.exponent, x));
        if (term.exponent === 3) {
          <line
            start={point(0.75, powerSequenceTerm(3, 0.75))}
            end={point(0.87, 0.27)}
            stroke-color={METRIC_INK}
            stroke-width={0.65}
          />;
        }
        label(
          term.exponent === 1 ? "y=x" : `y=x^{${term.exponent}}`,
          term.exponent === 3
            ? point(0.9, 0.21)
            : [position[0] - 10, position[1] + 13],
        );
      }
    }
    label("(0,0)", [-size / 2 - 3, -size / 2 - 12]);
    label("(0,1)", [-size / 2 - 7, size / 2 + 10]);
    label("(1,0)", [size / 2, -size / 2 - 12]);
  });
}

/** A uniform collar of the endpoint limit, illustrating why x^n escapes it. */
export function powerSequenceLimitCollarStyle(
  options: FunctionSequenceStyleOptions = {},
) {
  const size = options.size ?? 204,
    samples = options.samples ?? 144;
  if (
    !(size > 0) ||
    !Number.isFinite(size) ||
    !Number.isSafeInteger(samples) ||
    samples < 16
  )
    throw new Error(
      "A function graph needs finite positive size and at least sixteen samples",
    );
  return metricSpaces.style((ctx) => {
    const limits = ctx.entities(metricSpaces.EndpointLimitFunction);
    const terms = ctx.entities(metricSpaces.PowerFunction);
    if (limits.length !== 1 || terms.length !== 1)
      throw new Error(
        "The limit collar needs one endpoint limit and one representative sequence term",
      );
    const collars = ctx
      .facts(metricSpaces.FunctionNeighborhoodOf)
      .filter(([, f]) => f === limits[0]);
    if (collars.length !== 1)
      throw new Error(
        "A single uniform collar must surround the endpoint limit",
      );
    const rho = collars[0][0].rho;
    if (!(rho > 0 && rho < 0.5) || !Number.isFinite(rho))
      throw new Error("The endpoint collar illustration requires 0 < ρ < 1/2");
    const spaces = ctx
      .facts(metricSpaces.FunctionInSpace)
      .filter(([f]) => f === limits[0]);
    if (
      spaces.length !== 1 ||
      spaces[0][1].domain.some((v, i) => v !== i) ||
      spaces[0][1].codomain.some((v, i) => v !== i)
    )
      throw new Error("The endpoint limit has domain and codomain [0,1]");
    const fact = ctx
      .facts(metricSpaces.FailsToConvergeUniformlyTo)
      .find(([, f, space]) => f === limits[0] && space === spaces[0][1]);
    if (
      !fact ||
      !ctx.test(metricSpaces.FunctionTerm, terms[0], fact[0]) ||
      !ctx.test(
        metricSpaces.PointwiseConvergesTo,
        fact[0],
        limits[0],
        spaces[0][1],
      )
    )
      throw new Error(
        "The diagram needs the pointwise convergence and failed uniform convergence facts",
      );
    const point = (x: number, y: number): [number, number] => [
      (x - 0.5) * size,
      (y - 0.5) * size,
    ];
    const label = (text: string, position: Vec2) => (
      <equation
        center={position}
        font-size={options.fontSize ?? "14px"}
        fill-color={METRIC_INK}
      >
        {text}
      </equation>
    );
    <rect
      name="limit.lower-collar"
      center={point(0.5, rho / 2)}
      width={size}
      height={rho * size}
      fill-color={METRIC_ACCENT}
      stroke-width={0}
    />;
    // Diagonal hatching is clipped analytically to the displayed collar rectangle.
    for (let k = -rho * size; k < size; k += 4) {
      const lo = Math.max(0, -k),
        hi = Math.min(rho * size, size - k);
      if (hi > lo)
        <line
          start={point((k + lo) / size, lo / size)}
          end={point((k + hi) / size, hi / size)}
          stroke-color={[0.25, 0.25, 0.25, 0.35]}
          stroke-width={0.6}
        />;
    }
    <line
      start={point(0, 0)}
      end={point(1.03, 0)}
      stroke-color={METRIC_AXIS}
      stroke-width={0.75}
    />;
    <line
      start={point(0, 0)}
      end={point(0, 1.03)}
      stroke-color={METRIC_AXIS}
      stroke-width={0.75}
    />;
    const curve = Array.from({ length: samples + 1 }, (_, i) =>
      point(i / samples, powerSequenceTerm(terms[0].exponent, i / samples)),
    );
    <polyline
      name="limit.sequence-term"
      points={curve}
      fill-color={[0, 0, 0, 0]}
      stroke-color={METRIC_INK}
      stroke-width={1.1}
    />;
    for (const y of [0, rho, 1 - rho, 1])
      <circle
        center={point(1, y)}
        r={1.8}
        fill-color={METRIC_INK}
        stroke-width={0}
      />;
    <line
      start={point(1, 1)}
      end={point(1, 1 - rho)}
      end-arrowhead="line"
      end-arrowhead-size={0.7}
      stroke-color={METRIC_INK}
      stroke-width={0.8}
    />;
    label("(0,0)", [-size / 2 - 4, -size / 2 - 12]);
    label("(0,1)", [-size / 2 - 9, size / 2 + 9]);
    label("(1,0)", [size / 2 + 2, -size / 2 - 12]);
    label("(1,1)", [size / 2 - 9, size / 2 + 12]);
    const notation = collars[0][0].radiusLabel ?? "\\rho";
    label(`(1,${notation})`, [size / 2 + 7, (rho - 0.5) * size + 10]);
    label(notation === "\\frac13" ? "(1,\\frac23)" : `(1,1-${notation})`, [
      size / 2 + 7,
      (0.5 - rho) * size - 13,
    ]);
    label("y=x^n", [-10, -4]);
  });
}

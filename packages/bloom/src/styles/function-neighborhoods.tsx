/** @jsxImportSource @penrose/bloom */

import type { Vec2 } from "../core/types.js";
import { metricSpaces } from "../domains/metric-spaces.js";

export interface FunctionNeighborhoodStyleOptions {
  /** Illustrative graph only; mathematical functions remain substance objects. */
  sample?: (argument: number) => number;
  width?: number;
  height?: number;
  samples?: number;
  markAt?: number;
  argumentLabel?: string;
  fontSize?: string;
  regionColor?: [number, number, number, number];
}

const INK: [number, number, number, number] = [0.08, 0.08, 0.08, 1];
const AXIS: [number, number, number, number] = [0.25, 0.25, 0.25, 1];

/** A representative smooth graph with the same broad rise and fall as Figure 2.6. */
export const exampleFunctionGraph = (x: number): number =>
  0.19 + 0.26 * Math.exp(-9 * (x - 0.45) ** 2);

/**
 * Draw a vertical ±ρ collar for the uniform metric, as in printed Figure 2.6.
 * The boundary curves are not themselves required to lie in the codomain:
 * the neighborhood consists of functions into that codomain with sup distance
 * strictly less than ρ. A finite drawing illustrates this condition; it does not
 * compute the supremum of an arbitrary supplied mathematical function.
 */
export function functionNeighborhoodStyle(
  options: FunctionNeighborhoodStyleOptions = {},
) {
  const width = options.width ?? 232;
  const height = options.height ?? 232;
  const samples = options.samples ?? 96;
  const sample = options.sample ?? exampleFunctionGraph;
  const fontSize = options.fontSize ?? "13px";
  const color: [number, number, number, number] = options.regionColor ?? [
    0.95, 0.41, 0.12, 0.16,
  ];
  if (
    ![width, height].every((n) => n > 0 && Number.isFinite(n)) ||
    !Number.isInteger(samples) ||
    samples < 8
  ) {
    throw new Error(
      "Function neighborhood needs positive dimensions and at least eight samples",
    );
  }

  return metricSpaces.style((ctx) => {
    const facts = ctx.facts(metricSpaces.FunctionNeighborhoodOf);
    if (facts.length !== 1)
      throw new Error(
        "Function collar requires exactly one function neighborhood",
      );
    const [ball, fn] = facts[0];
    const spaces = ctx
      .facts(metricSpaces.FunctionInSpace)
      .filter(([f]) => f === fn)
      .map(([, space]) => space);
    if (spaces.length !== 1)
      throw new Error("The function must belong to exactly one function space");
    const space = spaces[0];
    if (space.metric !== "uniform") {
      throw new Error("A vertical function collar requires the uniform metric");
    }
    const [a, b] = space.domain;
    const [c, d] = space.codomain;
    if (
      ![a, b, c, d].every(Number.isFinite) ||
      !(a < b) ||
      !(c < d) ||
      !(ball.rho > 0) ||
      !Number.isFinite(ball.rho)
    ) {
      throw new Error(
        "Function intervals must increase and the radius must be finite and positive",
      );
    }
    if (!Number.isFinite((ball.rho / (d - c)) * height)) {
      throw new Error("Scaled function collar geometry must remain finite");
    }
    const mark = options.markAt ?? a + (b - a) / 4;
    if (!Number.isFinite(mark) || mark < a || mark > b) {
      throw new Error("The marked argument must lie in the function domain");
    }
    const origin: [number, number] = [-width / 2, -height / 2 + 6];
    const point = (x: number, y: number): Vec2 => [
      origin[0] + ((x - a) / (b - a)) * width,
      origin[1] + ((y - c) / (d - c)) * height,
    ];
    const evaluate = (x: number) => {
      const y = sample(x);
      if (!Number.isFinite(y) || y < c || y > d)
        throw new Error(
          "The illustrative function graph must remain in its codomain",
        );
      return y;
    };
    const xs = Array.from(
      { length: samples + 1 },
      (_, i) => a + ((b - a) * i) / samples,
    );
    const ys = xs.map(evaluate);
    const graph = xs.map((x, i) => point(x, ys[i]));
    const upper = xs.map((x, i) => point(x, ys[i] + ball.rho));
    const lower = xs.map((x, i) => point(x, ys[i] - ball.rho));
    <polygon
      name="function.collar"
      points={[...upper, ...lower.slice().reverse()]}
      fill-color={color}
      stroke-width={0}
    />;
    for (let i = 1; i < samples; i++) {
      <line
        name={`function.hatch-${i}`}
        start={upper[i - 1]}
        end={lower[i + 1]}
        stroke-color={[0.25, 0.25, 0.25, 0.35]}
        stroke-width={0.55}
      />;
    }
    <polyline
      name="function.upper"
      points={upper}
      fill-color={[0, 0, 0, 0]}
      stroke-color={AXIS}
      stroke-width={0.65}
    />;
    <polyline
      name="function.lower"
      points={lower}
      fill-color={[0, 0, 0, 0]}
      stroke-color={AXIS}
      stroke-width={0.65}
    />;
    <polyline
      name="function.graph"
      points={graph}
      fill-color={[0, 0, 0, 0]}
      stroke-color={INK}
      stroke-width={1.25}
    />;
    <line
      name="function.axis-x"
      start={origin}
      end={[origin[0] + width + 9, origin[1]]}
      stroke-color={AXIS}
      stroke-width={0.8}
    />;
    <line
      name="function.axis-y"
      start={origin}
      end={[origin[0], origin[1] + height + 9]}
      stroke-color={AXIS}
      stroke-width={0.8}
    />;

    const label = (text: string, center: Vec2, name?: string) => (
      <equation
        name={name}
        center={center}
        font-size={fontSize}
        fill-color={INK}
      >
        {text}
      </equation>
    );
    const endpoints: [number, number][] = [
      [a, c],
      [a, d],
      [b, c],
    ];
    for (const [x, y] of endpoints) {
      if (x !== a || y !== c) {
        <circle
          center={point(x, y)}
          r={2.1}
          fill-color={INK}
          stroke-width={0}
        />;
      }
      const [px, py] = point(x, y) as [number, number];
      label(`(${x},${y})`, [
        px + (x === b ? -2 : -16),
        py + (y === d ? 0 : -14),
      ]);
    }

    const value = evaluate(mark);
    const [mx, my] = point(mark, value) as [number, number];
    const collarPixels = (ball.rho / (d - c)) * height;
    const x = options.argumentLabel ?? "x";
    const f = fn.label;
    const rho = ball.radiusLabel ?? "\\rho";
    const leaders = [
      {
        offset: collarPixels,
        text: `(${x},${f}(${x})+${rho})`,
        end: [mx + 25, my + collarPixels + 25] as [number, number],
        labelOffset: 11,
        name: "function.upper-label",
      },
      {
        offset: 0,
        text: `(${x},${f}(${x}))`,
        end: [mx + 34, my - collarPixels - 18] as [number, number],
        labelOffset: -3,
        name: "function.graph-label",
      },
      {
        offset: -collarPixels,
        text: `(${x},${f}(${x})-${rho})`,
        end: [mx + 28, my - collarPixels - 41] as [number, number],
        labelOffset: -8,
        name: "function.lower-label",
      },
    ];
    for (const leader of leaders) {
      <circle
        center={[mx, my + leader.offset]}
        r={2.1}
        fill-color={INK}
        stroke-width={0}
      />;
      <line
        start={[mx + 2, my + leader.offset]}
        end={leader.end}
        stroke-color={INK}
        stroke-width={0.7}
      />;
      label(
        leader.text,
        [leader.end[0] + 37, leader.end[1] + leader.labelOffset],
        leader.name,
      );
    }
  });
}

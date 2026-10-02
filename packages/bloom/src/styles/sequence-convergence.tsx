/** @jsxImportSource @penrose/bloom */

import type { EntityOf, ProgramStyleContext } from "../core/program.js";
import type { Vec2 } from "../core/types.js";
import {
  inNeighborhood,
  metricSpaces,
  reciprocalHeightTerm,
} from "../domains/metric-spaces.js";
import {
  HatchedMetricBall,
  METRIC_AXIS,
  METRIC_INK,
} from "./metric-primitives.js";

type Context = ProgramStyleContext<typeof metricSpaces.definitions>;
type Tail = EntityOf<typeof metricSpaces.SequenceTail>;
type Ball = EntityOf<typeof metricSpaces.Neighborhood>;
type Sequence = EntityOf<typeof metricSpaces.PointSequence>;

const sequenceFor = (ctx: Context, tail: Tail): Sequence => {
  if (!Number.isSafeInteger(tail.after) || tail.after < 1)
    throw new Error("A sequence tail needs a positive integer cutoff");
  const sequences = ctx
    .facts(metricSpaces.TailOfSequence)
    .filter(([t]) => t === tail)
    .map(([, s]) => s);
  if (sequences.length !== 1)
    throw new Error("A tail must belong to exactly one sequence");
  return sequences[0];
};

const verifyBall = (ctx: Context, sequence: Sequence, ball: Ball) => {
  const center = ctx
    .facts(metricSpaces.NeighborhoodAt)
    .find(([b]) => b === ball)?.[1];
  const spaces = ctx
    .facts(metricSpaces.InSpace)
    .filter(([b]) => b === ball)
    .map(([, space]) => space);
  if (
    !center ||
    spaces.length !== 1 ||
    center.point.some((v, i) => v !== ball.point[i]) ||
    !ctx.test(metricSpaces.ConvergesTo, sequence, center, spaces[0]) ||
    !ctx.test(metricSpaces.SequenceInPlane, sequence, spaces[0])
  )
    throw new Error(
      "The sequence must converge to this neighborhood's center in its metric space",
    );
  if (!(ball.rho > 0) || ![ball.rho, ...ball.point].every(Number.isFinite))
    throw new Error("Neighborhood data must be finite with positive radius");
  return { center, space: spaces[0] };
};

/** A representative convergent sequence for a schematic figure without a formula. */
export function schematicConvergentTerm(
  n: number,
  cutoff: number,
): readonly [number, number] {
  const prefix: readonly (readonly [number, number])[] = [
    [0.93, 0.83],
    [-1.23, 0.12],
    [-0.73, 1.18],
    [1.35, 0.08],
    [-0.1, 0.44],
    [-0.22, 0.26],
  ];
  if (n <= cutoff)
    return n === cutoff ? [-0.94, -0.7] : prefix[(n - 1) % prefix.length];
  const k = n - cutoff;
  if (k === 1) return [-0.52, 0.45];
  const radius = 0.6 * Math.exp(-0.23 * (k - 1));
  const angle = 0.2 + 1.1 * (k - 2);
  return [radius * Math.cos(angle), radius * Math.sin(angle)];
}

/** Figure 2.10: a finite visual witness for the declared eventual-membership fact. */
export function sequenceConvergenceStyle(
  options: {
    unit?: number;
    count?: number;
    fontSize?: string;
    sampleTerm?: (index: number) => readonly [number, number];
  } = {},
) {
  const unit = options.unit ?? 76;
  if (!(unit > 0) || !Number.isFinite(unit))
    throw new Error("A positive finite unit scale is required");
  return metricSpaces.style((ctx) => {
    const membership = ctx.facts(metricSpaces.TailInNeighborhood);
    if (membership.length !== 1)
      throw new Error("A sequence-tail panel needs one neighborhood");
    const [tail, ball] = membership[0];
    const sequence = sequenceFor(ctx, tail);
    const { center, space } = verifyBall(ctx, sequence, ball);
    if (space.metric !== "euclidean")
      throw new Error(
        "The circular schematic requires a Euclidean neighborhood",
      );
    const count = options.count ?? Math.max(tail.after + 16, 20);
    if (!Number.isSafeInteger(count) || count <= tail.after)
      throw new Error(
        "The drawn prefix must extend beyond the sequence cutoff",
      );
    const position = (n: number): readonly [number, number] => {
      if (options.sampleTerm) return options.sampleTerm(n);
      const [x, y] = schematicConvergentTerm(n, tail.after);
      return [ball.point[0] + x * ball.rho, ball.point[1] + y * ball.rho];
    };
    const projected = (p: readonly [number, number]): [number, number] => [
      (p[0] - ball.point[0]) * unit,
      (p[1] - ball.point[1]) * unit,
    ];
    <HatchedMetricBall
      name="sequence.neighborhood"
      center={[0, 0]}
      r={ball.rho * unit}
    />;
    const drawn: [number, number][] = [];
    for (let n = 1; n <= count; n++) {
      const p = position(n);
      if (
        !p.every(Number.isFinite) ||
        (n > tail.after &&
          !inNeighborhood("euclidean", ball.point, ball.rho, p))
      )
        throw new Error(
          "Drawn tail terms must lie in the declared open neighborhood",
        );
      const coordinate = projected(p);
      drawn.push(coordinate);
      <circle
        name={`sequence.term-${n}`}
        center={coordinate}
        r={1.8}
        fill-color={METRIC_INK}
        stroke-width={0}
      />;
    }
    <circle
      name="sequence.limit"
      center={[0, 0]}
      r={1.8}
      fill-color={METRIC_INK}
      stroke-width={0}
    />;
    const label = (text: string, p: Vec2) => (
      <equation
        center={p}
        font-size={options.fontSize ?? "14px"}
        fill-color={METRIC_INK}
      >
        {text}
      </equation>
    );
    label(center.label, [-2, -11]);
    label("s_1", [drawn[0][0] + 10, drawn[0][1]]);
    label("s_m", [drawn[tail.after - 1][0] + 2, drawn[tail.after - 1][1] - 11]);
    label("s_{m+1}", [drawn[tail.after][0] + 2, drawn[tail.after][1] - 11]);
    label(`N(${center.label},\\rho)`, [
      ball.rho * unit * 0.85,
      -ball.rho * unit - 5,
    ]);
  });
}

/** Figure 2.11: the same sequence and limit in three equivalent plane metrics. */
export function sequenceMetricComparisonStyle(
  options: {
    unit?: number;
    count?: number;
    fontSize?: string;
    sampleTerm?: (index: number) => readonly [number, number];
  } = {},
) {
  const unit = options.unit ?? 112;
  const count = options.count ?? 24;
  if (
    !(unit > 0) ||
    !Number.isFinite(unit) ||
    !Number.isSafeInteger(count) ||
    count < 3
  )
    throw new Error(
      "The comparison requires finite scale and at least three terms",
    );
  return metricSpaces.style((ctx) => {
    const membership = ctx.facts(metricSpaces.TailInNeighborhood);
    if (membership.length !== 3)
      throw new Error(
        "The sequence comparison needs three metric neighborhoods",
      );
    const tail = membership[0][0];
    const sequence = sequenceFor(ctx, tail);
    const neighborhoods = membership.map(([t, b]) => {
      if (t !== tail)
        throw new Error(
          "The three neighborhoods must contain the same sequence tail",
        );
      return { ball: b, ...verifyBall(ctx, sequence, b) };
    });
    const metrics = new Set(neighborhoods.map(({ space }) => space.metric));
    if (
      metrics.size !== 3 ||
      !metrics.has("euclidean") ||
      !metrics.has("taxicab") ||
      !metrics.has("supremum")
    )
      throw new Error("Compare Euclidean, taxicab, and supremum metrics");
    const { ball, center } = neighborhoods[0];
    if (
      neighborhoods.some(
        ({ ball: b }) =>
          b.rho !== ball.rho || b.point.some((p, i) => p !== ball.point[i]),
      )
    )
      throw new Error("Comparison neighborhoods must share center and radius");
    const origin: [number, number] = [-94, -26];
    const project = (p: readonly [number, number]): [number, number] => [
      origin[0] + p[0] * unit,
      origin[1] + p[1] * unit,
    ];
    const point = project(ball.point);
    for (const metric of ["supremum", "euclidean", "taxicab"] as const) {
      <HatchedMetricBall
        name={`sequence.${metric}`}
        metric={metric}
        center={point}
        r={ball.rho * unit}
        opacity={metric === "supremum" ? 0.06 : 0.1}
        crossHatch={metric === "taxicab"}
      />;
    }
    <line
      name="sequence.axis-x"
      start={[origin[0] - 15, origin[1]]}
      end={[origin[0] + 1.85 * unit, origin[1]]}
      stroke-color={METRIC_AXIS}
      stroke-width={0.8}
    />;
    <line
      name="sequence.axis-y"
      start={[origin[0], origin[1] - 0.7 * unit]}
      end={[origin[0], origin[1] + 1.2 * unit]}
      stroke-color={METRIC_AXIS}
      stroke-width={0.8}
    />;
    const terms = [];
    for (let n = 1; n <= count; n++) {
      const p = (options.sampleTerm ?? reciprocalHeightTerm)(n);
      if (
        !p.every(Number.isFinite) ||
        neighborhoods.some(
          ({ ball: b, space }) =>
            n > tail.after && !inNeighborhood(space.metric, b.point, b.rho, p),
        )
      )
        throw new Error("Drawn terms contradict the declared sequence tail");
      const projected = project(p);
      terms.push(projected);
      <circle
        name={`sequence.term-${n}`}
        center={projected}
        r={1.8}
        fill-color={METRIC_INK}
        stroke-width={0}
      />;
    }
    <circle
      name="sequence.limit"
      center={point}
      r={1.8}
      fill-color={METRIC_INK}
      stroke-width={0}
    />;
    const label = (text: string, p: Vec2) => (
      <equation
        center={p}
        font-size={options.fontSize ?? "14px"}
        fill-color={METRIC_INK}
      >
        {text}
      </equation>
    );
    label("x", [origin[0] + 1.9 * unit, origin[1] + 3]);
    label("y", [origin[0], origin[1] + 1.25 * unit]);
    label("(0,0)", [origin[0] + 19, origin[1] - 11]);
    label(center.label, [point[0] + 13, point[1] - 11]);
    for (let n = 1; n <= 3; n++)
      label(`s_${n}`, [terms[n - 1][0] + 12, terms[n - 1][1]]);
  });
}

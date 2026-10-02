import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  metricSpaces,
  reciprocalConvergenceThreshold,
  sequenceConvergenceStyle,
  sequenceMetricComparisonStyle,
} from "@penrose/bloom";

/** Figure 2.10 expresses eventual membership; its unspecified sequence stays symbolic. */
export function convergentSequenceNeighborhood(
  options: { rho?: number; cutoff?: number } = {},
) {
  const rho = options.rho ?? 1,
    cutoff = options.cutoff ?? 6;
  if (
    !(rho > 0) ||
    !Number.isFinite(rho) ||
    !Number.isSafeInteger(cutoff) ||
    cutoff < 1
  )
    throw new Error(
      "A positive radius and positive integer cutoff are required",
    );
  const sub = metricSpaces.substance();
  const plane = sub.MetricPlane({ label: "(X,D)", metric: "euclidean" });
  const y = sub.MetricPoint({ label: "y", point: [0, 0] });
  const sequence = sub.PointSequence({ label: "S=\\{s_n\\}" });
  const tail = sub.SequenceTail({ label: "m", after: cutoff });
  const ball = sub.Neighborhood({ label: "N(y,\\rho)", point: y.point, rho });
  sub.PointInSpace(y, plane);
  sub.InSpace(ball, plane);
  sub.NeighborhoodAt(ball, y);
  sub.SequenceInPlane(sequence, plane);
  sub.ConvergesTo(sequence, y, plane);
  sub.TailOfSequence(tail, sequence);
  sub.TailInNeighborhood(tail, ball);
  return sub.make();
}

/** Figure 2.11: s_n=(1,1/n) converges in D, D1 and D3; the discrete metric is excluded. */
export function reciprocalSequenceNeighborhoods(
  options: { rho?: number } = {},
) {
  const rho = options.rho ?? 0.45;
  const sub = metricSpaces.substance();
  const y = sub.MetricPoint({ label: "(1,0)", point: [1, 0] });
  const sequence = sub.PointSequence({
    label: "S=\\{s_n\\}",
    formula: "s_n=(1,1/n)",
  });
  const tail = sub.SequenceTail({
    label: "M",
    after: reciprocalConvergenceThreshold(rho),
  });
  sub.TailOfSequence(tail, sequence);
  for (const [metric, notation] of [
    ["euclidean", "D"],
    ["taxicab", "D_1"],
    ["supremum", "D_3"],
  ] as const) {
    const plane = sub.MetricPlane({
      label: `(\\mathbb{R}^2,${notation})`,
      metric,
    });
    const ball = sub.Neighborhood({
      label: `N_${notation}((1,0),\\rho)`,
      point: y.point,
      rho,
    });
    sub.PointInSpace(y, plane);
    sub.InSpace(ball, plane);
    sub.NeighborhoodAt(ball, y);
    sub.SequenceInPlane(sequence, plane);
    sub.ConvergesTo(sequence, y, plane);
    sub.TailInNeighborhood(tail, ball);
  }
  return sub.make();
}

export const buildSequenceConvergenceFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: convergentSequenceNeighborhood(),
    sty: sequenceConvergenceStyle({ unit: 104 }),
    canvas: canvas(300, 270),
    variation: "gemignani-generic-convergent-sequence",
    ...renderOptions,
  });

export const buildSequenceMetricComparisonFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: reciprocalSequenceNeighborhoods(),
    sty: sequenceMetricComparisonStyle(),
    canvas: canvas(320, 300),
    variation: "gemignani-reciprocal-height-sequence",
    ...renderOptions,
  });

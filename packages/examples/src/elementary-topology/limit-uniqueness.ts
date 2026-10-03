import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  limitUniquenessStyle,
  metricSpaces,
  separatedLimitRadius,
} from "@penrose/bloom";

/** Figure 2.14 encodes the contradiction hypotheses, not two actual limits. */
export function distinctProposedLimitNeighborhoods(
  options: {
    first?: readonly [number, number];
    second?: readonly [number, number];
  } = {},
) {
  const first = options.first ?? [-1, -0.4],
    second = options.second ?? [1, 0.4];
  const rho = separatedLimitRadius("euclidean", first, second);
  const sub = metricSpaces.substance();
  const plane = sub.MetricPlane({ label: "(X,D)", metric: "euclidean" });
  const sequence = sub.PointSequence({ label: "S=\\{s_n\\}" });
  const yPrime = sub.MetricPoint({ label: "y'", point: first }),
    y = sub.MetricPoint({ label: "y", point: second });
  const left = sub.Neighborhood({ label: "N(y',\\rho)", point: first, rho }),
    right = sub.Neighborhood({ label: "N(y,\\rho)", point: second, rho });
  sub.SequenceInPlane(sequence, plane);
  for (const [point, ball] of [
    [yPrime, left],
    [y, right],
  ] as const) {
    sub.PointInSpace(point, plane);
    sub.InSpace(ball, plane);
    sub.NeighborhoodAt(ball, point);
    sub.ProposedLimitOf(sequence, point, plane);
  }
  sub.DistinctMetricPoints(yPrime, y);
  sub.DisjointNeighborhoods(left, right);
  return sub.make();
}

export const buildLimitUniquenessFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: distinctProposedLimitNeighborhoods(),
    sty: limitUniquenessStyle(),
    canvas: canvas(370, 285),
    variation: "gemignani-unique-sequence-limit",
    ...renderOptions,
  });

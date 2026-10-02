import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  finiteBallLebesgueWitness,
  halfBallContaining,
  lebesgueNumberStyle,
  pointSetTopology as topology,
  type IntervalBall,
} from "@penrose/bloom";

export interface CompactIntervalCoverOptions {
  readonly interval?: readonly [number, number];
  readonly balls?: readonly IntervalBall[];
  readonly x?: number;
  readonly z?: number;
}
/** A mathematical finite-subcover witness; no drawing data is stored in Substance. */
export function compactIntervalLebesgueCover(
  options: CompactIntervalCoverOptions = {},
) {
  const interval = options.interval ?? [0, 1],
    balls = options.balls ?? [
      { center: 0, radius: 0.5 },
      { center: 0.33, radius: 0.44 },
      { center: 0.66, radius: 0.5 },
      { center: 1, radius: 0.46 },
    ];
  const rho = finiteBallLebesgueWitness(interval, balls),
    xValue = options.x ?? interval[0] + 0.54 * (interval[1] - interval[0]);
  const j = halfBallContaining(interval, balls, xValue),
    zValue = options.z ?? Math.min(interval[1], xValue + 0.7 * rho);
  if (
    !Number.isFinite(zValue) ||
    zValue < interval[0] ||
    zValue > interval[1] ||
    !(Math.abs(zValue - xValue) < rho)
  )
    throw new Error("The proof point z must lie in X and in N(x,rho)");
  const sub = topology.substance(),
    X = sub.ClosedInterval({
      label: `[${interval[0]},${interval[1]}]`,
      a: interval[0],
      b: interval[1],
      leftClosed: true,
      rightClosed: true,
    }),
    tau = sub.Topology({ label: "\\tau_D" }),
    metric = sub.EuclideanMetric({ label: "D(s,t)=|s-t|", dimension: 1 });
  const original = sub.OpenCover({ label: "\\{U_i:i\\in I\\}" }),
    refinement = sub.OpenCover({ label: "\\{N(x,\\rho_x/2):x\\in X\\}" }),
    finite = sub.FiniteOpenCover({
      label: "\\{N(x_j,\\rho_{x_j}/2):1\\le j\\le n\\}",
    });
  const number = sub.LebesgueNumber({ label: "\\rho", value: rho }),
    x = sub.RealPoint({ label: "x", coordinate: xValue }),
    z = sub.RealPoint({ label: "z", coordinate: zValue });
  sub.TopologyOn(tau, X);
  sub.MetricOn(metric, X);
  sub.MetricInducesTopology(metric, tau);
  sub.Compact(tau);
  sub.CompactIn(X, tau);
  sub.T2(tau);
  sub.OpenCoverOf(original, X, tau);
  sub.OpenCoverOf(refinement, X, tau);
  sub.OpenCoverOf(finite, X, tau);
  sub.SubfamilyOf(finite, refinement);
  sub.Member(x, X);
  sub.Member(z, X);
  sub.LebesgueNumberFor(number, original, X, metric);
  const uniform = sub.MetricNeighborhood({ label: "N(x,\\rho)", radius: rho });
  sub.MetricNeighborhoodAt(uniform, x, metric);
  sub.NeighborhoodOf(uniform, x);
  sub.Member(z, uniform);
  sub.Subset(uniform, X);
  sub.OpenIn(uniform, tau);
  balls.forEach(({ center, radius }, i) => {
    const p = sub.RealPoint({ label: `x_${i + 1}`, coordinate: center }),
      big = sub.MetricNeighborhood({
        label: `N(x_${i + 1},\\rho_{x_${i + 1}})`,
        radius,
      }),
      half = sub.MetricNeighborhood({
        label: `N(x_${i + 1},\\rho_{x_${i + 1}}/2)`,
        radius: radius / 2,
      });
    sub.Member(p, X);
    sub.Member(p, big);
    sub.Member(p, half);
    sub.NeighborhoodOf(big, p);
    sub.NeighborhoodOf(half, p);
    sub.MetricNeighborhoodAt(big, p, metric);
    sub.MetricNeighborhoodAt(half, p, metric);
    sub.SetInFamily(big, original);
    sub.SetInFamily(half, finite);
    sub.SetInFamily(half, refinement);
    sub.OpenIn(big, tau);
    sub.OpenIn(half, tau);
    sub.Subset(big, X);
    sub.Subset(half, big);
    if (i === j) {
      sub.Member(x, half);
      sub.Member(z, big);
      sub.Subset(uniform, big);
    }
  });
  return sub.make();
}
/** The same proof and Style with a larger compact interval and different finite subcover. */
export function symmetricIntervalLebesgueCover() {
  return compactIntervalLebesgueCover({
    interval: [-2, 2],
    balls: [
      { center: -2, radius: 1.8 },
      { center: -0.8, radius: 2 },
      { center: 0.8, radius: 2 },
      { center: 2, radius: 1.8 },
    ],
    x: 0.25,
    z: 0.78,
  });
}
export async function buildLebesgueNumberFigure(
  options: CompactIntervalCoverOptions = {},
  renderOptions: FigureRenderOptions = {},
) {
  return diagram({
    sub: compactIntervalLebesgueCover(options),
    sty: lebesgueNumberStyle(),
    canvas: canvas(450, 330),
    variation: "ElementaryTopologyOriginalLebesgueNumber",
    ...renderOptions,
  });
}
export async function buildSymmetricLebesgueNumberFigure(
  renderOptions: FigureRenderOptions = {},
) {
  return diagram({
    sub: symmetricIntervalLebesgueCover(),
    sty: lebesgueNumberStyle(),
    canvas: canvas(450, 330),
    variation: "ElementaryTopologyOriginalSymmetricLebesgueNumber",
    ...renderOptions,
  });
}

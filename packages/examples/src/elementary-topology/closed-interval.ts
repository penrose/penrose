import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  closedIntervalComplementStyle,
  diagram,
  intervalComplementRadius,
  metricSpaces,
} from "@penrose/bloom";

/** Figure 2.9: an absolute-value neighborhood disjoint from a closed interval. */
export function closedIntervalComplementNeighborhood(
  options: {
    bounds?: readonly [number, number];
    position?: number;
  } = {},
) {
  const bounds = options.bounds ?? [0, 1];
  const position = options.position ?? -0.6;
  const rho = intervalComplementRadius(position, bounds);
  const sub = metricSpaces.substance();
  const line = sub.MetricLine({ label: "\\mathbb{R}", metric: "absolute" });
  const interval = sub.ClosedInterval({
    label: `[${bounds[0]},${bounds[1]}]`,
    bounds,
  });
  const x = sub.LinePoint({ label: "x", position });
  const ball = sub.LineNeighborhood({
    label: "N(x,\\rho)",
    center: position,
    rho,
  });
  sub.IntervalInLine(interval, line);
  sub.LinePointInSpace(x, line);
  sub.LineNeighborhoodInSpace(ball, line);
  sub.OutsideInterval(x, interval);
  sub.LineNeighborhoodAt(ball, x);
  sub.LineNeighborhoodAvoidsInterval(ball, interval);
  return sub.make();
}

export const buildClosedIntervalFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: closedIntervalComplementNeighborhood(),
    sty: closedIntervalComplementStyle(),
    canvas: canvas(330, 110),
    variation: "gemignani-closed-unit-interval",
    ...renderOptions,
  });

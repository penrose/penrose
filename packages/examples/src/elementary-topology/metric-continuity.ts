import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  metricContinuityStyle,
  metricSpaces,
} from "@penrose/bloom";

/** A generic continuity witness f(N(a,q)) ⊆ N(f(a),ρ); all layout belongs to Style. */
export function continuousMapNeighborhoods(
  options: { sourceRadius?: number; targetRadius?: number } = {},
) {
  const q = options.sourceRadius ?? 1,
    rho = options.targetRadius ?? 1;
  if (![q, rho].every((r) => Number.isFinite(r) && r > 0))
    throw new Error("Continuity witnesses need finite positive radii");
  const sub = metricSpaces.substance();
  const source = sub.MetricSpace({ label: "X", metricLabel: "D" }),
    target = sub.MetricSpace({ label: "Y", metricLabel: "D'" });
  const f = sub.MetricMap({ label: "f" });
  const a = sub.SpacePoint({ label: "a" }),
    x = sub.SpacePoint({ label: "x" });
  const fa = sub.SpacePoint({ label: "f(a)" }),
    fx = sub.SpacePoint({ label: "f(x)" });
  const sourceBall = sub.SpaceNeighborhood({
    label: "N(a,q)",
    rho: q,
    radiusLabel: "q",
  });
  const targetBall = sub.SpaceNeighborhood({
    label: "N(f(a),\\rho)",
    rho,
    radiusLabel: "\\rho",
  });
  sub.MapBetweenSpaces(f, source, target);
  sub.ContinuousAt(f, a);
  sub.MapsPoint(f, a, fa);
  sub.MapsPoint(f, x, fx);
  for (const point of [a, x]) sub.SpacePointInSpace(point, source);
  for (const point of [fa, fx]) sub.SpacePointInSpace(point, target);
  sub.SpaceNeighborhoodInSpace(sourceBall, source);
  sub.SpaceNeighborhoodInSpace(targetBall, target);
  sub.SpaceNeighborhoodAt(sourceBall, a);
  sub.SpaceNeighborhoodAt(targetBall, fa);
  sub.InsideSpaceNeighborhood(x, sourceBall);
  sub.InsideSpaceNeighborhood(fx, targetBall);
  sub.MapsNeighborhoodInto(f, sourceBall, targetBall);
  return sub.make();
}

export const buildMetricContinuityFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: continuousMapNeighborhoods(),
    sty: metricContinuityStyle(),
    canvas: canvas(570, 260),
    variation: "gemignani-continuous-metric-map",
    ...renderOptions,
  });

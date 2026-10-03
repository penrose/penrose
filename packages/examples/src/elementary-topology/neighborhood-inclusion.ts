import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  interiorNeighborhoodRadius,
  metricSpaces,
  neighborhoodInclusionStyle,
} from "@penrose/bloom";

/** Figure 2.8's mathematical ball-inclusion argument; no visual shapes. */
export function openBallInteriorNeighborhood(
  options: {
    center?: readonly [number, number];
    interior?: readonly [number, number];
    rho?: number;
  } = {},
) {
  const center = options.center ?? [0, 0];
  const interior = options.interior ?? [-0.52, -0.63];
  const rho = options.rho ?? 1;
  const q = interiorNeighborhoodRadius("euclidean", center, interior, rho);
  const sub = metricSpaces.substance();
  const plane = sub.MetricPlane({ label: "(X,D)", metric: "euclidean" });
  const x = sub.MetricPoint({ label: "x", point: center });
  const w = sub.MetricPoint({ label: "w", point: interior });
  const outer = sub.Neighborhood({ label: "N(x,\\rho)", point: center, rho });
  const inner = sub.Neighborhood({
    label: "N(w,q)",
    point: interior,
    rho: q,
    radiusLabel: "q",
  });
  sub.PointInSpace(x, plane);
  sub.PointInSpace(w, plane);
  sub.InSpace(outer, plane);
  sub.InSpace(inner, plane);
  sub.NeighborhoodAt(outer, x);
  sub.NeighborhoodAt(inner, w);
  sub.InsideNeighborhood(w, outer);
  sub.NeighborhoodContainedIn(inner, outer);
  return sub.make();
}

export const buildNeighborhoodInclusionFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: openBallInteriorNeighborhood(),
    sty: neighborhoodInclusionStyle(),
    canvas: canvas(340, 330),
    variation: "gemignani-open-ball-inclusion",
    ...renderOptions,
  });

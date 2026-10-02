import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  metricNeighborhoodStyle,
  metricSpaces,
  type PlaneMetric,
} from "@penrose/bloom";

/** Verified on printed page 19 (PDF page 27) of the supplied second edition. */
export const metricNeighborhoodFigures = [
  { id: "2.1", metric: "euclidean", notation: "D" },
  { id: "2.2", metric: "taxicab", notation: "D_1" },
  { id: "2.3", metric: "discrete", notation: "D_2" },
  { id: "2.4", metric: "supremum", notation: "D_3" },
] as const;

/** A substance program, containing mathematical facts and no shape definitions. */
export function unitNeighborhood(metric: PlaneMetric) {
  const sub = metricSpaces.substance();
  const plane = sub.MetricPlane({ label: "\\mathbb{R}^2", metric });
  const ball = sub.Neighborhood({ label: "N((0,0),1)", point: [0, 0], rho: 1 });
  sub.InSpace(ball, plane);
  return sub.make();
}

export const buildMetricNeighborhoodFigure = (
  metric: PlaneMetric,
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: unitNeighborhood(metric),
    sty: metricNeighborhoodStyle(),
    canvas: canvas(300, 280),
    variation: `gemignani-neighborhood-${metric}`,
    ...renderOptions,
  });

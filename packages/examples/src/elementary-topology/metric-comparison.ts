import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  metricComparisonStyle,
  metricSpaces,
} from "@penrose/bloom";

/** Mathematical content of Figure 2.5, independent of the comparison style. */
export function concentricMetricNeighborhoods(
  options: { point?: readonly [number, number]; rho?: number } = {},
) {
  const point = options.point ?? [0, 0];
  const rho = options.rho ?? 1;
  if (!point.every(Number.isFinite) || !(rho > 0) || !Number.isFinite(rho)) {
    throw new Error(
      "Concentric neighborhoods require a finite center and positive radius",
    );
  }
  const sub = metricSpaces.substance();
  const euclidean = sub.MetricPlane({
    label: "(\\mathbb{R}^2,D)",
    metric: "euclidean",
  });
  const taxicab = sub.MetricPlane({
    label: "(\\mathbb{R}^2,D_1)",
    metric: "taxicab",
  });
  const coordinates = { point, coordinateNames: ["x'", "y'"] as const };
  const inner = sub.Neighborhood({
    ...coordinates,
    label: "N_D((x',y'),\\rho/\\sqrt{2})",
    rho: rho / Math.SQRT2,
    radiusLabel: "\\rho/\\sqrt{2}",
  });
  const diamond = sub.Neighborhood({
    ...coordinates,
    label: "N_{D_1}((x',y'),\\rho)",
    rho,
    radiusLabel: "\\rho",
  });
  const outer = sub.Neighborhood({
    ...coordinates,
    label: "N_D((x',y'),\\rho)",
    rho,
    radiusLabel: "\\rho",
  });
  sub.InSpace(inner, euclidean);
  sub.InSpace(diamond, taxicab);
  sub.InSpace(outer, euclidean);
  sub.NeighborhoodContainedIn(inner, diamond);
  sub.NeighborhoodContainedIn(diamond, outer);
  return sub.make();
}

export const buildMetricComparisonFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: concentricMetricNeighborhoods(),
    sty: metricComparisonStyle(),
    canvas: canvas(360, 300),
    variation: "gemignani-metric-comparison",
    ...renderOptions,
  });

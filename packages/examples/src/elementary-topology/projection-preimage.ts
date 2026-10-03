import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  metricSpaces,
  projectionPreimageStyle,
} from "@penrose/bloom";

/** The infinite inverse image under f(x,y)=x has no finite vertical extent. */
export function coordinateProjectionNeighborhood(
  options: { center?: number; rho?: number } = {},
) {
  const center = options.center ?? 2,
    rho = options.rho ?? 1;
  if (![center, rho].every(Number.isFinite) || !(rho > 0))
    throw new Error(
      "Projection neighborhoods need finite data and positive radius",
    );
  const sub = metricSpaces.substance();
  const plane = sub.MetricPlane({
    label: "(\\mathbb{R}^2,D)",
    metric: "euclidean",
  });
  const line = sub.MetricLine({ label: "\\mathbb{R}", metric: "absolute" });
  const f = sub.CoordinateProjection({ label: "f", coordinate: 0 });
  const a = sub.LinePoint({ label: "a", position: center });
  const ball = sub.LineNeighborhood({ label: "N(a,\\rho)", center, rho });
  const strip = sub.VerticalOpenStrip({
    label: "f^{-1}(N(a,\\rho))",
    center,
    halfWidth: rho,
  });
  sub.ProjectionBetween(f, plane, line);
  sub.LinePointInSpace(a, line);
  sub.LineNeighborhoodAt(ball, a);
  sub.LineNeighborhoodInSpace(ball, line);
  sub.StripInPlane(strip, plane);
  sub.ProjectionInverseImage(strip, f, ball);
  return sub.make();
}

export const buildProjectionPreimageFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: coordinateProjectionNeighborhood(),
    sty: projectionPreimageStyle(),
    canvas: canvas(350, 270),
    variation: "gemignani-coordinate-projection-preimage",
    ...renderOptions,
  });

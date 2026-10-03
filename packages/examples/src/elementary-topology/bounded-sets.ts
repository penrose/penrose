import type { FigureRenderOptions } from "@penrose/bloom";
import {
  boundedSetStyle,
  canvas,
  diagram,
  pointSetTopology as topology,
} from "@penrose/bloom";

/** Closed bounded A is a subset of a sup-norm neighborhood and its compact product closure. */
export function closedBoundedSet(options: { radius?: number } = {}) {
  const p = options.radius ?? 1;
  if (!(p > 0) || !Number.isFinite(p))
    throw new Error(
      "The bounding neighborhood radius must be finite and positive",
    );
  const sub = topology.substance();
  const plane = sub.EuclideanPlane({ label: "\\mathbb R^2" }),
    tau = sub.Topology({ label: "\\tau_{D_\\infty}" });
  const a = sub.ClosedRegion({ label: "A" });
  const neighborhood = sub.OpenSquare({
    label: "N'(\\bar0,p)",
    center: [0, 0],
    radius: p,
  });
  // The source graphic omits the prime used in the adjacent proof. Preserve its printed label.
  const box = sub.ClosedProductRectangle({
    label: "\\operatorname{Cl}N(\\bar0,p)",
    bounds: [-p, -p, p, p],
  });
  const interval = sub.ClosedInterval({
    label: "[-p,p]",
    a: -p,
    b: p,
    leftClosed: true,
    rightClosed: true,
  });
  const origin = sub.CoordinatePoint({ label: "\\bar0", coordinates: [0, 0] });
  sub.TopologyOn(tau, plane);
  sub.Subset(a, plane);
  sub.Subset(neighborhood, plane);
  sub.Subset(box, plane);
  sub.ClosedIn(a, tau);
  sub.BoundedBy(a, neighborhood);
  sub.Subset(a, neighborhood);
  sub.Subset(a, box);
  sub.ClosureOf(box, neighborhood, tau);
  sub.ProductOf(box, interval, interval);
  sub.ClosedIn(box, tau);
  sub.CompactIn(box, tau);
  sub.CompactIn(a, tau);
  sub.Member(origin, neighborhood);
  return sub.make();
}
export const buildClosedBoundedSetFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: closedBoundedSet(),
    sty: boundedSetStyle(),
    canvas: canvas(350, 208),
    variation: "gemignani-closed-bounded-sup-norm-box",
    ...renderOptions,
  });

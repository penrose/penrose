import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  lowerLimitProductStyle,
  pointSetTopology as topology,
} from "@penrose/bloom";

const construction = (diagonal: boolean) => {
  const sub = topology.substance();
  const real = sub.RealLine({ label: "\\mathbb R" }),
    lower = sub.LowerLimitTopology({ label: "\\tau" });
  const plane = sub.ProductSet({ label: "\\mathbb R^2" }),
    product = sub.Topology({ label: "\\tau\\times\\tau" });
  sub.TopologyOn(lower, real);
  sub.Normal(lower);
  sub.Regular(lower);
  sub.T1(lower);
  sub.ProductOf(plane, real, real);
  sub.ProductTopologyOf(product, lower, lower);
  sub.TopologyOn(product, plane);
  sub.Regular(product);
  sub.T1(product);
  sub.T3(product);
  sub.NotNormal(product);
  const [x, y, width, height] = diagonal
    ? [-1.3, 1.3, 0.65, 0.75]
    : [-2.35, 0.5, 1.1, 1.1];
  const ix = sub.LowerLimitInterval({
      label: "[x,a)",
      a: x,
      b: x + width,
      leftClosed: true,
      rightClosed: false,
    }),
    iy = sub.LowerLimitInterval({
      label: "[y,b)",
      a: y,
      b: y + height,
      leftClosed: true,
      rightClosed: false,
    });
  const px = sub.RealPoint({ label: "x", coordinate: x }),
    py = sub.RealPoint({ label: "y", coordinate: y });
  for (const [interval, point] of [
    [ix, px],
    [iy, py],
  ] as const) {
    sub.OpenIn(interval, lower);
    sub.ClosedIn(interval, lower);
    sub.ClosureOf(interval, interval, lower);
    sub.Member(point, interval);
    sub.NeighborhoodOf(interval, point);
    sub.Subset(interval, real);
  }
  const neighborhood = sub.LowerLimitPlaneNeighborhood({ label: "U" }),
    anchor = sub.CoordinatePoint({ label: "(x,y)", coordinates: [x, y] });
  sub.ProductOf(neighborhood, ix, iy);
  sub.Subset(neighborhood, plane);
  sub.OpenIn(neighborhood, product);
  sub.ClosedIn(neighborhood, product);
  sub.ClosureOf(neighborhood, neighborhood, product);
  sub.NeighborhoodOf(neighborhood, anchor);
  sub.Member(anchor, neighborhood);
  sub.Member(anchor, plane);
  if (diagonal) {
    const line = sub.AffineSubspace({ label: "Y", coefficients: [1, 1, 0] }),
      tauY = sub.Topology({ label: "\\tau_Y" });
    const singleton = sub.Singleton({ label: "\\{(x,y)\\}" });
    sub.Subset(line, plane);
    sub.ClosedIn(line, product);
    sub.TopologyOn(tauY, line);
    sub.SubspaceTopologyOf(tauY, line, product);
    sub.Discrete(tauY);
    sub.Member(anchor, line);
    sub.SingletonOf(singleton, anchor);
    sub.Member(anchor, singleton);
    sub.IntersectionOf(singleton, neighborhood, line);
  }
  return sub.make();
};

/** A typical [x,a)×[y,b) basic neighborhood in the lower-limit plane. */
export const lowerLimitBasicNeighborhood = () => construction(false);
/** Its intersection with x+y=0 is a singleton; the subspace is discrete. */
export const lowerLimitDiagonalSingleton = () => construction(true);

export const buildLowerLimitNeighborhoodFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: lowerLimitBasicNeighborhood(),
    sty: lowerLimitProductStyle(),
    canvas: canvas(325, 211),
    variation: "gemignani-lower-limit-neighborhood",
    ...renderOptions,
  });
export const buildLowerLimitDiagonalFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: lowerLimitDiagonalSingleton(),
    sty: lowerLimitProductStyle(),
    canvas: canvas(331, 241),
    variation: "gemignani-lower-limit-diagonal-singleton",
    ...renderOptions,
  });

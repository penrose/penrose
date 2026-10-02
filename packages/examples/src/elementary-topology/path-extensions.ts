import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  pathExtensionStyle,
  pointSetTopology as topology,
} from "@penrose/bloom";

/** The two closed horizontal edges carry two paths; their union map extends to the square. */
export function boundaryPathExtension() {
  const sub = topology.substance();
  const interval = sub.ClosedInterval({
    label: "[0,1]",
    a: 0,
    b: 1,
    leftClosed: true,
    rightClosed: true,
  });
  const square = sub.ClosedProductRectangle({
    label: "[0,1]\\times[0,1]",
    bounds: [0, 0, 1, 1],
  });
  const boundary = sub.HorizontalBoundaryPair({ label: "A" }),
    family = sub.FiniteSetFamily({ label: "\\{A_0,A_1\\}" });
  const bottom = sub.LinearSegment({
      label: "A_0",
      endpoints: [
        [0, 0],
        [1, 0],
      ],
    }),
    top = sub.LinearSegment({
      label: "A_1",
      endpoints: [
        [0, 1],
        [1, 1],
      ],
    });
  const real = sub.RealLine({ label: "\\mathbb R" }),
    plane = sub.ProductSet({ label: "\\mathbb R^2" });
  const tauI = sub.Topology({ label: "\\tau_I" }),
    tauSquare = sub.Topology({ label: "\\tau_{I^2}" }),
    tauBoundary = sub.Topology({ label: "\\tau_A" }),
    tauR = sub.Topology({ label: "\\tau_R" }),
    tauPlane = sub.Topology({ label: "\\tau_{R^2}" });
  sub.ProductOf(square, interval, interval);
  sub.ProductOf(plane, real, real);
  sub.ProductTopologyOf(tauSquare, tauI, tauI);
  sub.ProductTopologyOf(tauPlane, tauR, tauR);
  sub.TopologyOn(tauI, interval);
  sub.TopologyOn(tauSquare, square);
  sub.TopologyOn(tauBoundary, boundary);
  sub.TopologyOn(tauR, real);
  sub.TopologyOn(tauPlane, plane);
  sub.Normal(tauSquare);
  sub.Normal(tauPlane);
  sub.SetInFamily(bottom, family);
  sub.SetInFamily(top, family);
  sub.UnionOf(boundary, family);
  sub.HorizontalEdgesOf(boundary, square);
  sub.Subset(boundary, square);
  sub.ClosedIn(boundary, tauSquare);
  sub.SubspaceTopologyOf(tauBoundary, boundary, tauSquare);
  const lower = sub.TopologicalPath({ label: "f_0" }),
    upper = sub.TopologicalPath({ label: "f_1" }),
    f = sub.TopologicalMap({ label: "f" }),
    extension = sub.TopologicalMap({ label: "F" });
  sub.MapBetween(lower, interval, plane);
  sub.MapBetween(upper, interval, plane);
  sub.ContinuousMap(lower, tauI, tauPlane);
  sub.ContinuousMap(upper, tauI, tauPlane);
  sub.MapBetween(f, boundary, plane);
  sub.MapBetween(extension, square, plane);
  sub.BoundaryPathsOf(f, lower, upper, boundary);
  sub.ExtensionOf(extension, f, boundary, square);
  sub.ContinuousMap(f, tauBoundary, tauPlane);
  sub.ContinuousMap(extension, tauSquare, tauPlane);
  return sub.make();
}

export const buildBoundaryPathExtensionFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: boundaryPathExtension(),
    sty: pathExtensionStyle(),
    canvas: canvas(337, 198),
    variation: "gemignani-two-path-continuous-extension",
    ...renderOptions,
  });

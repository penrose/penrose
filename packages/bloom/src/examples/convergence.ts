import { diagram, type FigureRenderOptions } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  partitionMesh,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  intersectingNeighborhoodsStyle,
  intervalPartitionStyle,
} from "../styles/convergence.js";

/** Example 11: one finite partition in the book's directed family of all partitions. */
export function intervalPartitionSubstance(
  points: readonly number[] = [0, 0.13, 0.27, 0.47, 0.57, 0.64, 0.71, 0.84, 1],
) {
  partitionMesh(points);
  const s = topology.substance();
  const interval = s.ClosedInterval({
    label: "[a,b]",
    a: points[0],
    b: points[points.length - 1],
    leftClosed: true,
    rightClosed: true,
    endpointNames: ["a", "b"],
  });
  const P = s.IntervalPartition({ label: "P", points });
  const family = s.PartitionDirectedSet({
    label: "\\mathfrak{P}",
    order: "finer-first",
  });
  s.PartitionOf(P, interval);
  s.PartitionsOfInterval(family, interval);
  s.PartitionInFamily(P, family);
  return s.make();
}

/** Proposition 8: an all-neighborhood selection net can have two distinct limits. */
export function nonHausdorffSelectionNetSubstance() {
  const s = topology.substance();
  const X = s.Set({ label: "X" }),
    tau = s.Topology({ label: "\\tau" });
  const x = s.Point({ label: "x" }),
    y = s.Point({ label: "y" });
  s.TopologyOn(tau, X);
  s.NotT2(tau);
  s.Member(x, X);
  s.Member(y, X);
  s.UnseparablePoints(x, y, tau);
  const Tx = s.NeighborhoodSystem({ label: "T(X,x)" }),
    Ty = s.NeighborhoodSystem({ label: "T(X,y)" });
  const Ix = s.NeighborhoodDirectedSet({
    label: "T(X,x)",
    order: "reverse-inclusion",
  });
  const Iy = s.NeighborhoodDirectedSet({
    label: "T(X,y)",
    order: "reverse-inclusion",
  });
  s.NeighborhoodSystemAt(Tx, x, tau);
  s.NeighborhoodSystemAt(Ty, y, tau);
  s.NeighborhoodsDirectedBy(Tx, Ix);
  s.NeighborhoodsDirectedBy(Ty, Iy);
  const I = s.ProductDirectedSet({ label: "T(X,x)\\times T(X,y)" });
  s.DirectedProductOf(I, Ix, Iy);
  const selector = s.TopologicalMap({ label: "s" });
  s.MapBetween(selector, I, X);
  s.ChoosesFromIntersections(selector, Tx, Ty);
  const net = s.Net({ label: "\\{s_{(U,V)}\\}" });
  s.NetIndexedBy(net, I);
  s.NetIn(net, X);
  s.SelectionNetOf(net, selector);
  s.NetConvergesTo(net, x, tau);
  s.NetConvergesTo(net, y, tau);
  const U = s.Neighborhood({ label: "U" }),
    V = s.Neighborhood({ label: "V" });
  s.NeighborhoodOf(U, x);
  s.NeighborhoodOf(V, y);
  s.Subset(U, X);
  s.Subset(V, X);
  s.OpenIn(U, tau);
  s.OpenIn(V, tau);
  s.NeighborhoodInSystem(U, Tx);
  s.NeighborhoodInSystem(V, Ty);
  const value = s.Point({ label: "s(U,V)" });
  s.Member(value, X);
  s.Member(value, U);
  s.Member(value, V);
  s.SelectedIntersectionValue(selector, U, V, value);
  return s.make();
}

export const buildIntervalPartitionFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: intervalPartitionSubstance(),
    sty: intervalPartitionStyle(),
    canvas: canvas(365, 82),
    variation: "gemignani-6.1",
    ...renderOptions,
  });
export const buildNonHausdorffNetFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: nonHausdorffSelectionNetSubstance(),
    sty: intersectingNeighborhoodsStyle({ offset: [-9, -15] }),
    canvas: canvas(370, 187),
    variation: "gemignani-6.2",
    ...renderOptions,
  });

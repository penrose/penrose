import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  compactNeighborhoodStyle,
  diagram,
  dyadicValue,
  locallyClosedSubspaceStyle,
  rationalNeighborhoodStyle,
  pointSetTopology as topology,
} from "@penrose/bloom";

/** N(x,p/2) subset Cl N(x,p/2) subset N(x,p) subset arbitrary U. */
export function euclideanCompactNeighborhood(
  options: { radius?: number } = {},
) {
  const p = options.radius ?? 1;
  if (!(p > 0) || !Number.isFinite(p) || !(p / 2 > 0))
    throw new Error(
      "Local compactness needs a positive finite radius and half-radius",
    );
  const sub = topology.substance();
  const plane = sub.EuclideanPlane({ label: "\\mathbb R^2" }),
    tau = sub.Topology({ label: "\\tau_D" });
  const x = sub.CoordinatePoint({ label: "x", coordinates: [0, 0] });
  const u = sub.Neighborhood({ label: "U" });
  const inner = sub.DiskNeighborhood({
    label: "N(x,p/2)",
    center: [0, 0],
    radius: p / 2,
  });
  const outer = sub.DiskNeighborhood({
    label: "N(x,p)",
    center: [0, 0],
    radius: p,
  });
  const closed = sub.ClosedDisk({
    label: "\\operatorname{Cl}N(x,p/2)",
    center: [0, 0],
    radius: p / 2,
  });
  sub.TopologyOn(tau, plane);
  sub.NotCompact(tau);
  sub.LocallyCompact(tau);
  sub.Member(x, plane);
  for (const n of [u, inner, outer]) {
    sub.NeighborhoodOf(n, x);
    sub.Member(x, n);
    sub.OpenIn(n, tau);
    sub.Subset(n, plane);
  }
  sub.Member(x, closed);
  sub.Subset(inner, closed);
  sub.Subset(closed, outer);
  sub.Subset(outer, u);
  sub.ClosureOf(closed, inner, tau);
  sub.InteriorOf(inner, closed, tau);
  sub.ClosedIn(closed, tau);
  sub.CompactIn(closed, tau);
  sub.CompactNeighborhoodWithin(x, u, inner, closed, tau);
  return sub.make();
}

/** The assumed compact rational neighborhood admits the irrational-cut cover with no finite subcover. */
export function rationalNoncompactNeighborhood(
  options: {
    interval?: readonly [number, number];
    numerator?: number;
    exponent?: number;
  } = {},
) {
  const [a, b] = options.interval ?? [0, 1],
    numerator = options.numerator ?? 5,
    exponent = options.exponent ?? 4;
  const coordinate = dyadicValue(numerator, exponent),
    cut = Math.SQRT2 / 2;
  if (
    ![a, b].every(Number.isFinite) ||
    !(a < coordinate && coordinate < cut && cut < b)
  )
    throw new Error(
      "The illustrated rational x and irrational t must occur in the source's order inside (a,b)",
    );
  const sub = topology.substance();
  const real = sub.RealLine({ label: "R" }),
    q = sub.RationalSubspace({ label: "Q" });
  const tauR = sub.Topology({ label: "\\tau_R" }),
    tauQ = sub.Topology({ label: "\\tau_Q" });
  const assumed = sub.ClosedRegion({ label: "A" }),
    interior = sub.Neighborhood({ label: "A^\\circ" });
  const interval = sub.OpenInterval({
    label: "(a,b)",
    a,
    b,
    leftClosed: false,
    rightClosed: false,
  });
  const relative = sub.Neighborhood({ label: "(a,b)\\cap Q" });
  const x = sub.DyadicRational({ label: "x", coordinate, numerator, exponent });
  const t = sub.IrrationalPoint({
    label: "t",
    expression: "\\sqrt2/2",
    approximation: cut,
  });
  const cover = sub.OpenCover({ label: "\\{U(q)\\cap A:q\\in A\\}" });
  sub.TopologyOn(tauR, real);
  sub.TopologyOn(tauQ, q);
  sub.Subset(q, real);
  sub.SubspaceTopologyOf(tauQ, q, tauR);
  sub.LocallyCompact(tauR);
  sub.NotLocallyCompact(tauQ);
  sub.AssumedCompactIn(assumed, tauQ);
  sub.Subset(assumed, q);
  sub.InteriorOf(interior, assumed, tauQ);
  sub.Subset(interior, assumed);
  sub.OpenIn(interior, tauQ);
  sub.IntersectionOf(relative, interval, q);
  sub.Subset(relative, interior);
  sub.OpenIn(relative, tauQ);
  sub.OpenIn(interval, tauR);
  sub.NeighborhoodOf(interior, x);
  sub.NeighborhoodOf(relative, x);
  for (const set of [q, real, interval, relative, interior, assumed])
    sub.Member(x, set);
  sub.IrrationalInInterval(t, interval);
  sub.Member(t, interval);
  sub.Member(t, real);
  sub.Outside(t, q);
  sub.RationalRayCoverAt(cover, t, assumed, q, tauQ);
  sub.OpenCoverOf(cover, assumed, tauQ);
  sub.HasNoFiniteSubcover(cover, assumed, tauQ);
  sub.FailsLocalCompactnessAt(x, q, tauQ);
  return sub.make();
}

/** The relative neighborhood U'=Y intersection V has compact closure, yielding Y open in Cl Y. */
export function locallyClosedSubspace() {
  const sub = topology.substance();
  const space = sub.Set({ label: "X" }),
    y = sub.OpenSubspace({ label: "Y" });
  const tauX = sub.Topology({ label: "\\tau" }),
    tauY = sub.Topology({ label: "\\tau_Y" }),
    tauClosure = sub.Topology({ label: "\\tau_{\\operatorname{Cl}Y}" });
  const complement = sub.Set({ label: "X-Y" }),
    point = sub.Point({ label: "y" });
  const ambient = sub.Neighborhood({ label: "V" }),
    relative = sub.Neighborhood({ label: "U'" });
  const closedRelative = sub.ClosedRegion({ label: "\\operatorname{Cl}_Y U'" }),
    closureY = sub.ClosedSubspace({ label: "\\operatorname{Cl}_X Y" });
  const globalOpen = sub.OpenSet({ label: "U" });
  sub.TopologyOn(tauX, space);
  sub.TopologyOn(tauY, y);
  sub.TopologyOn(tauClosure, closureY);
  sub.SubspaceTopologyOf(tauY, y, tauX);
  sub.SubspaceTopologyOf(tauClosure, closureY, tauX);
  sub.T2(tauX);
  sub.LocallyCompact(tauX);
  sub.LocallyCompact(tauY);
  sub.LocallyCompactIn(y, tauX);
  sub.LocallyClosedIn(y, space, tauX);
  sub.Subset(y, space);
  sub.ComplementOf(complement, y, space);
  sub.IntersectionOf(relative, y, ambient);
  sub.OpenIn(ambient, tauX);
  sub.OpenIn(relative, tauY);
  sub.NeighborhoodOf(ambient, point);
  sub.NeighborhoodOf(relative, point);
  sub.Member(point, y);
  sub.Member(point, relative);
  sub.Member(point, ambient);
  sub.Outside(point, complement);
  sub.Subset(relative, y);
  sub.ClosureOf(closedRelative, relative, tauY);
  sub.CompactIn(closedRelative, tauY);
  sub.ClosedIn(closedRelative, tauX);
  sub.ClosureOf(closedRelative, relative, tauX);
  sub.Subset(closedRelative, y);
  sub.ClosureOf(closureY, y, tauX);
  sub.OpenIn(y, tauClosure);
  sub.OpenIn(globalOpen, tauX);
  sub.IntersectionOf(y, globalOpen, closureY);
  return sub.make();
}
export const buildCompactNeighborhoodFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: euclideanCompactNeighborhood(),
    sty: compactNeighborhoodStyle(),
    canvas: canvas(294, 246),
    variation: "gemignani-compact-half-radius-neighborhood",
    ...renderOptions,
  });
export const buildRationalNeighborhoodFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: rationalNoncompactNeighborhood(),
    sty: rationalNeighborhoodStyle(),
    canvas: canvas(351, 106),
    variation: "gemignani-rational-irrational-cut-cover",
    ...renderOptions,
  });
export const buildLocallyClosedSubspaceFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: locallyClosedSubspace(),
    sty: locallyClosedSubspaceStyle(),
    canvas: canvas(239, 218),
    variation: "gemignani-locally-closed-subspace",
    ...renderOptions,
  });

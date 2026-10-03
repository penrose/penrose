import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  compactHausdorffStyle,
  diagram,
  extendFiniteCoverReach,
  finiteCoverReachStyle,
  separatedCoverRadii,
  separatedCoverStyle,
  pointSetTopology as topology,
} from "@penrose/bloom";

/** A generic maximal p-separated subset; drawing a finite grid does not enumerate it. */
export function separatedMetricCover(options: { separation?: number } = {}) {
  const p = options.separation ?? 1,
    radii = separatedCoverRadii(p);
  const sub = topology.substance();
  const space = sub.Set({ label: "X" }),
    tau = sub.Topology({ label: "\\tau_D" });
  const centers = sub.MaximalSeparatedSubset({ label: "E", separation: p });
  const quarter = sub.MetricBallFamily({
    label: "\\{N(a,p/4):a\\in E\\}",
    radius: radii.quarter,
  });
  const half = sub.MetricBallFamily({
    label: "\\{N(a,p/2):a\\in E\\}",
    radius: radii.half,
  });
  const closures = sub.MetricClosureFamily({
    label: "\\{\\operatorname{Cl}N(a,p/4):a\\in E\\}",
    radius: radii.quarter,
  });
  const closedUnion = sub.ClosedRegion({
    label: "\\bigcup_{a\\in E}\\operatorname{Cl}N(a,p/4)",
  });
  const complement = sub.OpenSet({ label: "V" }),
    cover = sub.OpenCover({ label: "\\mathcal U" });
  const a = sub.Point({ label: "a" });
  sub.TopologyOn(tau, space);
  sub.Lindelof(tau);
  sub.SeparatedIn(centers, space, tau);
  sub.Subset(centers, space);
  sub.Countable(centers);
  sub.Member(a, centers);
  sub.Member(a, space);
  sub.MetricBallsAt(quarter, centers, tau);
  sub.MetricBallsAt(half, centers, tau);
  sub.ClosuresOfFamily(closures, quarter, tau);
  sub.UnionOf(closedUnion, closures);
  sub.ClosedIn(closedUnion, tau);
  sub.ComplementOf(complement, closedUnion, space);
  sub.OpenIn(complement, tau);
  sub.SetInFamily(complement, cover);
  sub.FamilyIncludedIn(half, cover);
  sub.OpenCoverOf(cover, space, tau);
  sub.IndispensableCenteredCover(cover, half, centers, complement);
  return sub.make();
}

/** Case 1: a finite subcover of [0,u) plus one cover member reaches beyond the assumed supremum. */
export function finiteCoverSupremumWitness(
  options: { u?: number; interval?: readonly [number, number] } = {},
) {
  const uValue = options.u ?? 0.55;
  const [a, b] = options.interval ?? [0.4, 0.72];
  const intervalData = {
    a,
    b,
    leftClosed: false as const,
    rightClosed: false as const,
  };
  const nextValue = extendFiniteCoverReach(uValue, intervalData);
  const sub = topology.substance();
  const real = sub.RealLine({ label: "\\mathbb R" }),
    tau = sub.Topology({ label: "\\tau_R" });
  const unit = sub.ClosedInterval({
    label: "[0,1]",
    a: 0,
    b: 1,
    leftClosed: true,
    rightClosed: true,
  });
  const original = sub.OpenCover({ label: "\\{U_i:i\\in I\\}" });
  const reach = sub.FiniteCoverReachSet({ label: "T" });
  const u = sub.RealPoint({ label: "u", coordinate: uValue }),
    next = sub.RealPoint({ label: "v", coordinate: nextValue });
  const initial = sub.RealInterval({
    label: "[0,u)",
    a: 0,
    b: uValue,
    leftClosed: true,
    rightClosed: false,
  });
  const coveredExtension = sub.ClosedInterval({
    label: "[0,v]",
    a: 0,
    b: nextValue,
    leftClosed: true,
    rightClosed: true,
  });
  const interval = sub.OpenIntervalNeighborhood({
    label: "U_{i'}",
    ...intervalData,
  });
  const before = sub.OpenInterval({
    label: "W",
    a: -1,
    b: uValue,
    leftClosed: false,
    rightClosed: false,
  });
  const prefixCover = sub.FiniteOpenCover({ label: "\\mathcal U_0" }),
    extendedCover = sub.FiniteOpenCover({
      label: "\\mathcal U_0\\cup\\{U_{i'}\\}",
    });
  sub.TopologyOn(tau, real);
  sub.Subset(unit, real);
  sub.Subset(reach, unit);
  sub.OpenCoverOf(original, unit, tau);
  sub.FiniteCoverReachOf(reach, original, unit);
  sub.AssumedSupremumOf(u, reach);
  sub.Member(u, reach);
  sub.Member(u, unit);
  sub.Member(next, reach);
  sub.Member(next, unit);
  sub.Member(u, interval);
  sub.NeighborhoodOf(interval, u);
  sub.OpenIn(interval, tau);
  sub.OpenIn(before, tau);
  sub.SetInFamily(before, prefixCover);
  sub.SetInFamily(before, extendedCover);
  sub.SetInFamily(interval, extendedCover);
  sub.SetInFamily(before, original);
  sub.SetInFamily(interval, original);
  sub.SubfamilyOf(prefixCover, original);
  sub.SubfamilyOf(extendedCover, original);
  sub.OpenCoverOf(prefixCover, initial, tau);
  sub.OpenCoverOf(extendedCover, coveredExtension, tau);
  sub.InitialIntervalAt(initial, u);
  return sub.make();
}

/** Finite Hausdorff witnesses selected from an open cover of a compact subset. */
export function compactSubsetHausdorffSeparation() {
  const sub = topology.substance();
  const space = sub.Set({ label: "X" }),
    tau = sub.Topology({ label: "\\tau" });
  const a = sub.Set({ label: "A" }),
    x = sub.Point({ label: "x" });
  const full = sub.OpenCover({ label: "\\{V_y:y\\in A\\}" }),
    finite = sub.FiniteOpenCover({ label: "\\{V_{y_1},\\ldots,V_{y_n}\\}" });
  const outsideFamily = sub.FiniteSetFamily({
    label: "\\{U_{y_1},\\ldots,U_{y_n}\\}",
  });
  const intersection = sub.Neighborhood({ label: "U" }),
    union = sub.NeighborhoodUnion({ label: "V" });
  sub.TopologyOn(tau, space);
  sub.T2(tau);
  sub.CompactIn(a, tau);
  sub.Subset(a, space);
  sub.Outside(x, a);
  sub.Member(x, space);
  sub.OpenCoverOf(full, a, tau);
  sub.OpenCoverOf(finite, a, tau);
  sub.SubfamilyOf(finite, full);
  // The source's n is arbitrary. Six illustrative chosen witnesses do not enumerate A.
  for (let i = 1; i <= 6; i++) {
    const index = i === 6 ? "n" : String(i);
    const y = sub.Point({ label: `y_${index}` });
    const u = sub.Neighborhood({ label: `U_{y_${index}}` }),
      v = sub.Neighborhood({ label: `V_{y_${index}}` });
    sub.Member(y, a);
    sub.Member(y, v);
    sub.Member(x, u);
    sub.NeighborhoodOf(u, x);
    sub.NeighborhoodOf(v, y);
    sub.OpenIn(u, tau);
    sub.OpenIn(v, tau);
    sub.Disjoint(u, v);
    sub.HausdorffNeighborhoodPair(x, y, u, v, tau);
    sub.SetInFamily(v, finite);
    sub.SetInFamily(v, full);
    sub.SetInFamily(u, outsideFamily);
    sub.Subset(intersection, u);
    sub.Subset(v, union);
  }
  sub.FiniteIntersectionOf(intersection, outsideFamily);
  sub.UnionOf(union, finite);
  sub.NeighborhoodOf(intersection, x);
  sub.Member(x, intersection);
  sub.OpenIn(intersection, tau);
  sub.OpenIn(union, tau);
  sub.Subset(a, union);
  sub.Disjoint(intersection, union);
  return sub.make();
}

export const buildSeparatedMetricCoverFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: separatedMetricCover(),
    sty: separatedCoverStyle(),
    canvas: canvas(219, 213),
    variation: "gemignani-separated-metric-cover",
    ...renderOptions,
  });
export const buildFiniteCoverReachFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: finiteCoverSupremumWitness(),
    sty: finiteCoverReachStyle(),
    canvas: canvas(339, 63),
    variation: "gemignani-finite-cover-supremum",
    ...renderOptions,
  });
export const buildCompactHausdorffFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: compactSubsetHausdorffSeparation(),
    sty: compactHausdorffStyle(),
    canvas: canvas(478, 198),
    variation: "gemignani-compact-hausdorff-subset",
    ...renderOptions,
  });

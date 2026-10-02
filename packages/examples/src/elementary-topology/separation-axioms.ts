import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  closureNeighborhoodStyle,
  deletedReciprocalStyle,
  diagram,
  reciprocalNeighborhoodOverlap,
  reciprocalSequenceTerm,
  separationAxiomStyle,
  pointSetTopology as topology,
} from "@penrose/bloom";

/** A chosen separation witness; A and B need not be closed. */
export function disjointSetNeighborhoods() {
  const sub = topology.substance();
  const x = sub.Set({ label: "X" }),
    tau = sub.Topology({ label: "\\tau" });
  const a = sub.Set({ label: "A" }),
    b = sub.Set({ label: "B" }),
    u = sub.OpenSet({ label: "U" }),
    v = sub.OpenSet({ label: "V" });
  sub.TopologyOn(tau, x);
  sub.OpenIn(u, tau);
  sub.OpenIn(v, tau);
  sub.Subset(a, u);
  sub.Subset(b, v);
  sub.Subset(u, x);
  sub.Subset(v, x);
  sub.Disjoint(a, b);
  sub.Disjoint(u, v);
  sub.SetSeparation(a, b, u, v, tau);
  return sub.make();
}

/** Gemignani's T3 point/closed-set separation, without asserting T1. */
export function topologicalPointClosedSeparation() {
  const sub = topology.substance();
  const universe = sub.Set({ label: "X" }),
    tau = sub.Topology({ label: "\\tau" });
  const x = sub.Point({ label: "x" }),
    f = sub.ClosedRegion({ label: "F" }),
    u = sub.Neighborhood({ label: "U" }),
    v = sub.OpenSet({ label: "V" });
  sub.TopologyOn(tau, universe);
  sub.T3(tau);
  sub.ClosedIn(f, tau);
  sub.OpenIn(u, tau);
  sub.OpenIn(v, tau);
  sub.Member(x, universe);
  sub.Member(x, u);
  sub.Outside(x, f);
  sub.NeighborhoodOf(u, x);
  sub.Subset(f, v);
  sub.Subset(u, universe);
  sub.Subset(v, universe);
  sub.Disjoint(u, v);
  sub.PointClosedSetSeparation(x, f, u, v, tau);
  return sub.make();
}

/** Proposition 4's proof witness V⊆Cl V⊆X−W⊆U, with X−U⊆W. */
export function nestedClosureNeighborhoods() {
  const sub = topology.substance();
  const universe = sub.Set({ label: "X" }),
    tau = sub.Topology({ label: "\\tau" });
  const x = sub.Point({ label: "x" }),
    u = sub.Neighborhood({ label: "U" }),
    v = sub.Neighborhood({ label: "V" }),
    w = sub.OpenSet({ label: "W" });
  const closure = sub.ClosedRegion({ label: "\\operatorname{Cl}V" }),
    xU = sub.ClosedRegion({ label: "X-U" }),
    xW = sub.ClosedRegion({ label: "X-W" });
  sub.TopologyOn(tau, universe);
  sub.T3(tau);
  for (const open of [u, v, w]) {
    sub.OpenIn(open, tau);
    sub.Subset(open, universe);
  }
  for (const closed of [closure, xU, xW]) {
    sub.ClosedIn(closed, tau);
    sub.Subset(closed, universe);
  }
  sub.NeighborhoodOf(u, x);
  sub.NeighborhoodOf(v, x);
  sub.Member(x, universe);
  sub.Member(x, u);
  sub.Member(x, v);
  sub.ClosureOf(closure, v, tau);
  sub.ComplementOf(xU, u, universe);
  sub.ComplementOf(xW, w, universe);
  sub.Subset(v, closure);
  sub.Subset(closure, xW);
  sub.Subset(xW, u);
  sub.Subset(closure, u);
  sub.Subset(xU, w);
  sub.Disjoint(v, w);
  sub.NeighborhoodClosureWithin(v, closure, u, x, tau);
  return sub.make();
}

/**
 * The infinite deleted-reciprocal topology is T2 but not the book's T3.
 * U denotes an arbitrary open set containing F. A representative local interval
 * about c/n, as guaranteed by openness of U, supplies a point in U∩V.
 */
export function deletedReciprocalCounterexample(
  options: {
    coefficient?: number;
    radius?: number;
    index?: number;
    epsilon?: number;
  } = {},
) {
  const coefficient = options.coefficient ?? 1,
    radius = options.radius ?? 0.27,
    index = options.index ?? Math.max(6, Math.floor(coefficient / radius) + 1),
    termValue = reciprocalSequenceTerm(coefficient, index),
    epsilon = options.epsilon ?? Math.min(0.018 * coefficient, termValue / 3);
  const witness = reciprocalNeighborhoodOverlap(
    radius,
    coefficient,
    index,
    epsilon,
  );
  const sub = topology.substance();
  const line = sub.RealLine({ label: "\\mathbb{R}" });
  const tau = sub.DeletedReciprocalTopology({ label: "\\tau", coefficient });
  const f = sub.ReciprocalSequenceSet({
    label:
      coefficient === 1
        ? "\\{1/n:n\\in\\mathbb{N}\\}"
        : `\\{${coefficient}/n:n\\in\\mathbb{N}\\}`,
    coefficient,
  });
  const zero = sub.RealPoint({ label: "0", coordinate: 0 }),
    term = sub.RealPoint({
      label: `${coefficient}/${index}`,
      coordinate: witness.term,
    }),
    overlap = sub.RealPoint({ label: "z", coordinate: witness.point });
  const v = sub.DeletedReciprocalNeighborhood({ label: "V", radius }),
    u = sub.NeighborhoodUnion({ label: "U" });
  const interval = sub.OpenIntervalNeighborhood({
    label: "I",
    a: witness.interval[0],
    b: witness.interval[1],
    leftClosed: false,
    rightClosed: false,
  });
  sub.TopologyOn(tau, line);
  sub.T2(tau);
  sub.NotT3(tau);
  sub.ReciprocalSetFor(f, tau);
  sub.ClosedIn(f, tau);
  for (const open of [u, v, interval]) {
    sub.OpenIn(open, tau);
    sub.Subset(open, line);
  }
  sub.Subset(f, line);
  sub.Subset(f, u);
  sub.Subset(interval, u);
  sub.Member(zero, line);
  sub.Member(zero, v);
  sub.Outside(zero, f);
  sub.Member(term, line);
  sub.Member(term, f);
  sub.Member(term, u);
  sub.Member(term, interval);
  sub.Member(overlap, line);
  sub.Member(overlap, interval);
  sub.Member(overlap, u);
  sub.Member(overlap, v);
  sub.Outside(overlap, f);
  sub.NeighborhoodOf(v, zero);
  sub.NeighborhoodOf(interval, term);
  sub.DeletedReciprocalNeighborhoodOf(v, zero, f, tau);
  sub.UnseparablePointClosedSet(zero, f, tau);
  sub.Intersecting(u, v);
  sub.IntersectionWitness(overlap, u, v, tau);
  return sub.make();
}

export const buildSetSeparationFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: disjointSetNeighborhoods(),
    sty: separationAxiomStyle(),
    canvas: canvas(342, 163),
    variation: "gemignani-abstract-set-separation",
    ...renderOptions,
  });
export const buildTopologicalPointSeparationFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: topologicalPointClosedSeparation(),
    sty: separationAxiomStyle(),
    canvas: canvas(297, 178),
    variation: "gemignani-topological-point-separation",
    ...renderOptions,
  });
export const buildClosureNeighborhoodFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: nestedClosureNeighborhoods(),
    sty: closureNeighborhoodStyle(),
    canvas: canvas(263, 199),
    variation: "gemignani-neighborhood-closure-witness",
    ...renderOptions,
  });
export const buildDeletedReciprocalFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: deletedReciprocalCounterexample(),
    sty: deletedReciprocalStyle(),
    canvas: canvas(333, 70),
    variation: "gemignani-deleted-reciprocal-counterexample",
    ...renderOptions,
  });

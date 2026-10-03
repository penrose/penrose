import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  dyadicNeighborhoodStyle,
  dyadicRefinementValues,
  dyadicValue,
  pointSetTopology as topology,
} from "@penrose/bloom";

/** A finite displayed excerpt of the infinite dyadic Urysohn family. */
export function dyadicSeparatedClosedSets() {
  const sub = topology.substance();
  const space = sub.Set({ label: "X" }),
    tau = sub.Topology({ label: "\\tau" });
  const a = sub.ClosedRegion({ label: "A" }),
    b = sub.ClosedRegion({ label: "B" }),
    family = sub.DyadicOpenFamily({ label: "U" });
  sub.TopologyOn(tau, space);
  sub.T4(tau);
  sub.ClosedIn(a, tau);
  sub.ClosedIn(b, tau);
  sub.Nonempty(a);
  sub.Nonempty(b);
  sub.Disjoint(a, b);
  sub.Subset(a, space);
  sub.Subset(b, space);
  sub.DyadicFamilyFor(family, a, b, tau);
  const q = sub.DyadicRational({
      label: "q",
      numerator: 1,
      exponent: 2,
      coordinate: dyadicValue(1, 2),
    }),
    qp = sub.DyadicRational({
      label: "q'",
      numerator: 3,
      exponent: 2,
      coordinate: dyadicValue(3, 2),
    });
  const u = sub.OpenSet({ label: "U(q)" }),
    next = sub.OpenSet({ label: "U(q')" }),
    closure = sub.ClosedRegion({ label: "\\operatorname{Cl}U(q)" });
  for (const [open, index] of [
    [u, q],
    [next, qp],
  ] as const) {
    sub.DyadicIndexOf(open, index, family);
    sub.SetInFamily(open, family);
    sub.OpenIn(open, tau);
    sub.Subset(a, open);
    sub.Subset(open, space);
    sub.Disjoint(b, open);
  }
  sub.ClosureOf(closure, u, tau);
  sub.ClosedIn(closure, tau);
  sub.Subset(u, closure);
  sub.Subset(closure, next);
  sub.DyadicSeparationStep(family, u, closure, next);
  return sub.make();
}

/** The new n/2^(k+1) index lies between the already constructed neighbors. */
export function dyadicRefinementNeighborhoods(
  options: { numerator?: number; exponent?: number } = {},
) {
  const numerator = options.numerator ?? 3,
    exponent = options.exponent ?? 2;
  const values = dyadicRefinementValues(numerator, exponent);
  const sub = topology.substance();
  const space = sub.Set({ label: "X" }),
    tau = sub.Topology({ label: "\\tau" });
  const a = sub.ClosedRegion({ label: "A" }),
    b = sub.ClosedRegion({ label: "B" }),
    family = sub.DyadicOpenFamily({ label: "U" });
  sub.TopologyOn(tau, space);
  sub.T4(tau);
  sub.Nonempty(a);
  sub.Nonempty(b);
  sub.ClosedIn(a, tau);
  sub.ClosedIn(b, tau);
  sub.Disjoint(a, b);
  sub.Subset(a, space);
  sub.Subset(b, space);
  sub.DyadicFamilyFor(family, a, b, tau);
  const expressions = [
    "\\frac{n-1}{2^{k+1}}",
    "\\frac{n}{2^{k+1}}",
    "\\frac{n+1}{2^{k+1}}",
  ];
  const indices = values.map((coordinate, i) =>
    sub.DyadicRational({
      label: expressions[i],
      numerator: numerator - 1 + i,
      exponent: exponent + 1,
      coordinate,
    }),
  );
  const opens = expressions.map((q) =>
    sub.OpenSet({ label: `U\\left(${q}\\right)` }),
  );
  for (const [i, open] of opens.entries()) {
    sub.DyadicIndexOf(open, indices[i], family);
    sub.SetInFamily(open, family);
    sub.OpenIn(open, tau);
    sub.Subset(a, open);
    sub.Subset(open, space);
    sub.Disjoint(b, open);
  }
  const prevClosure = sub.ClosedRegion({
      label: `\\operatorname{Cl}${opens[0].label}`,
    }),
    middleClosure = sub.ClosedRegion({
      label: `\\operatorname{Cl}${opens[1].label}`,
    });
  sub.ClosureOf(prevClosure, opens[0], tau);
  sub.ClosureOf(middleClosure, opens[1], tau);
  sub.ClosedIn(prevClosure, tau);
  sub.ClosedIn(middleClosure, tau);
  sub.Subset(opens[0], prevClosure);
  sub.Subset(prevClosure, opens[1]);
  sub.Subset(opens[1], middleClosure);
  sub.Subset(middleClosure, opens[2]);
  sub.DyadicRefinementStep(
    family,
    opens[0],
    prevClosure,
    opens[1],
    middleClosure,
    opens[2],
  );
  return sub.make();
}

export const buildDyadicSeparationFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: dyadicSeparatedClosedSets(),
    sty: dyadicNeighborhoodStyle(),
    canvas: canvas(318, 221),
    variation: "gemignani-dyadic-separated-closed-sets",
    ...renderOptions,
  });
export const buildDyadicRefinementFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: dyadicRefinementNeighborhoods(),
    // Preserve the graphic's literal exponent typo; Substance follows the ordered proof.
    sty: dyadicNeighborhoodStyle({
      middleLabel: "U\\left(\\frac{n}{2^k}\\right)",
    }),
    canvas: canvas(345, 293),
    variation: "gemignani-dyadic-induction-step",
    ...renderOptions,
  });

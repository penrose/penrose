import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { setTheory as sets } from "../domains/set-theory.js";
import {
  constraintTopologyStyles,
  type ConstraintTopologyStyleOptions,
} from "../styles/constraint-topology.js";

export type ConstraintTopologyCase =
  | "book-separation"
  | "book-nested"
  | "neighborhood-triangle"
  | "bipartite-map"
  | "L-membership"
  | "unhinted-composition"
  | "inconsistent";

/** Geometry belongs to Style hints; these constructors record only mathematics. */
export function constraintTopologySubstance(example: ConstraintTopologyCase) {
  const sub = sets.substance();
  if (example === "book-separation") {
    const U = sub.Set({ label: "U" }),
      V = sub.Set({ label: "V" });
    const x = sub.Point({ label: "x" }),
      y = sub.Point({ label: "y" });
    sub.Member(x, U);
    sub.Member(y, V);
    sub.Disjoint(U, V);
  } else if (example === "book-nested") {
    const U = sub.Set({ label: "U" }),
      V = sub.Set({ label: "V" }),
      W = sub.Set({ label: "W" });
    const x = sub.Point({ label: "x" });
    sub.Subset(W, V);
    sub.Subset(V, U);
    sub.Member(x, W);
  } else if (example === "neighborhood-triangle") {
    const X = sub.Set({ label: "X" });
    const neighborhoods = ["U", "V", "W"].map((label) => sub.Set({ label }));
    const points = ["x", "y", "z"].map((label) => sub.Point({ label }));
    const R = sub.BinaryRelation({ label: "R" });
    sub.RelationOn(R, X);
    neighborhoods.forEach((U, i) => {
      sub.Subset(U, X);
      sub.Member(points[i], U);
      sub.Member(points[i], X);
    });
    for (let i = 0; i < neighborhoods.length; i++)
      for (let j = i + 1; j < neighborhoods.length; j++)
        sub.Disjoint(neighborhoods[i], neighborhoods[j]);
    sub.RelatedUnder(R, points[0], points[1]);
    sub.RelatedUnder(R, points[1], points[2]);
    sub.RelatedUnder(R, points[2], points[0]);
  } else if (example === "bipartite-map") {
    const S = sub.Set({ label: "S" }),
      T = sub.Set({ label: "T" });
    const f = sub.Function({ label: "f" });
    sub.MapBetween(f, S, T);
    sub.Disjoint(S, T);
    const targets = ["u", "v"].map((label) => sub.Point({ label }));
    targets.forEach((p) => sub.Member(p, T));
    ["a", "b", "c", "d"].forEach((label, i) => {
      const p = sub.Point({ label });
      sub.Member(p, S);
      sub.MapsTo(f, p, targets[i % 2]);
    });
  } else if (example === "L-membership") {
    const L = sub.Set({ label: "L" }),
      A = sub.Set({ label: "A" }),
      B = sub.Set({ label: "B" });
    sub.Subset(A, L);
    sub.Subset(B, L);
    sub.Disjoint(A, B);
    const x = sub.Point({ label: "x" }),
      y = sub.Point({ label: "y" }),
      z = sub.Point({ label: "z" });
    sub.Member(x, A);
    sub.Member(y, B);
    sub.Member(z, L);
    const R = sub.BinaryRelation({ label: "R" });
    sub.RelationOn(R, L);
    sub.RelatedUnder(R, x, z);
    sub.RelatedUnder(R, z, y);
  } else if (example === "unhinted-composition") {
    const P = sub.Set({ label: "P" }),
      Q = sub.Set({ label: "Q" }),
      S = sub.Set({ label: "S" });
    const x = sub.Point({ label: "x" }),
      y = sub.Point({ label: "y" }),
      z = sub.Point({ label: "z" });
    sub.Member(x, P);
    sub.Member(x, Q);
    sub.Member(y, Q);
    sub.Member(z, S);
    sub.Intersecting(P, Q);
    sub.Disjoint(P, S);
    sub.Disjoint(Q, S);
    const H = sub.BinaryRelation({ label: "H" });
    sub.RelatedUnder(H, x, y);
    sub.RelatedUnder(H, y, z);
  } else if (example === "inconsistent") {
    const A = sub.Set({ label: "A" }),
      B = sub.Set({ label: "B" });
    sub.Subset(B, A);
    sub.Disjoint(A, B);
    const x = sub.Point({ label: "x" });
    sub.Member(x, B);
  } else throw new Error(`Unknown constraint topology case ${example}`);
  return sub.make();
}

export const constraintTopologyHints: Record<
  ConstraintTopologyCase,
  ConstraintTopologyStyleOptions
> = {
  "book-separation": {
    regions: {
      U: { center: [-105, 10], size: 78 },
      V: { center: [105, 10], size: 78 },
    },
    points: { x: [-112, 4], y: [112, 4] },
  },
  "book-nested": {
    regions: {
      U: { center: [0, 0], size: 150 },
      V: { center: [12, 5], size: 103 },
      W: { center: [25, 5], size: 48 },
    },
    points: { x: [23, 3] },
  },
  "neighborhood-triangle": {
    regions: {
      X: { center: [0, 0], size: 218 },
      U: { center: [-105, -70], size: 58 },
      V: { center: [105, -70], size: 58 },
      W: { center: [0, 105], size: 58 },
    },
    points: { x: [-105, -70], y: [105, -70], z: [0, 105] },
  },
  "bipartite-map": {
    regions: {
      S: { center: [-145, 0], size: 105 },
      T: { center: [145, 0], size: 90 },
    },
    points: {
      a: [-170, 55],
      b: [-130, 25],
      c: [-170, -25],
      d: [-130, -55],
      u: [145, 45],
      v: [145, -45],
    },
  },
  "L-membership": {
    regions: {
      L: { center: [0, 0], size: [330, 330], kind: "L" },
      A: { center: [-105, 70], size: 40 },
      B: { center: [80, -100], size: 40 },
    },
    points: { x: [-105, 70], y: [80, -100], z: [-105, -95] },
  },
  "unhinted-composition": {},
  inconsistent: {
    regions: {
      A: { center: [0, 0], size: 130 },
      B: { center: [15, 0], size: 60 },
    },
    points: { x: [15, 0] },
  },
};

export async function buildConstraintTopology(
  example: ConstraintTopologyCase,
  options: ConstraintTopologyStyleOptions = {},
) {
  const substance = constraintTopologySubstance(example);
  const styles = constraintTopologyStyles({
    ...constraintTopologyHints[example],
    ...options,
  });
  const drawing = await diagram({
    sub: substance,
    sty: [styles.regions, styles.relations],
    canvas: canvas(720, 540),
    variation: options.seed ?? "book",
  });
  return { drawing, substance, styles };
}

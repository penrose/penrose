/** @jsxImportSource @penrose/bloom */
// Gallery translation of: set-theory-domain/tree-euler

import type { Diagram } from "@penrose/bloom";
import {
  DiagramBuilder,
  canvas,
  constraints,
  objectives,
} from "@penrose/bloom";

const SEED = "MonsoonCaterpillar95943";

export const buildDiagram = async (variation = SEED): Promise<Diagram> => {
  const {
    ensure,
    encourage,
    build,
    type,
    predicate,
    forall,
    layer,
    forallWhere,
  } = new DiagramBuilder(canvas(800, 700), variation);

  const Set = type();
  const IsSubset = predicate();
  const Disjoint = predicate();

  const A = Set();
  const B = Set();
  const C = Set();
  const D = Set();
  const E = Set();
  const F = Set();
  const G = Set();

  A.label = "A";
  B.label = "B";
  C.label = "C";
  D.label = "D";
  E.label = "E";
  F.label = "F";
  G.label = "G";

  IsSubset(B, A);
  IsSubset(C, A);
  IsSubset(D, B);
  IsSubset(E, B);
  IsSubset(F, C);
  IsSubset(G, C);

  Disjoint(E, D);
  Disjoint(F, G);
  Disjoint(B, C);

  forall({ x: Set }, ({ x }) => {
    x.icon = <circle />;
    x.text = <equation string={x.label} font-size={"32px"} />;

    ensure(constraints.contains(x.icon, x.text));
    encourage(objectives.sameCenter(x.text, x.icon));
    layer(x.icon, x.text);
  });

  forallWhere(
    { x: Set, y: Set },
    ({ x, y }) => IsSubset.test(x, y),
    ({ x, y }) => {
      ensure(constraints.contains(y.icon, x.icon, 5));
      ensure(constraints.disjoint(y.text, x.icon, 10));
      layer(y.icon, x.icon);
    },
  );

  forall({ x: Set, y: Set }, ({ x, y }) => {
    if (Disjoint.test(x, y)) {
      ensure(constraints.disjoint(x.icon, y.icon));
    }
  });

  return await build();
};

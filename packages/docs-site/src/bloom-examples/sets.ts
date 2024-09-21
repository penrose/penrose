import {
  DiagramBuilder,
  canvas,
  constraints,
  objectives,
} from "@penrose/bloom";

export const sets = async () => {
  const {
    circle,
    equation,
    ensure,
    encourage,
    build,
    type,
    predicate,
    forall,
    layer,
  } = new DiagramBuilder(canvas(800, 700), "abcd");

  // domain
  const Set = type();
  const IsSubset = predicate();
  const IsDisjoint = predicate();

  // substance
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

  IsSubset(A, B);
  IsSubset(A, C);

  IsSubset(B, D);
  IsSubset(B, E);

  IsSubset(C, F);
  IsSubset(C, G);

  IsDisjoint(B, C);
  IsDisjoint(D, E);
  IsDisjoint(F, G);

  // style
  forall({ s: Set }, ({ s }) => {
    s.icon = circle({
      drag: true,
    });

    s.text = equation({
      string: s.label,
      fontSize: "32px",
    });

    layer(s.icon, s.text);

    encourage(objectives.near(s.icon, s.text));
    ensure(constraints.contains(s.icon, s.text, 5));
  });

  forall({ a: Set, b: Set }, ({ a, b }) => {
    if (IsSubset.test(a, b)) {
      ensure(constraints.contains(a.icon, b.icon, 10));
      ensure(constraints.disjoint(a.text, b.icon, 5));
    }
  });

  forall({ a: Set, b: Set }, ({ a, b }) => {
    if (IsDisjoint.test(a, b)) {
      ensure(constraints.disjoint(a.icon, b.icon, 10));
    }
  });

  return await build();
};

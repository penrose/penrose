import { makeCanvas } from "@penrose/core";
import { DiagramBuilder } from "../builder/builder.js";
import constraints from "../builder/constraints.ts";
import objectives from "../builder/objectives.ts";

export const sets = async () => {
  const {
    vary,
    circle,
    equation,
    ensure,
    encourage,
    build,
    type,
    predicate,
    forall,
    where,
    test,
    layer,
  } = new DiagramBuilder(makeCanvas(800, 700), "abcd");

  // domain
  const Set = type();
  const IsSubset = predicate();
  const IsDisjoint = predicate();

  // substance
  const A = Set("A");
  const B = Set("B");
  const C = Set("C");
  const D = Set("D");
  const E = Set("E");
  const F = Set("F");
  const G = Set("G");

  IsSubset(A, B);
  IsSubset(A, C);

  IsSubset(B, D);
  IsSubset(B, E);

  IsSubset(C, F);
  IsSubset(C, G);

  IsDisjoint(B, C);

  // style
  forall({ s: Set }, ({ s }) => {
    s.icon = circle();
    s.label = equation({
      string: s.name,
    });

    layer(s.icon, s.label);

    encourage(objectives.near(s.icon, s.label));
    ensure(constraints.contains(s.icon, s.label, 5));
  });

  forall({ a: Set, b: Set }, ({ a, b }) => {
    if (IsSubset.test(a, b)) {
      ensure(constraints.contains(a.icon, b.icon, 10));
      ensure(constraints.disjoint(a.label, b.icon, 5));
    }
  });

  forall({ a: Set, b: Set }, ({ a, b }) => {
    if (IsDisjoint.test(a, b)) {
      ensure(constraints.disjoint(a.icon, b.icon, 10));
    }
  });

  return await build();
};

import { makeCanvas, ops } from "@penrose/core";
import { DiagramBuilder } from "../builder/builder.js";
import { Circle, Equation } from "../builder/types.js";
import constraints from "../builder/constraints.ts";
import objectives from "../builder/objectives.ts";

export const sets = async () => {
  const { vary, circle, equation, ensure, encourage, build }
    = new DiagramBuilder(makeCanvas(800, 700), "a");

  class Set {
    icon: Circle;
    label: Equation

    constructor(name: string) {
      const r = vary(`${name}.r`);
      this.icon = circle({
        r,
      });
      this.label = equation({
        string: name,
        fontSize: "32px"
      });

      encourage(objectives.equal(ops.vnorm(ops.vsub(this.icon.center, this.label.center)), 0));
      ensure(constraints.contains(this.icon, this.label));
      ensure(constraints.greaterThan(this.icon.r, 20))
    }
  }

  const disjoint = (A: Set, B: Set) => {
    ensure(constraints.disjoint(A.icon, B.icon, 10));
  }

  const subset = (A: Set, B: Set) => {
    ensure(constraints.contains(A.icon, B.icon, 5));
    ensure(constraints.disjoint(A.label, B.icon, 10));
  }

  const A = new Set("A");
  const B = new Set("B");
  const C = new Set("C");
  const D = new Set("D");
  const E = new Set("E");
  const F = new Set("F");
  const G = new Set("G");

  subset(A, B);
  subset(A, C);

  subset(B, D);
  subset(B, E);

  subset(C, F);
  subset(C, G);

  disjoint(B, C);
  disjoint(D, E);
  disjoint(F, G);

  return await build();
}
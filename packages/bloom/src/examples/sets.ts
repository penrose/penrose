import { add, constrDict, makeCanvas, max, mul, objDict, ops, sub } from "@penrose/core";
import { DiagramBuilder } from "../builder/builder.js";
import { Circle, Equation } from "../builder/types.js";
import { constraints } from "../builder/constraints.ts";

export const sets = async () => {
  const { vary, circle, equation, ensure, encourage, build }
    = new DiagramBuilder(makeCanvas(400, 400), "asddf");

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
      });

      encourage(ops.vnormsq(ops.vsub(this.icon.center, this.label.center)));
      ensure(constraints.contains(this.icon, this.label, 10));
      ensure(constraints.greaterThan(this.icon.r, 20))
    }
  }

  const disjoint = (A: Set, B: Set) => {
    ensure(constraints.disjoint(A.icon, B.icon, 10));
  }

  const contains = (A: Set, B: Set) => {
    ensure(constraints.contains(A.icon, B.icon, 10));
  }

  const A = new Set("A");
  const B = new Set("B");
  const C = new Set("C");

  contains(A, B);
  contains(A, C);

  disjoint(B, C);

  ensure(constraints.disjoint(A.label, B.icon, 10));
  ensure(constraints.disjoint(A.label, C.icon, 10));

  return await build();
}
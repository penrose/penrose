import { add, constrDict, makeCanvas, objDict, ops, sub } from "@penrose/core";
import { DiagramBuilder } from "../builder/builder.js";
import { Circle, Equation } from "../builder/types.js";

export const sets = async () => {
  const { circle, equation, ensure, encourage, build }
    = new DiagramBuilder(makeCanvas(400, 400), "a");

  class Set {
    icon: Circle;
    label: Equation

    constructor(name: string) {
      this.icon = circle();
      this.label = equation({
        string: name,
      });
      encourage(objDict.nearVec.body(this.icon.center, this.label.center, 0).value);
    }
  }

  const disjoint = (A: Set, B: Set) => {
    ensure(sub(add(A.icon.r, B.icon.r), ops.vdist(A.icon.center, B.icon.center)));
  }

  const contains = (A: Set, B: Set) => {
    ensure(sub(add(ops.vdist(A.icon.center, B.icon.center), B.icon.r), A.icon.r));
  }

  const A = new Set("A");
  const B = new Set("B");
  // const C = new Set("C");

  // contains(A, B);
  // contains(A, C);
  //
  // disjoint(B, C);

  return await build();
}
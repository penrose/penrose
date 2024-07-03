import { add, makeCanvas, mul, ops, sub } from "@penrose/core";
import { DiagramBuilder } from "../builder/builder.js";
import { Circle } from "../builder/types.js";

export const circles = async () => {
  const { circle, ensure, build } = new DiagramBuilder(
    makeCanvas(400, 400),
    "simple"
  );

  const circles: Readonly<Circle>[] = [];

  for (let i = 0; i < 10; i++) {
    const circ = circle();
    circles.push(circ);

    const r = circ.r;
    ensure(mul(sub(r, 10), sub(r, 20)));
  }

  for (let i = 0; i < circles.length - 1; i++) {
    for (let j = i + 1; j < circles.length; j++) {
      const c1 = circles[i].center;
      const c2 = circles[j].center;

      const r1 = circles[i].r;
      const r2 = circles[j].r;

      ensure(sub(add(r1, r2), ops.vdist(c1, c2)));
    }
  }

  const diagram = await build();
  return diagram;
};

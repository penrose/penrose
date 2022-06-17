import { genCode, input, secondaryGraph } from "engine/Autodiff";
import seedrandom from "seedrandom";
import { makeCircle } from "shapes/Circle";
import {
  Context,
  InputFactory,
  makeCanvas,
  simpleContext,
} from "shapes/Samplers";
import * as ad from "types/ad";
import { black, floatV, vectorV } from "utils/Util";
import { compDict } from "./Functions";

const canvas = makeCanvas(800, 700);
let globalKey = 0;
// const makeInput: InputFactory = (meta) => {
//   const rng = seedrandom("sdf");
//   const x = input({
//     key: globalKey++,
//     val: "pending" in meta ? meta.pending : meta.sampler(rng),
//   });
//   return x;
// };

const makeInputArray = (pt: number[]): ad.Input[] => {
  const inputs: ad.Input[] = [];
  const makeInput: InputFactory = (meta) => {
    const rng = seedrandom("sdf");
    const x = input({
      key: inputs.length,
      val: "pending" in meta ? meta.pending : meta.sampler(rng),
    });
    inputs.push(x);
    return x;
  };
  for (const coord of pt) {
    makeInput({ sampler: () => coord });
  }
  return [...inputs];
};

const compareClosestPoint = (
  //
  context: Context,
  shapeType: string,
  shape: any,
  pt: [ad.Input, ad.Input],
  expected: [number, number]
) => {
  const result = compDict.closestPoint(context, [shapeType, shape], pt);
  const g = secondaryGraph(result.contents);
  const f = genCode(g);
  const [x, y] = f([
    pt[0].val,
    pt[1].val,
    shape.center.contents[0].val,
    shape.center.contents[1].val,
    shape.r.contents.val,
  ]).secondary;
  expect(x).toBeCloseTo(expected[0]);
  expect(y).toBeCloseTo(expected[1]);
};

export const testCircle = (
  center: number[],
  radius: number,
  strokeWidth: number,
  pt: [number, number],
  expected: [number, number]
) => {
  const p = makeInputArray([pt[0], pt[1], center[0], center[1], radius]);
  const context = simpleContext("circle");
  const shape = makeCircle(context, canvas, {
    center: vectorV([p[2], p[3]]),
    r: floatV(p[4]),
    strokeWidth: floatV(strokeWidth),
    strokeColor: black(),
  });
  compareClosestPoint(context, "Circle", shape, [p[0], p[1]], expected);
};

describe("closest point", () => {
  test("circle", () => {
    testCircle([0, 0], 3, 0, [3, 0], [3, 0]);
  });
});

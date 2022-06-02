import { genCode, input, secondaryGraph } from "engine/Autodiff";
import seedrandom from "seedrandom";
import { makeCircle } from "shapes/Circle";
import { Context, InputFactory, makeCanvas } from "shapes/Samplers";
import * as ad from "types/ad";
import { Shape } from "types/shapes";
import { VectorV } from "types/value";
import { black, floatV, vectorV } from "utils/Util";
import { compDict } from "./Functions";

const canvas = makeCanvas(800, 700);

const compareClosestPoint = (
  context: Context,
  shapeType: string,
  shape: Shape,
  p: ad.Input[],
  expected: [number, number]
) => {
  const result = getResult(context, shapeType, shape, p);
  console.log(
    `result contents 0: ${JSON.stringify(result.contents[0], null, 2)}`
  );
  console.log(
    `result contents 1: ${JSON.stringify(result.contents[1], null, 2)}`
  );
  const g = secondaryGraph(result.contents);
  const f = genCode(g);
  const foo = f([]).secondary;
  console.log(`foo 0: ${JSON.stringify(foo[0], null, 2)}`);
  console.log("type of foo 0: " + typeof foo[0]);
  if (typeof foo[0] === null) {
    console.log("error is NULL");
  }
  if (typeof foo[0] === undefined) {
    console.log("error is undefined");
  }
  console.log(`foo: ${JSON.stringify(foo, null, 2)}`);
  //   expect(x).toBeCloseTo(expected[0]);
  //   expect(y).toBeCloseTo(expected[1]);
};

const getResult = (
  context: Context,
  shapeType: string,
  s: any,
  p: ad.Input[]
): VectorV<ad.Num> => {
  const result = compDict.closestPoint(context, [shapeType, s], p);
  return result;
};
export const testCircle = (
  center: number[],
  radius: number,
  strokeWidth: number,
  pt: number[],
  expected: [number, number]
) => {
  const { context, p } = makeContext(pt);
  const shape = makeCircle(context, canvas, {
    center: vectorV(center),
    r: floatV(radius),
    strokeWidth: floatV(strokeWidth),
    strokeColor: black(),
  });
  compareClosestPoint(context, "Circle", shape, p, expected);
};

describe("closest point", () => {
  test("circle", () => {
    // testCircle([0, 0], 3, 0, [0, 0], -3);
    //test
    testCircle([0, 0], 3, 0, [3, 0], [3, 0]);
    // testCircle([0, 0], 3, 0, [4, 0], [3, 0]);
    // testCircle([0, 0], 3, 0, [-5, 0], [-3, 0]);
  });
});
const makeContext = (pt: number[]): { context: Context; p: ad.Input[] } => {
  const rng = seedrandom("sdf");
  const inputs: ad.Input[] = [];
  const makeInput: InputFactory = (meta) => {
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
  return { context: { makeInput }, p: [...inputs] };
};

import { genCode, makeADInputVars, secondaryGraph } from "engine/Autodiff";
import seedrandom from "seedrandom";
import { makeCircle } from "shapes/Circle";
import { floatV, makeCanvas, sampleBlack, vectorV } from "shapes/Samplers";
import * as ad from "types/ad";
import { Shape } from "types/shapes";
import { VectorV } from "types/value";
import { compDict } from "./Functions";

const canvas = makeCanvas(800, 700);

const compareClosestPoint = (
  shapeType: string,
  shape: Shape,
  p: ad.Input[],
  expected: [number, number]
) => {
  const result = getResult(shapeType, shape, p);
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
  shapeType: string,
  s: any,
  p: ad.Input[]
): VectorV<ad.Num> => {
  const result = compDict.closestPoint(
    { rng: seedrandom("shape") },
    [shapeType, s],
    p
  );
  //   console.log(
  //     `closestPoint(${seedrandom("shape")},[${shapeType},${JSON.stringify(
  //       s
  //     )}],${p})
  //   ) = ${JSON.stringify(result, null, 2)}`
  //   );
  return result;
};
export const testCircle = (
  center: number[],
  radius: number,
  strokeWidth: number,
  pt: number[],
  expected: [number, number]
) => {
  const seed = seedrandom("bbox Rectangle");
  const shape = makeCircle(seed, canvas, {
    center: vectorV(center),
    r: floatV(radius),
    strokeWidth: floatV(strokeWidth),
    strokeColor: sampleBlack(),
  });
  compareClosestPoint("Circle", shape, makeADInputVars(pt), expected);
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

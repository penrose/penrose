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
  const g = secondaryGraph([result.contents[0], result.contents[1]]);
  //const g = secondaryGraph([result.contents]);
  const f = genCode(g);
  /* const [dist] = 
    const {
      secondary: [dist],
      stmts,
    } = f([]); // no inputs, so, empty array
    const code = stmts.join("\n");
    console.log(code); */
  //TODO: debug gradient for ellipse
  // the commented code in the next three lines is useful for debugging
  // gradients
  //const newfun = (xs: number[]) => f(xs).primary;
  //const foo = _gradFiniteDiff(newfun)([p[0].val, p[1].val]);
  //console.log("symbolic gradient", gradient, "computed gradient:", foo);
  const [x, y] = f([]).secondary;
  expect(x).toBeCloseTo(expected[0]);
  expect(y).toBeCloseTo(expected[1]);
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
    testCircle([0, 0], 3, 0, [3, 0], [3, 0]);
    testCircle([0, 0], 3, 0, [4, 0], [3, 0]);
    testCircle([0, 0], 3, 0, [-5, 0], [-3, 0]);
  });
});

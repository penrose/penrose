import { genCode, secondaryGraph } from "engine/Autodiff";
import { makeCircle } from "shapes/Circle";
import { Context, makeCanvas, simpleContext } from "shapes/Samplers";
import { Shape } from "types/shapes";
import { black, floatV, vectorV } from "utils/Util";
import { compDict } from "./Functions";

const canvas = makeCanvas(800, 700);

const compareClosestPoint = (
  context: Context,
  shapeType: string,
  shape: Shape,
  pt: [number, number],
  expected: [number, number]
) => {
  const result = compDict.closestPoint(context, [shapeType, shape], pt);
  const g = secondaryGraph(result.contents);
  const f = genCode(g);
  const [x, y] = f([]).secondary;
  expect(x).toBeCloseTo(expected[0]);
  expect(y).toBeCloseTo(expected[1]);
};

const compareAddPt = (
  shapeType: string,
  shape: any,
  p1: [number, number],
  p2: [number, number],
  expected: [number, number]
) => {
  const context = simpleContext("add pt");
  const result = compDict.addPtsTest(context, [shapeType, shape], p1, p2);
  const g = secondaryGraph(result.contents);
  const f = genCode(g);
  const [x, y] = f([]).secondary;
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
  const context = simpleContext("circle");
  const shape = makeCircle(context, canvas, {
    center: vectorV(center),
    r: floatV(radius),
    strokeWidth: floatV(strokeWidth),
    strokeColor: black(),
  });
  compareClosestPoint(context, "Circle", shape, pt, expected);
};

describe("closest point", () => {
  test("addPt", () => {
    compareAddPt("Circle", null, [1, 1], [2, 2], [3, 3]);
    compareAddPt("Circle", null, [1, 1], [3, 3], [4, 4]);
  });

  test("circle", () => {
    testCircle([0, 0], 3, 0, [3, 0], [3, 0]);
    testCircle([0, 0], 3, 0, [4, 0], [3, 0]);
    testCircle([0, 0], 3, 0, [-5, 0], [-3, 0]);
  });
});

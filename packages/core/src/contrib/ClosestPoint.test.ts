import { genCode, secondaryGraph } from "engine/Autodiff";
import { makeCircle } from "shapes/Circle";
import { makeRectangle } from "shapes/Rectangle";
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

export const testRectangle = (
  center: number[],
  width: number,
  height: number,
  strokeWidth: number,
  pt: [number, number],
  expected: [number, number]
) => {
  const context = simpleContext("rectangle");
  const shape = makeRectangle(context, canvas, {
    center: vectorV(center),
    width: floatV(width),
    height: floatV(height),
    strokeWidth: floatV(strokeWidth),
    strokeColor: black(),
  });
  compareClosestPoint(context, "Rectangle", shape, pt, expected);
};

describe("closest point", () => {
  test("circle", () => {
    testCircle([0, 0], 3, 0, [3, 0], [3, 0]);
    testCircle([0, 0], 3, 0, [4, 0], [3, 0]);
    testCircle([0, 0], 3, 0, [-5, 0], [-3, 0]);
  });

  test("rectangle", () => {
    testRectangle([0, 0], 4, 4, 0, [2, 2], [2, 2]);
    testRectangle([0, 0], 6, 4, 0, [-6, 0], [-3, 0]);
  });
});

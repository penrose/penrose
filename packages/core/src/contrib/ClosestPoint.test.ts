import { genCode, secondaryGraph } from "engine/Autodiff";
import { makeCircle } from "shapes/Circle";
import { makeLine } from "shapes/Line";
import { makeRectangle } from "shapes/Rectangle";
import { Context, makeCanvas, simpleContext } from "shapes/Samplers";
import { Shape } from "types/shapes";
import { black, floatV, vectorV } from "utils/Util";
import { compDict } from "./Functions";

const canvas = makeCanvas(800, 700);

const compareClosestPoint = (
  //
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
  console.log(x);
  console.log(y);
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

export const testLine = (
  start: number[],
  end: number[],
  strokeWidth: number,
  pt: [number, number],
  expected: [number, number]
) => {
  const context = simpleContext("Line");
  const shape = makeLine(context, canvas, {
    start: vectorV(start),
    end: vectorV(end),
    strokeWidth: floatV(strokeWidth),
    strokeColor: black(),
  });
  compareClosestPoint(context, "Line", shape, pt, expected);
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
    testRectangle([2, 3], 4, 6, 0, [-6, 0], [0, 0]);
    testRectangle([0, 0], 6, 4, 0, [-6, 2], [-3, 2]);
    testRectangle([0, 0], 6, 4, 0, [6, -3], [3, -2]);
    testRectangle([0, 0], 6, 4, 0, [-6, -5], [-3, -2]);
    testRectangle([0, 0], 6, 4, 0, [1, 1], [1, 2]);
    testRectangle([0, 0], 6, 4, 0, [0, 0], [0, 2]); //when input point is at center of rectangle, a point still gets returned.
  });

  test("Line", () => {
    testLine([0, 0], [7, 0], 4, [2, 2], [2, 0]);
    testLine([0, 0], [7, 0], 4, [8, 1], [7, 0]);
    testLine([0, 0], [7, 0], 4, [-1, 1], [0, 0]);
    testLine([0, 0], [6, 3], 4, [1, 3], [2, 1]);
    testLine([7, 0], [0, 0], 4, [2, 2], [2, 0]); //Other orientation of line
    testLine([7, 0], [0, 0], 4, [2, 2], [2, 0]);
    testLine([7, 0], [0, 0], 4, [8, 1], [7, 0]);
    testLine([7, 0], [0, 0], 4, [-1, 1], [0, 0]);
    testLine([6, 3], [0, 0], 4, [1, 3], [2, 1]);
  });
});

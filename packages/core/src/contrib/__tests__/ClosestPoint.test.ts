import { Circle, makeCircle } from "../../shapes/Circle";
import { Ellipse, makeEllipse } from "../../shapes/Ellipse";
import { Line, makeLine } from "../../shapes/Line";
import { Polygon, makePolygon } from "../../shapes/Polygon";
import { Polyline, makePolyline } from "../../shapes/Polyline";
import { makeRectangle } from "../../shapes/Rectangle";
import { Context, makeCanvas, simpleContext } from "../../shapes/Samplers";
import * as ad from "../../types/ad";
import { black, floatV, ptListV, vectorV } from "../../utils/Util";
import { compDict } from "../Functions";
import { Rectlike, numOf } from "../Utils";

const canvas = makeCanvas(800, 700);

/**
 * Constructs a computation graph by calling `closestPoint` on the given `shape`
 * with the given `pt`, then compiles it and calls the resulting function to get
 * numerical outputs, then checks those against `expected`.
 * @param context to pass to `closestPoint`
 * @param shapeType the type of `shape`
 * @param shape to pass to `closestPoint`
 * @param pt to pass to `closestPoint`
 * @param expected to test against the output of `closestPoint`
 */
const compareClosestPoint = async (
  context: Context,
  shape:
    | Circle<ad.Num>
    | Ellipse<ad.Num>
    | Line<ad.Num>
    | Polygon<ad.Num>
    | Polyline<ad.Num>
    | Rectlike<ad.Num>,
  pt: [number, number],
  expected: [number, number]
) => {
  const result = compDict.closestPoint.body(context, shape, pt);
  const [x, y] = result.contents;
  expect(numOf(x)).toBeCloseTo(expected[0]);
  expect(numOf(y)).toBeCloseTo(expected[1]);
};

/**
 * Constructs a `Circle` and tests calling `closestPoint` on it with `pt`.
 * @param center of the circle
 * @param radius of the circle
 * @param strokeWidth of the circle
 * @param pt input to closest-point function
 * @param expected to test against output of closest-point function
 */
const testCircle = (
  center: number[],
  radius: number,
  strokeWidth: number,
  pt: [number, number],
  expected: [number, number]
) => {
  const context = simpleContext("closestPoint Circle");
  const shape = makeCircle(context, canvas, {
    center: vectorV(center),
    r: floatV(radius),
    strokeWidth: floatV(strokeWidth),
    strokeColor: black(),
  });
  compareClosestPoint(context, shape, pt, expected);
};

/**
 * Constructs a `Rectangle` and tests calling `closestPoint` on it with `pt`.
 * @param center of the rectangle
 * @param width of the rectangle
 * @param height of the rectangle
 * @param strokeWidth of the rectangle
 * @param pt input to closest-point function
 * @param expected to test against output of closest-point function
 */
const testRectangle = (
  center: number[],
  width: number,
  height: number,
  strokeWidth: number,
  pt: [number, number],
  expected: [number, number]
) => {
  const context = simpleContext("closestPoint Rectangle");
  const shape = makeRectangle(context, canvas, {
    center: vectorV(center),
    width: floatV(width),
    height: floatV(height),
    strokeWidth: floatV(strokeWidth),
    strokeColor: black(),
  });
  compareClosestPoint(context, shape, pt, expected);
};

/**
 * Constructs a `Line` and tests calling `closestPoint` on it with `pt`.
 * @param start of the line
 * @param end of the line
 * @param strokeWidth of the line
 * @param pt input to closest-point function
 * @param expected to test against output of closest-point function
 */
const testLine = (
  start: number[],
  end: number[],
  strokeWidth: number,
  pt: [number, number],
  expected: [number, number]
) => {
  const context = simpleContext("closestPoint Line");
  const shape = makeLine(context, canvas, {
    start: vectorV(start),
    end: vectorV(end),
    strokeWidth: floatV(strokeWidth),
    strokeColor: black(),
  });
  compareClosestPoint(context, shape, pt, expected);
};

/**
 * Constructs a `Polyline` and tests calling `closestPoint` on it with `pt`.
 * @param points of the polyline
 * @param strokeWidth of the polyline
 * @param pt input to closest-point function
 * @param expected to test against output of closest-point function
 */
const testPolyline = (
  points: number[][],
  strokeWidth: number,
  pt: [number, number],
  expected: [number, number]
) => {
  const context = simpleContext("Polyline");
  const shape = makePolyline(context, canvas, {
    strokeWidth: floatV(strokeWidth),
    points: ptListV(points),
    strokeColor: black(),
  });
  compareClosestPoint(context, shape, pt, expected);
};

/**
 * Constructs a `Polygon` and tests calling `closestPoint` on it with `pt`.
 * @param points of the polygon
 * @param strokeWidth of the polygon
 * @param pt input to closest-point function
 * @param expected to test against output of closest-point function
 */
const testPolygon = (
  points: number[][],
  strokeWidth: number,
  pt: [number, number],
  expected: [number, number]
) => {
  const context = simpleContext("Polygon");
  const shape = makePolygon(context, canvas, {
    strokeWidth: floatV(strokeWidth),
    strokeColor: black(),
    points: ptListV(points),
  });
  compareClosestPoint(context, shape, pt, expected);
};

/**
 * Constructs an `Ellipse` and tests calling `closestPoint` on it with `pt`.
 * @param center of the ellipse
 * @param rx of the ellipse
 * @param ry of the ellipse
 * @param pt input to closest-point function
 * @param expected to test against output of closest-point function
 */
const testEllipse = (
  center: number[],
  rx: number,
  ry: number,
  pt: [number, number],
  expected: [number, number]
) => {
  const context = simpleContext("Ellipse");
  const shape = makeEllipse(context, canvas, {
    center: vectorV(center),
    rx: floatV(rx),
    ry: floatV(ry),
    strokeColor: black(),
  });
  compareClosestPoint(context, shape, pt, expected);
};

// This test suite directly checks the output of `closestPoint` on a few shapes
// with several different examples each.
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
    testRectangle([0, 0], 6, 4, 0, [0, 0], [0, 2]); // when input point is at center of rectangle, a point still gets returned
  });

  test("Line", () => {
    testLine([0, 0], [7, 0], 4, [2, 2], [2, 0]);
    testLine([0, 0], [7, 0], 4, [8, 1], [7, 0]);
    testLine([0, 0], [7, 0], 4, [-1, 1], [0, 0]);
    testLine([0, 0], [6, 3], 4, [1, 3], [2, 1]);
    testLine([7, 0], [0, 0], 4, [2, 2], [2, 0]);
    testLine([7, 0], [0, 0], 4, [2, 2], [2, 0]);
    testLine([7, 0], [0, 0], 4, [8, 1], [7, 0]);
    testLine([7, 0], [0, 0], 4, [-1, 1], [0, 0]);
    testLine([6, 3], [0, 0], 4, [1, 3], [2, 1]);
    testLine([0, 0], [3, 2], 4, [1, 1], [1.153846, 0.7692307692307693]);
    testLine([0, 0], [3, 2], 4, [3, 3], [3, 2]);
    testLine([3, 2], [5, 0], 4, [3, 3], [3, 2]);
  });

  test("Polyline", () => {
    testPolyline(
      [
        [0, 0],
        [3, 2],
        [5, 0],
        [5, -3],
      ],
      4,
      [3, 3],
      [3, 2]
    );
    testPolyline(
      [
        [0, 0],
        [3, 2],
        [5, 0],
        [5, -3],
      ],
      4,
      [5, 2],
      [4, 1]
    );
  });

  test("rectangle as polygon", () => {
    testPolygon(
      [
        [4, 2],
        [4, -2],
        [-4, -2],
        [-4, 2],
      ],
      0,
      [5, 0],
      [4, 0]
    );
    testPolygon(
      [
        [4, 2],
        [4, -2],
        [-4, -2],
        [-4, 2],
      ],
      0,
      [0, 2],
      [0, 2]
    );
  });

  testPolygon(
    [
      [-4, -4],
      [-4, 0],
      [0, 0],
      [0, -4],
    ],
    0,
    [-2, -2],
    [-2, -4]
  );

  test("convex heptagon", () => {
    testPolygon(
      [
        [4, 8],
        [8, 8],
        [8, 0],
        [0, 0],
        [0, 4],
        [4, 4],
      ],
      0,
      [3, 6],
      [4, 6]
    );
  });

  test("ellipse", () => {
    testEllipse([0, 0], 50, 100, [50, 0], [50, 0]);
    testEllipse([0, 0], 100, 50, [0, 100], [0, 50]);
    testEllipse([0, 0], 100, 50, [0, -100], [0, -50]);
    testEllipse([0, 0], 100, 50, [0, 60], [0, 50.0]);
    testEllipse([0, 0], 100, 50, [0, 0], [6.123233995736766e-15, 50.0]);
    testEllipse([0, 0], 100, 50, [0, 10], [6.123233995736766e-15, 50.0]);
    testEllipse([0, 0], 100, 50, [200, 0], [100.0, 0.0]);
    testEllipse([0, 0], 100, 50, [0, 110], [6.123233995736766e-15, 50.0]);
    testEllipse(
      [0, 0],
      100,
      50,
      [200, 200],
      [81.19841273323402, 29.183975781254418]
    );
    testEllipse(
      [0, 0],
      100,
      50,
      [100, 100],
      [69.28204652936475, 36.05550592039633]
    );
    testEllipse(
      [0, 0],
      50,
      100,
      [10, 10],
      [49.61089494163424, 12.493833223000912]
    );
    testEllipse([0, 0], 100, 50, [100, 0], [100, 0]);
    testEllipse([0, 0], 100, 50, [0, 0], [0, 50.0]);
    testEllipse([0, 0], 100, 50, [-100, 0], [-100, 0]); // [-100, 0] -> [0, 100]
    testEllipse([0, 0], 100, 50, [0, -50], [6.123233995736766e-15, -50.0]); // [0, -50] -> [0, 50]
    testEllipse([0, 0], 100, 50, [100, 0], [100, 0]); // [100, 0] -> [0,100]
    testEllipse([0, 0], 100, 50, [0, 50], [0, 50.0]); // fine
    testEllipse(
      [0, 0],
      3,
      3,
      [2.121320343559643, 2.121320343559643],
      [2.121320343559643, 2.121320343559643]
    );
    testEllipse(
      [0, 0],
      50,
      100,
      [-10, -15],
      [-49.11524150621692, -18.728913666946834]
    );
    testEllipse(
      [0, 0],
      50,
      100,
      [-10, -15],
      [-49.11524150621692, -18.728913666946834]
    );
    testEllipse(
      [0, 0],
      50,
      100,
      [20, -30],
      [46.83455310025607, -35.015689963174765]
    );
    testEllipse(
      [0, 0],
      50,
      100,
      [10, -30],
      [46.38787139061665, -37.3183808784684]
    );
    testEllipse(
      [0, 0],
      50,
      100,
      [35, -30],
      [47.35509305915151, -32.093311537396765]
    );
    testEllipse(
      [0, 0],
      50,
      100,
      [-60, 10],
      [-49.77368387088071, 9.511432996109345]
    );
    testEllipse(
      [0, 0],
      50,
      100,
      [-40, 10],
      [-49.72375690607735, 10.513979153705371]
    );
    testEllipse(
      [0, 0],
      50,
      100,
      [80, -30],
      [48.311109867232666, -25.771042927764825]
    );
    testEllipse([0, 0], 50, 100, [0, -100], [0, -100]);
    // testEllipse(
    //   [10, 0],
    //   50,
    //   100,
    //   [0, -90],
    //   [13.524560987414274, -96.27224418486799]
    // );
    // testEllipse(
    //   [10, 100],
    //   50,
    //   100,
    //   [0, -90],
    //   [2.170529302886986, -99.9057316182672]
    // );
    testEllipse([50, 50], 100, 50, [-50, 50], [-50, 50]);
    testEllipse([50, 50], 100, 50, [150, 50], [150, 50]);
    // testEllipse([-100, 200], 100, 50, [-50, 50], [-67.59, 152.6987]);
  });
});

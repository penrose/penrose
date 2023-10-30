import { describe, expect, it, test } from "vitest";
import { ops } from "../../engine/Autodiff.js";
import { sub } from "../../engine/AutodiffFunctions.js";
import { makeCircle } from "../../shapes/Circle.js";
import { makeEllipse } from "../../shapes/Ellipse.js";
import { makeLine } from "../../shapes/Line.js";
import { makePath } from "../../shapes/Path.js";
import { makePolygon } from "../../shapes/Polygon.js";
import { makeRectangle } from "../../shapes/Rectangle.js";
import { makeCanvas, simpleContext } from "../../shapes/Samplers.js";
import { Shape } from "../../shapes/Shapes.js";
import * as ad from "../../types/ad.js";
import { Pt2 } from "../../types/ad.js";
import { black, floatV, ptListV, vectorV } from "../../utils/Util.js";
import { compDict } from "../Functions.js";
import {
  bboxFromShape,
  bboxPts,
  convexPolygonOriginSignedDistance,
  outwardUnitNormal,
  polygonLikePoints,
  shapeCenter,
  shapeDistancePolys,
  shapeDistanceRects,
} from "../Queries.js";
import { numOf, numsOf } from "../Utils.js";
import { _rectangles } from "../__testfixtures__/TestShapes.input.js";

const context = simpleContext("Queries");
const canvas = makeCanvas(800, 700);
const precisionDigits = 10;

const shapes: Shape<ad.Num>[] = [
  // shapes[0]
  makeRectangle(context, canvas, {
    center: vectorV([11, 22]),
    width: floatV(44),
    height: floatV(44),
    strokeWidth: floatV(0),
    strokeColor: black(),
  }),
  // shapes[1]
  makeCircle(context, canvas, {
    r: floatV(22),
    center: vectorV([11, 22]),
    strokeWidth: floatV(0),
    strokeColor: black(),
  }),
  // shapes[2]
  makeEllipse(context, canvas, {
    rx: floatV(22),
    ry: floatV(22),
    center: vectorV([11, 22]),
    strokeWidth: floatV(0),
    strokeColor: black(),
  }),
  // shapes[3]
  makePath(context, canvas, {
    d: compDict.pathFromPoints.body(context, "open", [
      [-11, 0],
      [33, 0],
      [33, 44],
    ]).value,
  }),
  // shapes[4]
  makeLine(context, canvas, {
    start: vectorV([-11, 0]),
    end: vectorV([33, 44]),
    strokeWidth: floatV(0),
  }),
  // shapes[5]
  makePolygon(context, canvas, {
    points: ptListV([
      [-11, 0],
      [33, 0],
      [33, 44],
    ]),
    scale: floatV(1),
  }),
];

describe("simple queries", () => {
  it.each(shapes)("bboxFromShape for %p", (shape: Shape<ad.Num>) => {
    const bbox = bboxFromShape(shape);
    const [x, y, w, h] = numsOf([
      bbox.center[0],
      bbox.center[1],
      bbox.width,
      bbox.height,
    ]);
    expect(x).toBeCloseTo(11, precisionDigits);
    expect(y).toBeCloseTo(22, precisionDigits);
    expect(w).toBeCloseTo(44, precisionDigits);
    expect(h).toBeCloseTo(44, precisionDigits);
  });

  it.each(shapes)("shapeCenter for %p", (shape: Shape<ad.Num>) => {
    const center = shapeCenter(shape);
    const [x, y] = numsOf([center[0], center[1]]);
    expect(x).toBeCloseTo(11, precisionDigits);
    expect(y).toBeCloseTo(22, precisionDigits);
  });
});

describe("polygonLikePoints", () => {
  const ptsToNums = (result: Pt2[]): [number, number][] => {
    const outputs = [];
    for (const pt of result) {
      outputs.push(...pt);
    }
    const nums = numsOf(outputs);
    const pts: [number, number][] = [];
    for (let i = 0; i < nums.length; i += 2) {
      pts.push([nums[i], nums[i + 1]]);
    }
    return pts;
  };

  test("Rectangle shape", async () => {
    const result = ptsToNums(polygonLikePoints(shapes[0]));
    expect(result.length).toEqual(4);
    expect(result[0]).toEqual([33, 44]);
    expect(result[1]).toEqual([-11, 44]);
    expect(result[2]).toEqual([-11, 0]);
    expect(result[3]).toEqual([33, 0]);
  });

  test("Line shape", async () => {
    const result = ptsToNums(polygonLikePoints(shapes[4]));
    expect(result.length).toEqual(2);
    expect(result[0]).toEqual([-11, 0]);
    expect(result[1]).toEqual([33, 44]);
  });

  test("Polygon shape", async () => {
    const result = ptsToNums(polygonLikePoints(shapes[5]));
    expect(result.length).toEqual(3);
    expect(result[0]).toEqual([-11, 0]);
    expect(result[1]).toEqual([33, 0]);
    expect(result[2]).toEqual([33, 44]);
  });

  it.each([shapes[1], shapes[2], shapes[3]])(
    "unsupported shape %p",
    (shape: Shape<ad.Num>) => {
      expect(() => polygonLikePoints(shape)).toThrowError();
    },
  );
});

describe("outwardUnitNormal", () => {
  let point1 = [2, 3];
  let point2 = [1, 2];
  let point3 = [1, 4];
  let point4 = [2, 2];
  let lineSegment = [point3, point4];

  test("inside point above", async () => {
    let result = outwardUnitNormal(lineSegment, point1);

    const [norm, dot, diff] = numsOf([
      ops.vnorm(result),
      ops.vdot(result, ops.vsub(lineSegment[1], lineSegment[0])),
      sub(ops.vdot(result, point1), ops.vdot(result, lineSegment[0])),
    ]);

    // It is unit
    expect(norm).toBeCloseTo(1, 4);
    // It is orthogonal to the line segment
    expect(dot).toBeCloseTo(0, 4);
    // `insidePoint1` is inside
    expect(diff).toBeLessThan(0);
  });

  test("inside point below", async () => {
    let result = outwardUnitNormal(lineSegment, point2);

    const [norm, dot, diff] = numsOf([
      ops.vnorm(result),
      ops.vdot(result, ops.vsub(lineSegment[1], lineSegment[0])),
      sub(ops.vdot(result, point2), ops.vdot(result, lineSegment[0])),
    ]);

    // It is unit
    expect(norm).toBeCloseTo(1, 4);
    // It is orthogonal to the line segment
    expect(dot).toBeCloseTo(0, 4);
    // `insidePoint2` is inside
    expect(diff).toBeLessThan(0);
  });
});

describe("convexPolygonOriginSignedDistance", () => {
  test("inside point", () => {
    const d = numOf(
      convexPolygonOriginSignedDistance([
        [-1, -1],
        [1, -1],
        [0, 1],
      ]),
    );
    const [x, y] = [2 / 5, 1 / 5]; // closest
    expect(d).toBeCloseTo(-Math.sqrt(x ** 2 + y ** 2));
  });

  test("outside point near edge", () => {
    const d = numOf(
      convexPolygonOriginSignedDistance([
        [-1, 1],
        [1, 1],
        [0, 3],
      ]),
    );
    expect(d).toBeCloseTo(1);
  });

  test("outside point near vertex", () => {
    const d = numOf(
      convexPolygonOriginSignedDistance([
        [-1, -3],
        [1, -3],
        [0, -1],
      ]),
    );
    expect(d).toBeCloseTo(1);
  });

  test("outside point near edge with obtuse interior angles", () => {
    const d = numOf(
      convexPolygonOriginSignedDistance([
        [-2, -3],
        [2, -3],
        [1, -2],
        [-1, -2],
      ]),
    );
    expect(d).toBeCloseTo(2);
  });
});

test("shapeDistanceAABBs should return the same value as shapeDistancePolygonlikes", () => {
  for (const i in _rectangles) {
    const r1 = _rectangles[i];

    for (const j in _rectangles) {
      const r2 = _rectangles[j];

      const result1 = shapeDistanceRects(
        bboxPts(bboxFromShape(r1)),
        bboxPts(bboxFromShape(r2)),
      );
      const result2 = shapeDistancePolys(
        polygonLikePoints(r1),
        polygonLikePoints(r2),
      );

      const [result1num, result2num] = numsOf([result1, result2]);

      expect(result1num).toBeCloseTo(result2num, 4);
    }
  }
});

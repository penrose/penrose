import { describe, expect, it, test } from "vitest";
import { Polygon } from "../../shapes/Polygon.js";
import { Polyline } from "../../shapes/Polyline.js";
import * as ad from "../../types/ad.js";
import { objDict, objDictSpecific } from "../Objectives.js";
import { numOf } from "../Utils.js";
import { _polygons, _polylines } from "../__testfixtures__/TestShapes.input.js";

const digitPrecision = 4;

describe("key-name equality", () => {
  test("each function's key and name should be equal", () => {
    for (const [name, func] of Object.entries(objDict)) {
      expect(name).toEqual(func.name);
    }
  });
});

describe("simple objective", () => {
  it.each([
    [1, 1, 0],
    [2, 1, 1],
    [3, 5, 4],
    [4, 5, 1],
  ])(
    "equal(%p, %p) should return %p",
    (x: number, y: number, expected: number) => {
      const result = objDict.equal.body(x, y);
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );

  it.each([
    [1, [1, 2], [1, 2], 1e5],
    [1, [1, 0], [1, 1], 1],
    [2, [1, 0], [1, -1], 2],
    [1, [1, 2], [-1, 2], 0.25],
    [4, [-1, 2], [1, 2], 1],
  ])(
    "repelPt(%p, %p, %p) should return %p",
    (weight: number, a: number[], b: number[], expected: number) => {
      const result = objDict.repelPt.body(weight, a, b);
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );

  it.each([
    [1, 1, 1e5],
    [0, 1, 1],
    [0, -1, 1],
    [1, -1, 0.25],
    [-2, 0, 0.25],
  ])(
    "repelScalar(%p, %p) should return %p",
    (c: number, d: number, expected: number) => {
      const result = objDict.repelScalar.body(c, d);
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );
});

describe("isRegular", () => {
  it.each([[_polylines[6]], [_polygons[6]]])(
    "convex %p",
    (shape: Polyline<ad.Num> | Polygon<ad.Num>) => {
      const result = objDictSpecific.isRegular.body(shape);
      expect(numOf(result)).toBeLessThanOrEqual(1e-5);
    }
  );

  it.each([
    [_polylines[7]],
    [_polygons[7]],
    [_polylines[8]],
    [_polygons[8]],
    [_polylines[9]],
    [_polygons[9]],
  ])("non-convex %p", (shape: Polyline<ad.Num> | Polygon<ad.Num>) => {
    const result = objDictSpecific.isRegular.body(shape);
    expect(numOf(result)).toBeGreaterThan(0.01);
  });
});

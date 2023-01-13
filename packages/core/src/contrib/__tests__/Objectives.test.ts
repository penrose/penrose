import { objDict, objDictSpecific } from "contrib/Objectives";
import { numOf } from "contrib/Utils";
import {
  _polygons,
  _polylines,
} from "contrib/__testfixtures__/TestShapes.input";

const digitPrecision = 4;

describe("simple objective", () => {
  it.each([
    [1, 1, 0],
    [2, 1, 1],
    [3, 5, 4],
    [4, 5, 1],
  ])(
    "equal(%p, %p) should return %p",
    (x: number, y: number, expected: number) => {
      const result = objDict.equal(x, y);
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
      const result = objDict.repelPt(weight, a, b);
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
      const result = objDict.repelScalar(c, d);
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );
});

describe("isRegular", () => {
  it.each([
    ["Polyline", _polylines[6]],
    ["Polygon", _polygons[6]],
  ])("convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = objDictSpecific.isRegular(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polyline", _polylines[7]],
    ["Polygon", _polygons[7]],
    ["Polyline", _polylines[8]],
    ["Polygon", _polygons[8]],
    ["Polyline", _polylines[9]],
    ["Polygon", _polygons[9]],
  ])("non-convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = objDictSpecific.isRegular(shape);
    expect(numOf(result)).toBeGreaterThan(0.01);
  });
});

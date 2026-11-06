import { describe, expect, it, test } from "vitest";
import { makeCircle } from "../../shapes/Circle.js";
import { Polygon } from "../../shapes/Polygon.js";
import { Polyline } from "../../shapes/Polyline.js";
import { makeCanvas, simpleContext } from "../../shapes/Samplers.js";
import * as ad from "../../types/ad.js";
import { vectorV } from "../../utils/Util.js";
import { objDict, objDictSpecific } from "../Objectives.js";
import { extractPoints, isClosed, numOf } from "../Utils.js";
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
      const result = objDict.equal.body(x, y).value;
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    },
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
      const result = objDict.repelPt.body(weight, a, b).value;
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    },
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
      const result = objDict.repelScalar.body(c, d).value;
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    },
  );
});

describe("isRegular", () => {
  it.each([[_polylines[6]], [_polygons[6]]])(
    "convex %p",
    (shape: Polyline<ad.Num> | Polygon<ad.Num>) => {
      const points: ad.Num[][] = extractPoints(shape);
      const closed: boolean = isClosed(shape);
      const result = objDictSpecific.isRegular.body(points, closed).value;
      expect(numOf(result)).toBeLessThanOrEqual(1e-5);
    },
  );

  it.each([
    [_polylines[7]],
    [_polygons[7]],
    [_polylines[8]],
    [_polygons[8]],
    [_polylines[9]],
    [_polygons[9]],
  ])("non-convex %p", (shape: Polyline<ad.Num> | Polygon<ad.Num>) => {
    const points: ad.Num[][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = objDictSpecific.isRegular.body(points, closed).value;
    expect(numOf(result)).toBeGreaterThan(0.01);
  });
});

describe("nonDegenerateAngle", () => {
  const context = simpleContext("nonDegenerateAngle");
  const canvas = makeCanvas(800, 700);
  const anglePrecision = 2;

  const makeTestCircle = (center: [number, number]) =>
    makeCircle(context, canvas, {
      center: vectorV(center),
    });

  it("returns 0 when angle is outside default range cutoff (e.g. 90 deg)", () => {
    const s0 = makeTestCircle([1, 0]);
    const s1 = makeTestCircle([0, 0]);
    const s2 = makeTestCircle([0, 1]);
    const result = objDict.nonDegenerateAngle.body(s0, s1, s2).value;
    expect(numOf(result)).toBeCloseTo(0, digitPrecision);
  });

  it("returns 0 when angle is outside default range cutoff (e.g. 45 deg, preventing radian/degree mismatch)", () => {
    const s0 = makeTestCircle([1, 0]);
    const s1 = makeTestCircle([0, 0]);
    const s2 = makeTestCircle([1, 1]);
    const result = objDict.nonDegenerateAngle.body(s0, s1, s2).value;
    expect(numOf(result)).toBeCloseTo(0, digitPrecision);
  });

  it("returns penalty when angle is collinear (0 deg)", () => {
    const s0 = makeTestCircle([1, 0]);
    const s1 = makeTestCircle([0, 0]);
    const s2 = makeTestCircle([2, 0]);
    const result = objDict.nonDegenerateAngle.body(s0, s1, s2).value;
    expect(numOf(result)).toBeCloseTo(20, anglePrecision);
  });

  it("returns penalty when angle is collinear (180 deg)", () => {
    const s0 = makeTestCircle([1, 0]);
    const s1 = makeTestCircle([0, 0]);
    const s2 = makeTestCircle([-1, 0]);
    const result = objDict.nonDegenerateAngle.body(s0, s1, s2).value;
    expect(numOf(result)).toBeCloseTo(20, anglePrecision);
  });

  it("returns penalty when angle is within default range cutoff (5 deg)", () => {
    const s0 = makeTestCircle([1, 0]);
    const s1 = makeTestCircle([0, 0]);
    const rad = (5 * Math.PI) / 180;
    const s2 = makeTestCircle([Math.cos(rad), Math.sin(rad)]);
    const result = objDict.nonDegenerateAngle.body(s0, s1, s2).value;
    expect(numOf(result)).toBeCloseTo(20 * Math.cos(rad), anglePrecision);
  });

  it("returns penalty when angle is within default range cutoff (175 deg)", () => {
    const s0 = makeTestCircle([1, 0]);
    const s1 = makeTestCircle([0, 0]);
    const rad = (175 * Math.PI) / 180;
    const s2 = makeTestCircle([Math.cos(rad), Math.sin(rad)]);
    const result = objDict.nonDegenerateAngle.body(s0, s1, s2).value;
    expect(numOf(result)).toBeCloseTo(
      20 * Math.cos((5 * Math.PI) / 180),
      anglePrecision,
    );
  });

  it("respects custom strength and range parameters", () => {
    const s0 = makeTestCircle([1, 0]);
    const s1 = makeTestCircle([0, 0]);
    const customStrength = 10;
    const customRange = 30;

    // 45 degrees is outside 30 deg cutoff -> 0
    const s2Outside = makeTestCircle([1, 1]);
    const resOutside = objDict.nonDegenerateAngle.body(
      s0,
      s1,
      s2Outside,
      customStrength,
      customRange,
    ).value;
    expect(numOf(resOutside)).toBeCloseTo(0, digitPrecision);

    // 15 degrees is within 30 deg cutoff -> customStrength * cos(15 deg)
    const rad15 = (15 * Math.PI) / 180;
    const s2Inside = makeTestCircle([Math.cos(rad15), Math.sin(rad15)]);
    const resInside = objDict.nonDegenerateAngle.body(
      s0,
      s1,
      s2Inside,
      customStrength,
      customRange,
    ).value;
    expect(numOf(resInside)).toBeCloseTo(
      customStrength * Math.cos(rad15),
      anglePrecision,
    );
  });
});

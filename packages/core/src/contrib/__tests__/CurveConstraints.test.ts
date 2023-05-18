import { describe, expect, it, test } from "vitest";
import { Polygon } from "../../shapes/Polygon.js";
import { Polyline } from "../../shapes/Polyline.js";
import { Shape } from "../../shapes/Shapes.js";
import * as ad from "../../types/ad.js";
import {
  centerOfMass,
  constrDictCurves,
  curvature,
  elasticEnergy,
  equivalued,
  inflectionEnergy,
  isoperimetricRatio,
  lengthK,
  maxCurvature,
  pElasticEnergy,
  perimeter,
  signedArea,
  totalCurvature,
  turningNumber,
} from "../CurveConstraints.js";
import { extractPoints, isClosed, numOf } from "../Utils.js";
import { _polygons, _polylines } from "../__testfixtures__/TestShapes.input.js";

describe("key-name equality", () => {
  test("each function's key and name should be equal", () => {
    for (const [name, func] of Object.entries(constrDictCurves)) {
      expect(name).toEqual(func.name);
    }
  });
});

describe("equivalued", () => {
  it.each([[[2]], [[3, 3, 3, 3, 3]], [[-5, -5, -5]], [[-5, -5, -5]]])(
    "same numbers",
    (values: number[]) => {
      const result = equivalued(values);
      expect(numOf(result)).toBeCloseTo(0, 4);
    }
  );

  it.each([
    [[3, 1], 2],
    [[-1, 0, 1], 2],
    [[-5, -1], 8],
  ])("different numbers", (values: number[], expected: number) => {
    const result = equivalued(values);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

describe("curvature", () => {
  it.each([
    [
      [1, 2],
      [2, 2],
      [3, 2],
    ],
    [
      [1, 1],
      [2, 2],
      [3, 3],
    ],
    [
      [-1, 1],
      [0, 0],
      [7, -7],
    ],
  ])("line segment", (p1: number[], p2: number[], p3: number[]) => {
    const result = curvature([p1[0], p1[1]], [p2[0], p2[1]], [p3[0], p3[1]]);
    expect(numOf(result)).toBeCloseTo(0, 4);
  });

  it.each([
    [[1, 0], [0, 0], [0, 1], -Math.PI / 2],
    [[0, 1], [0, 0], [1, 0], Math.PI / 2],
    [[2, 1], [0, 0], [-1, 2], -Math.PI / 2],
    [[3, 4], [3, 3], [4, 3], Math.PI / 2],
    [[7, 0], [0, 0], [-2, 2], -Math.PI / 4],
  ])(
    "curved segment",
    (p1: number[], p2: number[], p3: number[], expected: number) => {
      const result = curvature([p1[0], p1[1]], [p2[0], p2[1]], [p3[0], p3[1]]);
      expect(numOf(result)).toBeCloseTo(expected, 4);
    }
  );
});

describe("totalCurvature", () => {
  it.each([
    [_polygons[0], 2 * Math.PI],
    [_polygons[7], -2 * Math.PI],
    [_polygons[8], 4 * Math.PI],
    [_polylines[10], 0],
    [_polygons[10], 0],
  ])("of %p", (shape: Shape<ad.Num>, expected: number) => {
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = totalCurvature(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

describe("elasticEnergy", () => {
  it.each([
    [_polylines[0], 133246],
    [_polygons[1], 58284],
    [_polylines[3], 33983],
    [_polygons[4], 124721],
  ])("of %p", (shape: Shape<ad.Num>, expected: number) => {
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = elasticEnergy(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 0);
  });
});

describe("isoperimetricRatio", () => {
  it.each([
    [_polygons[6], 16],
    [_polygons[7], -18],
    [_polygons[8], (14 * 14) / 6],
  ])("of %p", (shape: Shape<ad.Num>, expected: number) => {
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = isoperimetricRatio(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

describe("perimeter", () => {
  it.each([
    [_polygons[6], 400],
    [_polygons[7], 600],
    [_polygons[8], 1400],
    [_polylines[10], 300],
  ])("of %p", (shape: Shape<ad.Num>, expected: number) => {
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = perimeter(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

describe("turningNumber", () => {
  it.each([
    [_polygons[0], 1],
    [_polygons[7], -1],
    [_polygons[8], 2],
  ])("of %p", (shape: Shape<ad.Num>, expected: number) => {
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = turningNumber(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

describe("signedArea", () => {
  it.each([
    [_polygons[6], 100 * 100],
    [_polygons[7], -200 * 100],
    [_polygons[8], 300 * 200],
  ])("of %p", (shape: Shape<ad.Num>, expected: number) => {
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = signedArea(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

describe("isLocallyConvex", () => {
  it.each([
    [_polylines[0]],
    [_polygons[0]],
    [_polylines[2]],
    [_polygons[2]],
    [_polylines[5]],
    [_polylines[8]],
    [_polygons[8]],
  ])("locally convex %p", (shape: Shape<ad.Num>) => {
    const result = constrDictCurves.isLocallyConvex.body(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([[_polygons[5]], [_polylines[10]], [_polygons[10]]])(
    "not locally convex %p",
    (shape: Shape<ad.Num>) => {
      const result = constrDictCurves.isLocallyConvex.body(shape);
      expect(numOf(result)).toBeGreaterThan(0.01);
    }
  );
});

describe("isConvex", () => {
  it.each([[_polygons[0]], [_polygons[2]], [_polygons[6]], [_polygons[7]]])(
    "convex %p",
    (shape: Polygon<ad.Num>) => {
      const result = constrDictCurves.isConvex.body(shape);
      expect(numOf(result)).toBeLessThanOrEqual(1e-5);
    }
  );

  it.each([[_polygons[5]], [_polygons[8]]])(
    "non-convex %p",
    (shape: Polygon<ad.Num>) => {
      const result = constrDictCurves.isConvex.body(shape);
      expect(numOf(result)).toBeGreaterThan(0.01);
    }
  );
});

describe("isEquidistant", () => {
  it.each([
    [_polylines[6]],
    [_polygons[6]],
    [_polylines[9]],
    [_polygons[9]],
    [_polylines[10]],
  ])("equidistant %p", (shape: Polyline<ad.Num> | Polygon<ad.Num>) => {
    const result = constrDictCurves.isEquilateral.body(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    [_polylines[7]],
    [_polygons[7]],
    [_polylines[8]],
    [_polygons[8]],
    [_polygons[10]],
  ])("non-equidistant %p", (shape: Polyline<ad.Num> | Polygon<ad.Num>) => {
    const result = constrDictCurves.isEquilateral.body(shape);
    expect(numOf(result)).toBeGreaterThan(0.01);
  });
});

describe("isEquiangular", () => {
  it.each([
    [_polylines[6]],
    [_polygons[6]],
    [_polylines[7]],
    [_polygons[7]],
    [_polylines[8]],
    [_polygons[8]],
  ])("equiangular %p", (shape: Polygon<ad.Num> | Polyline<ad.Num>) => {
    const result = constrDictCurves.isEquiangular.body(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    [_polylines[0]],
    [_polygons[0]],
    [_polylines[5]],
    [_polygons[5]],
    [_polylines[9]],
    [_polygons[9]],
  ])("non-equiangular %p", (shape: Polygon<ad.Num> | Polyline<ad.Num>) => {
    const result = constrDictCurves.isEquiangular.body(shape);
    expect(numOf(result)).toBeGreaterThan(0.01);
  });
});

describe("pElasticEnergy", () => {
  it.each([
    [_polylines[0], 2, 133246],
    [_polygons[1], 2, 58284],
    [_polylines[3], 2, 33983],
    [_polygons[4], 2, 124721],
    [_polylines[0], 3, 205775],
    [_polygons[1], 3, 103360],
    [_polylines[3], 3, 38283],
    [_polygons[4], 3, 210899],
  ])("of %p", (shape: Shape<ad.Num>, p: number, expected: number) => {
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = pElasticEnergy(points, closed, p);
    expect(numOf(result)).toBeCloseTo(expected, 0);
  });
});

describe("maxCurvature", () => {
  it.each([
    [_polygons[6], 0.01],
    [_polygons[7], 0.006],
    [_polygons[8], 0.01],
    [_polylines[10], 0.01],
  ])("of %p", (shape: Shape<ad.Num>, expected: number) => {
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = maxCurvature(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 2);
  });
});

describe("lengthK", () => {
  it.each([
    [_polygons[6], 1, 400],
    [_polygons[7], 1, 600],
    [_polygons[8], 1, 1400],
    [_polylines[10], 1, 300],
    [_polygons[6], 2, 40000],
    [_polygons[7], 2, 100000],
    [_polygons[8], 2, 280000],
    [_polylines[10], 2, 30000],
  ])("of %p", (shape: Shape<ad.Num>, k: number, expected: number) => {
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = lengthK(points, closed, k);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

describe("inflectionEnergy", () => {
  it.each([
    [_polygons[6], 1, 0.0],
    [_polygons[7], 1, 0.0],
    [_polygons[8], 1, 0.0],
    [_polylines[10], 1, 2.83],
    [_polygons[6], 2, 0.0],
    [_polygons[7], 2, 0.0],
    [_polygons[8], 2, 0.0],
    [_polylines[10], 2, 8.0],
  ])("of %p", (shape: Shape<ad.Num>, p: number, expected: number) => {
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = inflectionEnergy(points, closed, p);
    expect(numOf(result)).toBeCloseTo(expected, 2);
  });
});

describe("centerOfMass", () => {
  it.each([
    [_polygons[6], [250, 50]],
    [_polygons[7], [250, 100]],
    [_polygons[8], [150, 125]],
    [_polylines[10], [50, 100]],
  ])("of %p", (shape: Shape<ad.Num>, expected: number[]) => {
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const result = centerOfMass(points);
    expect(numOf(result[0])).toBeCloseTo(expected[0], 2);
    expect(numOf(result[1])).toBeCloseTo(expected[1], 2);
  });
});

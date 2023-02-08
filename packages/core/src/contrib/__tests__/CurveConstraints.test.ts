import * as ad from "../../types/ad";
import {
  constrDictCurves,
  curvature,
  elasticEnergy,
  equivalued,
  isoperimetricRatio,
  perimeter,
  signedArea,
  totalCurvature,
  turningNumber,
} from "../CurveConstraints";
import { extractPoints, isClosed, numOf } from "../Utils";
import { _polygons, _polylines } from "../__testfixtures__/TestShapes.input";

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
    ["Polygon", _polygons[0], 2 * Math.PI],
    ["Polygon", _polygons[7], -2 * Math.PI],
    ["Polygon", _polygons[8], 4 * Math.PI],
    ["Polyline", _polylines[10], 0],
    ["Polygon", _polygons[10], 0],
  ])("of %p", (shapeType: string, shapeData: any, expected: number) => {
    const shape: [string, any] = [shapeType, shapeData];
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = totalCurvature(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

describe("elasticEnergy", () => {
  it.each([
    ["Polyline", _polylines[0], 174949],
    ["Polygon", _polygons[1], 90849],
    ["Polyline", _polylines[3], 38372],
    ["Polygon", _polygons[4], 195758],
  ])("of %p", (shapeType: string, shapeData: any, expected: number) => {
    const shape: [string, any] = [shapeType, shapeData];
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = elasticEnergy(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 0);
  });
});

describe("isoperimetricRatio", () => {
  it.each([
    ["Polygon", _polygons[6], 16],
    ["Polygon", _polygons[7], -18],
    ["Polygon", _polygons[8], (14 * 14) / 6],
  ])("of %p", (shapeType: string, shapeData: any, expected: number) => {
    const shape: [string, any] = [shapeType, shapeData];
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = isoperimetricRatio(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

describe("perimeter", () => {
  it.each([
    ["Polygon", _polygons[6], 400],
    ["Polygon", _polygons[7], 600],
    ["Polygon", _polygons[8], 1400],
    ["Polyline", _polylines[10], 300],
  ])("of %p", (shapeType: string, shapeData: any, expected: number) => {
    const shape: [string, any] = [shapeType, shapeData];
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = perimeter(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

describe("turningNumber", () => {
  it.each([
    ["Polygon", _polygons[0], 1],
    ["Polygon", _polygons[7], -1],
    ["Polygon", _polygons[8], 2],
  ])("of %p", (shapeType: string, shapeData: any, expected: number) => {
    const shape: [string, any] = [shapeType, shapeData];
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = turningNumber(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

describe("signedArea", () => {
  it.each([
    ["Polygon", _polygons[6], 100 * 100],
    ["Polygon", _polygons[7], -200 * 100],
    ["Polygon", _polygons[8], 300 * 200],
  ])("of %p", (shapeType: string, shapeData: any, expected: number) => {
    const shape: [string, any] = [shapeType, shapeData];
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = signedArea(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

describe("isLocallyConvex", () => {
  it.each([
    ["Polyline", _polylines[0]],
    ["Polygon", _polygons[0]],
    ["Polyline", _polylines[2]],
    ["Polygon", _polygons[2]],
    ["Polyline", _polylines[5]],
    ["Polyline", _polylines[8]],
    ["Polygon", _polygons[8]],
  ])("locally convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isLocallyConvex(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polygon", _polygons[5]],
    ["Polyline", _polylines[10]],
    ["Polygon", _polygons[10]],
  ])("not locally convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isLocallyConvex(shape);
    expect(numOf(result)).toBeGreaterThan(0.01);
  });
});

describe("isConvex", () => {
  it.each([
    ["Polygon", _polygons[0]],
    ["Polygon", _polygons[2]],
    ["Polygon", _polygons[6]],
    ["Polygon", _polygons[7]],
  ])("convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isConvex(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polygon", _polygons[5]],
    ["Polygon", _polygons[8]],
  ])("non-convex %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isConvex(shape);
    expect(numOf(result)).toBeGreaterThan(0.01);
  });
});

describe("isEquidistant", () => {
  it.each([
    ["Polyline", _polylines[6]],
    ["Polygon", _polygons[6]],
    ["Polyline", _polylines[9]],
    ["Polygon", _polygons[9]],
    ["Polyline", _polylines[10]],
  ])("equidistant %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquilateral(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polyline", _polylines[7]],
    ["Polygon", _polygons[7]],
    ["Polyline", _polylines[8]],
    ["Polygon", _polygons[8]],
    ["Polygon", _polygons[10]],
  ])("non-equidistant %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquilateral(shape);
    expect(numOf(result)).toBeGreaterThan(0.01);
  });
});

describe("isEquiangular", () => {
  it.each([
    ["Polyline", _polylines[6]],
    ["Polygon", _polygons[6]],
    ["Polyline", _polylines[7]],
    ["Polygon", _polygons[7]],
    ["Polyline", _polylines[8]],
    ["Polygon", _polygons[8]],
  ])("equiangular %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquiangular(shape);
    expect(numOf(result)).toBeLessThanOrEqual(1e-5);
  });

  it.each([
    ["Polyline", _polylines[0]],
    ["Polygon", _polygons[0]],
    ["Polyline", _polylines[5]],
    ["Polygon", _polygons[5]],
    ["Polyline", _polylines[9]],
    ["Polygon", _polygons[9]],
  ])("non-equiangular %p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = constrDictCurves.isEquiangular(shape);
    expect(numOf(result)).toBeGreaterThan(0.01);
  });
});

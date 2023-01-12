import {
  algebraicArea,
  curvature,
  elasticEnergy,
  isoperimetricRatio,
  perimeter,
  totalCurvature,
  turningNumber,
} from "contrib/Functions";
import { extractPoints, isClosed, numOf } from "contrib/Utils";
import {
  _polygons,
  _polylines,
} from "contrib/__testfixtures__/TestShapes.input";
import * as ad from "types/ad";

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
    ["Polyline", _polylines[10], Math.PI / 2],
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
    ["Polyline", _polylines[0], 666],
    ["Polygon", _polygons[1], 666],
    ["Polyline", _polylines[3], 666],
    ["Polygon", _polygons[4], 666],
  ])("of %p", (shapeType: string, shapeData: any, expected: number) => {
    const shape: [string, any] = [shapeType, shapeData];
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = elasticEnergy(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 4);
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

describe("algebraicArea", () => {
  it.each([
    ["Polygon", _polygons[6], 100 * 100],
    ["Polygon", _polygons[7], -200 * 100],
    ["Polygon", _polygons[8], 300 * 200],
  ])("of %p", (shapeType: string, shapeData: any, expected: number) => {
    const shape: [string, any] = [shapeType, shapeData];
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = algebraicArea(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

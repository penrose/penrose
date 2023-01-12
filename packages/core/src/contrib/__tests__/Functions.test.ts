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
  _closed_paths,
  _open_paths,
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
    [[2, 1], [0, 0], [-2, 1], -Math.PI / 2],
    [[3, 4], [3, 3], [4, 3], Math.PI / 2],
    [[7, 0], [0, 0], [-2, 1], Math.PI / 6],
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
    ["Path", _closed_paths[1], 2 * Math.PI],
    ["Polygon", _polygons[7], -2 * Math.PI],
    ["Path", _closed_paths[7], -2 * Math.PI],
    ["Polygon", _polygons[8], 4 * Math.PI],
    ["Path", _closed_paths[8], 4 * Math.PI],
    ["Polyline", _polylines[10], Math.PI / 2],
    ["Path", _open_paths[7], (-3 * Math.PI) / 2],
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
    ["Polyline", _polylines[666], 666],
    ["Polygon", _polygons[666], 666],
    ["Path", _closed_paths[666], 666],
    ["Path", _open_paths[666], 666],
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
    ["Path", _closed_paths[6], 16],
    ["Polygon", _polygons[7], 18],
    ["Path", _closed_paths[7], 18],
    ["Polygon", _polygons[8], 256 / 7],
    ["Path", _closed_paths[8], 256 / 7],
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
    ["Path", _closed_paths[6], 400],
    ["Polygon", _polygons[7], 600],
    ["Path", _closed_paths[7], 600],
    ["Polygon", _polygons[8], 4 * 300 + 400],
    ["Path", _closed_paths[8], 4 * 300 + 400],
    ["Polyline", _polylines[10], 300],
    ["Path", _open_paths[10], 300],
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
    ["Path", _closed_paths[1], 1],
    ["Polygon", _polygons[7], -1],
    ["Path", _closed_paths[7], -1],
    ["Polygon", _polygons[8], 2],
    ["Path", _closed_paths[8], 2],
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
    ["Path", _closed_paths[6], 100 * 100],
    ["Polygon", _polygons[7], 200 * 100],
    ["Path", _closed_paths[7], 200 * 100],
    ["Polygon", _polygons[8], 300 * 300 - 2 * 100 * 100],
    ["Path", _closed_paths[8], 300 * 300 - 2 * 100 * 100],
  ])("of %p", (shapeType: string, shapeData: any, expected: number) => {
    const shape: [string, any] = [shapeType, shapeData];
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = algebraicArea(points, closed);
    expect(numOf(result)).toBeCloseTo(expected, 4);
  });
});

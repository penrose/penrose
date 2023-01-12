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
    ["Polyline", _polylines[666], 666],
    ["Polygon", _polygons[666], 666],
    ["Path", _closed_paths[666], 666],
    ["Path", _open_paths[666], 666],
  ])("of %p", (shapeType: string, shapeData: any, expected: number) => {
    const shape: [string, any] = [shapeType, shapeData];
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = totalCurvature(shapeData, closed);
    expect(numOf(result)).toStrictEqual(expected);
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
    const result = elasticEnergy(shapeData, closed);
    expect(numOf(result)).toStrictEqual(expected);
  });
});

describe("isoperimetricRatio", () => {
  it.each([
    ["Polyline", _polylines[666], 666],
    ["Polygon", _polygons[666], 666],
    ["Path", _closed_paths[666], 666],
    ["Path", _open_paths[666], 666],
  ])("of %p", (shapeType: string, shapeData: any, expected: number) => {
    const shape: [string, any] = [shapeType, shapeData];
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = isoperimetricRatio(shapeData, closed);
    expect(numOf(result)).toStrictEqual(expected);
  });
});

describe("perimeter", () => {
  it.each([
    ["Polyline", _polylines[666], 666],
    ["Polygon", _polygons[666], 666],
    ["Path", _closed_paths[666], 666],
    ["Path", _open_paths[666], 666],
  ])("of %p", (shapeType: string, shapeData: any, expected: number) => {
    const shape: [string, any] = [shapeType, shapeData];
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = perimeter(shapeData, closed);
    expect(numOf(result)).toStrictEqual(expected);
  });
});

describe("turningNumber", () => {
  it.each([
    ["Polyline", _polylines[666], 666],
    ["Polygon", _polygons[666], 666],
    ["Path", _closed_paths[666], 666],
    ["Path", _open_paths[666], 666],
  ])("of %p", (shapeType: string, shapeData: any, expected: number) => {
    const shape: [string, any] = [shapeType, shapeData];
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = turningNumber(shapeData, closed);
    expect(numOf(result)).toStrictEqual(expected);
  });
});

describe("algebraicArea", () => {
  it.each([
    ["Polyline", _polylines[666], 666],
    ["Polygon", _polygons[666], 666],
    ["Path", _closed_paths[666], 666],
    ["Path", _open_paths[666], 666],
  ])("of %p", (shapeType: string, shapeData: any, expected: number) => {
    const shape: [string, any] = [shapeType, shapeData];
    const points: [ad.Num, ad.Num][] = extractPoints(shape);
    const closed: boolean = isClosed(shape);
    const result = algebraicArea(shapeData, closed);
    expect(numOf(result)).toStrictEqual(expected);
  });
});

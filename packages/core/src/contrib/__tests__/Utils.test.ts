import {
  consecutiveTriples,
  consecutiveTuples,
  extractPoints,
  isClosed,
} from "../Utils";
import { _polygons, _polylines } from "../__testfixtures__/TestShapes.input";
import * as ad from "../../types/ad";

describe("consecutiveTuples", () => {
  const a: [ad.Num, ad.Num] = [1, 2];
  const b: [ad.Num, ad.Num] = [3, 4];
  const c: [ad.Num, ad.Num] = [5, 6];
  const d: [ad.Num, ad.Num] = [7, 8];
  it.each([
    [[a], [[a, a]]],
    [
      [a, b],
      [
        [a, b],
        [b, a],
      ],
    ],
    [
      [a, b, c],
      [
        [a, b],
        [b, c],
        [c, a],
      ],
    ],
  ])(
    "consecutiveTuples for closed objects",
    (points: [ad.Num, ad.Num][], expected: [ad.Num, ad.Num][][]) => {
      const result = consecutiveTuples(points, true);
      expect(result).toStrictEqual(expected);
    }
  );

  it.each([
    [[a], []],
    [[a, b], [[a, b]]],
    [
      [a, b, c],
      [
        [a, b],
        [b, c],
      ],
    ],
  ])(
    "consecutiveTuples for open objects",
    (points: [ad.Num, ad.Num][], expected: [ad.Num, ad.Num][][]) => {
      const result = consecutiveTuples(points, false);
      expect(result).toStrictEqual(expected);
    }
  );

  it.each([
    [[a], [[a, a, a]]],
    [
      [a, b],
      [
        [a, b, a],
        [b, a, b],
      ],
    ],
    [
      [a, b, c],
      [
        [a, b, c],
        [b, c, a],
        [c, a, b],
      ],
    ],
    [
      [a, b, c, d],
      [
        [a, b, c],
        [b, c, d],
        [c, d, a],
        [d, a, b],
      ],
    ],
  ])(
    "consecutiveTriples for closed objects",
    (points: [ad.Num, ad.Num][], expected: [ad.Num, ad.Num][][]) => {
      const result = consecutiveTriples(points, true);
      expect(result).toStrictEqual(expected);
    }
  );
  it.each([
    [[a], []],
    [[a, b], []],
    [[a, b, c], [[a, b, c]]],
    [
      [a, b, c, d],
      [
        [a, b, c],
        [b, c, d],
      ],
    ],
  ])(
    "consecutiveTriples for open objects",
    (points: [ad.Num, ad.Num][], expected: [ad.Num, ad.Num][][]) => {
      const result = consecutiveTriples(points, false);
      expect(result).toStrictEqual(expected);
    }
  );
});

describe("isClosed", () => {
  it.each([["Polygon", _polygons[1]]])(
    "closed %p",
    (shapeType: string, shapeData: any) => {
      const shape: [string, any] = [shapeType, shapeData];
      const result = isClosed(shape);
      expect(result).toBeTruthy();
    }
  );

  it.each([["Polyline", _polylines[3]]])(
    "open %p",
    (shapeType: string, shapeData: any) => {
      const shape: [string, any] = [shapeType, shapeData];
      const result = isClosed(shape);
      expect(result).toBeFalsy();
    }
  );
});

describe("extractPoints", () => {
  it.each([
    ["Polyline", _polylines[5]],
    ["Polygon", _polygons[6]],
  ])("%p", (shapeType: string, shapeData: any) => {
    const shape: [string, any] = [shapeType, shapeData];
    const result = extractPoints(shape);
    expect(result).toStrictEqual(shapeData.points.contents);
  });
});

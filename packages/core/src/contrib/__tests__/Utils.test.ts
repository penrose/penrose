import { Polygon } from "../../shapes/Polygon";
import { Polyline } from "../../shapes/Polyline";
import { Shape } from "../../shapes/Shapes";
import * as ad from "../../types/ad";
import {
  consecutiveTriples,
  consecutiveTuples,
  extractPoints,
  isClosed,
} from "../Utils";
import { _polygons, _polylines } from "../__testfixtures__/TestShapes.input";

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
  it.each([[_polygons[1]]])("closed %p", (shape: Shape<ad.Num>) => {
    const result = isClosed(shape);
    expect(result).toBeTruthy();
  });

  it.each([[_polylines[3]]])("open %p", (shape: Shape<ad.Num>) => {
    const result = isClosed(shape);
    expect(result).toBeFalsy();
  });
});

describe("extractPoints", () => {
  it.each([[_polylines[5]], [_polygons[6]]])(
    "%p",
    (shape: Polyline<ad.Num> | Polygon<ad.Num>) => {
      const result = extractPoints(shape);
      expect(result).toStrictEqual(shape.points.contents);
    }
  );
});

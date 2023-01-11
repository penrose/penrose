import { consecutiveTriples, consecutiveTuples } from "contrib/Utils";
import * as ad from "types/ad";

describe("consecutiveNTuples", () => {
  const a: [ad.Num, ad.Num] = [1, 2];
  const b: [ad.Num, ad.Num] = [3, 4];
  const c: [ad.Num, ad.Num] = [5, 6];
  const d: [ad.Num, ad.Num] = [7, 8];
  it.each([
    [[a], [[a, a]]],
    [
      [a, b],
      [
        [b, a],
        [a, b],
      ],
    ],
    [
      [a, b, c],
      [
        [c, a],
        [a, b],
        [b, c],
        [c, a],
      ],
    ],
  ])(
    "consecutiveTuples for closed objects",
    (points: [ad.Num, ad.Num][], expected: [ad.Num, ad.Num][][]) => {
      const result = consecutiveTuples(points, true);
      expect(result).toBe(expected);
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
      expect(result).toBe(expected);
    }
  );

  it.each([
    [[a], [[a, a, a]]],
    [
      [a, b],
      [
        [b, a, b],
        [a, b, a],
      ],
    ],
    [
      [a, b, c],
      [
        [b, c, a],
        [c, a, b],
        [a, b, c],
      ],
    ],
    [
      [a, b, c, d],
      [
        [c, d, a],
        [c, a, b],
        [a, b, c],
        [b, c, d],
      ],
    ],
  ])(
    "consecutiveTriples for closed objects",
    (points: [ad.Num, ad.Num][], expected: [ad.Num, ad.Num][][]) => {
      const result = consecutiveTriples(points, true);
      expect(result).toBe(expected);
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
      expect(result).toBe(expected);
    }
  );
});

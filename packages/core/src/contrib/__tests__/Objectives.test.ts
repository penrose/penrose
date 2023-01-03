import { ready } from "@penrose/optimizer";
import { objDict } from "../Objectives";
import { numOf } from "../Utils";

await ready;

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

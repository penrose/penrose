import { Shape } from "../../shapes/Shapes";
import * as ad from "../../types/ad";
import { constrDict } from "../Constraints";
import { numOf } from "../Utils";
import {
  _circles,
  _ellipses,
  _lines,
  _polygons,
  _rectangles,
} from "../__testfixtures__/TestShapes.input";

const digitPrecision = 10;

describe("key-name equality", () => {
  test("each function's key and name should be equal", () => {
    for (const [name, func] of Object.entries(constrDict)) {
      expect(name).toEqual(func.name);
    }
  });
});

describe("simple constraint", () => {
  it.each([
    [1, 1, 0],
    [2, 1, 1],
    [3, 5, 2],
    [4, 5, 1],
  ])(
    "equal(%p, %p) should return %p",
    (x: number, y: number, expected: number) => {
      const result = constrDict.equal.body(x, y);
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );

  it.each([
    [1, 1, 0, 0],
    [2, 1, 0, 1],
    [3, 5, 0, -2],
    [4, 5, 0, -1],
    [2, 1, -1, 0],
    [4, 5, 1, 0],
  ])(
    "lessThan(%p, %p, padding=%p) should return %p",
    (x: number, y: number, padding: number, expected: number) => {
      const result = constrDict.lessThan.body(x, y, padding);
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );

  it.each([
    [1, 1, 0, 0],
    [1, 2, 0, 1],
    [5, 3, 0, -2],
    [5, 4, 0, -1],
    [1, 2, -1, 0],
    [5, 4, 1, 0],
  ])(
    "greaterThan(%p, %p, padding=%p) should return %p",
    (x: number, y: number, padding: number, expected: number) => {
      const result = constrDict.greaterThan.body(x, y, padding);
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );

  it.each([
    [1, 1, 0],
    [2, 1, 1],
    [5, 3, 4],
    [4, 5, 0],
  ])(
    "lessThanSq(%p, %p) should return %p",
    (x: number, y: number, expected: number) => {
      const result = constrDict.lessThanSq.body(x, y);
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );

  it.each([
    [1, 1, 0],
    [1, 2, 1],
    [3, 5, 4],
    [5, 4, 0],
  ])(
    "greaterThanSq(%p, %p) should return %p",
    (x: number, y: number, expected: number) => {
      const result = constrDict.greaterThanSq.body(x, y);
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );

  it.each([
    [0, 1, 3, 1],
    [1, 1, 3, 0],
    [2, 1, 3, 0],
    [3, 1, 3, 0],
    [4, 1, 3, 1],
  ])(
    "inRange(%p, %p, %p) should return %p",
    (x: number, x0: number, x1: number, expected: number) => {
      const result = constrDict.inRange.body(x, x0, x1);
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );

  it.each([
    [1, 4, 1, 4, 0],
    [1, 4, 1, 3, 0],
    [1, 4, 2, 4, 0],
    [1, 4, 2, 5, 1],
    [1, 4, 0, 3, 1],
    [1, 4, 6, 7, 3],
    [1, 4, -2, -1, 3],
  ])(
    "contains1D([%p, %p], [%p, %p]) should return %p",
    (l1: number, r1: number, l2: number, r2: number, expected: number) => {
      const result = constrDict.contains1D.body([l1, r1], [l2, r2]);
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );

  it.each([
    [0, 1, 5, 0],
    [2, 1, 5, 1],
    [3, 1, 5, 2],
    [4, 1, 5, 1],
    [6, 1, 5, 0],
  ])(
    "disjointScalar(%p, %p, %p) should return %p",
    (c: number, left: number, right: number, expected: number) => {
      const result = constrDict.disjointScalar.body(c, left, right);
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );

  it.each([
    [[1, 2], [1, 1], [2, 1], 0],
    [[1, 3], [1, 1], [3, 1], 0],
    [[1, 0], [1, 1], [1, 2], 1],
    [[1, 0], [1, 1], [1, 10], 9],
    [[1, 0], [1, 1], [1, -10], 11],
  ])(
    "perpendicular(%p, %p, %p) should return %p",
    (q: number[], p: number[], r: number[], expected: number) => {
      const result = constrDict.perpendicular.body(q, p, r);
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );

  it.each([
    [[1, 2], [1, 1], [2, 1], 1],
    [[1, 3], [1, 1], [3, 1], 4],
    [[1, 0], [1, 1], [1, 2], 0],
    [[1, 0], [1, 1], [1, 10], 0],
    [[1, 0], [1, 1], [1, -10], 0],
  ])(
    "collinear(%p, %p, %p) should return %p",
    (c1: number[], c2: number[], c3: number[], expected: number) => {
      const result = constrDict.collinear.body(c1, c2, c3);
      expect(numOf(result)).toBeCloseTo(expected, 1);
    }
  );

  it.each([
    [[1, 2], [1, 1], [2, 1], 0.6],
    [[1, 3], [1, 1], [3, 1], 1.2],
    [[1, 0], [1, 1], [1, 2], 0],
    [[1, 0], [1, 1], [1, 10], 0],
    [[1, -10], [1, 0], [1, 1], 0],
  ])(
    "collinearOrdered(%p, %p, %p) should return %p",
    (c1: number[], c2: number[], c3: number[], expected: number) => {
      const result = constrDict.collinearOrdered.body(c1, c2, c3);
      expect(numOf(result)).toBeCloseTo(expected, 1);
    }
  );
});

describe("general constraints", () => {
  const expectSatified = (x: ad.Num) => {
    expect(numOf(x)).toBeLessThanOrEqual(1e-5);
  };
  const expectJustSatified = (x: ad.Num) => {
    expect(numOf(x)).toBeCloseTo(0, 5);
  };
  const expectNotSatisfied = (x: ad.Num) => {
    expect(numOf(x)).toBeGreaterThan(5);
  };

  // Overlapping shapes
  it.each([
    // Without padding
    [0, _rectangles[0], _rectangles[2]],
    [0, _rectangles[0], _circles[2]],
    [0, _circles[0], _rectangles[2]],
    [0, _circles[0], _circles[2]],
    [0, _circles[0], _ellipses[2]],
    [0, _ellipses[0], _circles[2]],
    [0, _ellipses[0], _ellipses[2]],
    [0, _rectangles[0], _ellipses[2]],
    [0, _ellipses[0], _rectangles[2]],
    [0, _lines[2], _lines[3]],
    [0, _polygons[2], _polygons[3]],
    [0, _polygons[0], _polygons[3]],
    [0, _polygons[0], _polygons[4]],
    [0, _lines[3], _polygons[0]],
    [0, _lines[2], _polygons[3]],
    [0, _rectangles[0], _lines[0]],
    [0, _rectangles[2], _polygons[2]],
    [0, _rectangles[0], _polygons[0]],
    [0, _ellipses[6], _lines[0]],
    [0, _ellipses[7], _ellipses[8]],
    [0, _ellipses[8], _ellipses[10]],
    // With padding
    [150, _rectangles[3], _rectangles[1]],
    [150, _rectangles[3], _circles[1]],
    [150, _circles[3], _rectangles[1]],
    [150, _rectangles[3], _ellipses[1]],
    [150, _ellipses[3], _rectangles[1]],
    [150, _circles[3], _circles[1]],
    [150, _circles[3], _ellipses[1]],
    [150, _ellipses[3], _circles[1]],
    [150, _ellipses[3], _ellipses[1]],
    [200, _lines[1], _lines[3]],
    [100, _polygons[1], _polygons[2]],
    [150, _polygons[1], _polygons[3]],
    [150, _lines[1], _polygons[3]],
    [150, _lines[1], _polygons[2]],
    [150, _rectangles[0], _lines[2]],
    [150, _rectangles[3], _polygons[1]],
    [150, _rectangles[1], _polygons[2]],
    [100, _ellipses[6], _ellipses[7]],
    [100, _ellipses[6], _ellipses[8]],
    [100 * (Math.SQRT2 - 1) + 10, _circles[4], _rectangles[2]],
    [100 * (Math.SQRT2 - 1) + 10, _ellipses[4], _rectangles[2]],
  ] as const)(
    "overlapping %p and %p with padding %p",
    (padding: number, shape0: Shape<ad.Num>, shape1: Shape<ad.Num>) => {
      // The condition should be satisfied
      const overlap = -padding;
      expectSatified(constrDict.overlapping.body(shape0, shape1, overlap));
      expectSatified(constrDict.overlapping.body(shape1, shape0, overlap));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.disjoint.body(shape0, shape1, padding));
      expectNotSatisfied(constrDict.disjoint.body(shape1, shape0, padding));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.contains.body(shape0, shape1, padding));
      expectNotSatisfied(constrDict.contains.body(shape1, shape0, padding));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.touching.body(shape0, shape1, padding));
      expectNotSatisfied(constrDict.touching.body(shape1, shape0, padding));
    }
  );

  // Disjoint shapes
  it.each([
    // Without padding
    [0, _rectangles[2], _rectangles[3]],
    [0, _rectangles[2], _circles[3]],
    [0, _circles[2], _rectangles[3]],
    [0, _rectangles[2], _ellipses[3]],
    [0, _ellipses[2], _rectangles[3]],
    [0, _circles[2], _circles[3]],
    [0, _circles[2], _ellipses[3]],
    [0, _ellipses[2], _circles[3]],
    [0, _ellipses[2], _ellipses[3]],
    [0, _lines[0], _lines[3]],
    [0, _lines[1], _lines[2]],
    [0, _lines[0], _polygons[2]],
    [0, _lines[0], _polygons[3]],
    [0, _rectangles[1], _polygons[3]],
    [0, _rectangles[2], _lines[2]],
    [0, _circles[3], _rectangles[2]],
    [0, _ellipses[3], _rectangles[2]],
    [0, _ellipses[7], _lines[2]],
    [0, _ellipses[8], _ellipses[9]],
    [0, _ellipses[7], _ellipses[10]],
    [0, _ellipses[9], _ellipses[10]],
    // With padding
    [10, _rectangles[1], _rectangles[3]],
    [10, _rectangles[1], _circles[3]],
    [10, _circles[1], _rectangles[3]],
    [10, _rectangles[1], _ellipses[3]],
    [10, _ellipses[1], _rectangles[3]],
    [10, _circles[1], _circles[3]],
    [110, _circles[2], _circles[3]],
    [10, _circles[1], _ellipses[3]],
    [110, _circles[2], _ellipses[3]],
    [10, _ellipses[1], _circles[3]],
    [110, _ellipses[2], _circles[3]],
    [10, _ellipses[1], _ellipses[3]],
    [110, _ellipses[2], _ellipses[3]],
    [50, _lines[0], _lines[3]],
    [50, _lines[1], _lines[2]],
    [50, _lines[0], _polygons[2]],
    [50, _lines[0], _polygons[3]],
    [50, _rectangles[1], _polygons[3]],
    [50, _rectangles[2], _lines[2]],
  ] as const)(
    "disjoint %p and %p with padding %p",
    (padding: number, shape0: Shape<ad.Num>, shape1: Shape<ad.Num>) => {
      // The condition should NOT be satisfied
      const overlap = -padding;
      expectNotSatisfied(constrDict.overlapping.body(shape0, shape1, overlap));
      expectNotSatisfied(constrDict.overlapping.body(shape1, shape0, overlap));
      // The condition should be satisfied
      expectSatified(constrDict.disjoint.body(shape0, shape1, padding));
      expectSatified(constrDict.disjoint.body(shape1, shape0, padding));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.contains.body(shape0, shape1, padding));
      expectNotSatisfied(constrDict.contains.body(shape1, shape0, padding));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.touching.body(shape0, shape1, padding));
      expectNotSatisfied(constrDict.touching.body(shape1, shape0, padding));
    }
  );

  // Touching shapes
  it.each([
    // Without padding
    [0, _rectangles[1], _rectangles[2]],
    [0, _rectangles[1], _circles[2]],
    [0, _circles[1], _rectangles[2]],
    [0, _rectangles[1], _ellipses[2]],
    [0, _ellipses[1], _rectangles[2]],
    [0, _circles[1], _circles[2]],
    [0, _circles[1], _ellipses[2]],
    [0, _ellipses[1], _circles[2]],
    [0, _ellipses[1], _ellipses[2]],
    [0, _lines[0], _lines[1]],
    [0, _lines[0], _lines[2]],
    [0, _lines[2], _polygons[2]],
    [0, _rectangles[1], _polygons[1]],
    [0, _rectangles[1], _lines[1]],
    // With padding
    [100, _rectangles[1], _rectangles[3]],
    [100, _rectangles[1], _circles[3]],
    [100, _circles[1], _rectangles[3]],
    [100, _rectangles[1], _ellipses[3]],
    [100, _ellipses[1], _rectangles[3]],
    [100, _circles[1], _circles[3]],
    [100, _circles[1], _ellipses[3]],
    [100, _ellipses[1], _circles[3]],
    [100, _ellipses[1], _ellipses[3]],
    [100, _lines[1], _lines[2]],
    [100 * (Math.SQRT2 - 1), _circles[4], _rectangles[2]],
  ] as const)(
    "touching %p and %p with padding %p",
    (padding: number, shape0: Shape<ad.Num>, shape1: Shape<ad.Num>) => {
      // The condition should JUST be satisfied
      const overlap = -padding;
      expectJustSatified(constrDict.overlapping.body(shape0, shape1, overlap));
      expectJustSatified(constrDict.overlapping.body(shape1, shape0, overlap));
      // The condition should JUST be satisfied
      expectJustSatified(constrDict.disjoint.body(shape0, shape1, padding));
      expectJustSatified(constrDict.disjoint.body(shape1, shape0, padding));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.contains.body(shape0, shape1, padding));
      expectNotSatisfied(constrDict.contains.body(shape1, shape0, padding));
      // The condition should be satisfied
      expectSatified(constrDict.touching.body(shape0, shape1, padding));
      expectSatified(constrDict.touching.body(shape1, shape0, padding));
    }
  );

  // The first shapes is contained in the second one
  it.each([
    // Without padding
    [0, _rectangles[0], _rectangles[1]],
    [0, _rectangles[0], _circles[1]],
    [0, _circles[0], _rectangles[1]],
    [0, _rectangles[0], _ellipses[1]],
    [0, _ellipses[0], _rectangles[1]],
    [0, _circles[0], _circles[1]],
    [0, _circles[0], _ellipses[1]],
    [0, _ellipses[0], _circles[1]],
    [0, _ellipses[0], _ellipses[1]],
    [0, _polygons[0], _polygons[1]],
    [0, _polygons[0], _lines[1]],
    [0, _rectangles[0], _polygons[1]],
    [0, _rectangles[0], _lines[1]],
    [0, _polygons[0], _circles[5]],
    [0, _circles[6], _polygons[1]],
    [0, _polygons[0], _ellipses[5]],
    [0, _ellipses[6], _polygons[1]],
    // With padding
    [50, _rectangles[0], _rectangles[1]],
    [50, _rectangles[0], _circles[1]],
    [50, _circles[0], _rectangles[1]],
    [50, _rectangles[0], _ellipses[1]],
    [50, _ellipses[0], _rectangles[1]],
    [50, _circles[0], _circles[1]],
    [50, _circles[0], _ellipses[1]],
    [50, _ellipses[0], _circles[1]],
    [50, _ellipses[0], _ellipses[1]],
    [20, _polygons[0], _polygons[1]],
    [20, _polygons[0], _lines[1]],
  ] as const)(
    "the first shape (%p) contains the second shape (%p) with padding %p",
    (padding: number, shape0: Shape<ad.Num>, shape1: Shape<ad.Num>) => {
      // The condition should be satisfied
      const overlap = -padding;
      expectSatified(constrDict.overlapping.body(shape0, shape1, overlap));
      expectSatified(constrDict.overlapping.body(shape1, shape0, overlap));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.disjoint.body(shape0, shape1, padding));
      expectNotSatisfied(constrDict.disjoint.body(shape1, shape0, padding));
      // The condition should be satisfied ONLY ONE WAY
      expectSatified(constrDict.contains.body(shape0, shape1, padding));
      expectNotSatisfied(constrDict.contains.body(shape1, shape0, padding));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.touching.body(shape1, shape0, padding));
      expectNotSatisfied(constrDict.touching.body(shape1, shape0, padding));
    }
  );
});

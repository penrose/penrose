import { constOf, numOf } from "engine/Autodiff";
import { constrDict } from "contrib/Constraints";
import {
  _rectangles,
  _circles,
} from "contrib/__testfixtures__/Constraints.input";

const digitPrecision = 10;
const toleranceValue = 10;

describe("simple constraint", () => {
  it.each([
    [1, 1, 0],
    [2, 1, 1],
    [3, 5, 2],
    [4, 5, 1],
  ])(
    "equal(%p, %p) should return %p",
    (x: number, y: number, expected: number) => {
      const result = constrDict.equal(constOf(x), constOf(y));
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
      const result = constrDict.lessThan(constOf(x), constOf(y), padding);
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
      const result = constrDict.lessThanSq(constOf(x), constOf(y));
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );

  it.each([
    [0, 1, 3, 3],
    [1, 1, 3, 0],
    [2, 1, 3, -1],
    [3, 1, 3, 0],
    [4, 1, 3, 3],
  ])(
    "inRange(%p, %p, %p) should return %p",
    (x: number, x0: number, x1: number, expected: number) => {
      const result = constrDict.inRange(constOf(x), constOf(x0), constOf(x1));
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );

  it.each([
    [1, 4, 1, 4, 0],
    [1, 4, 1, 3, 0],
    [1, 4, 2, 4, 0],
    [1, 4, 2, 5, 1],
    [1, 4, 0, 3, 1],
    [1, 4, 6, 7, 9],
    [1, 4, -2, -1, 9],
  ])(
    "contains1D([%p, %p], [%p, %p]) should return %p",
    (l1: number, r1: number, l2: number, r2: number, expected: number) => {
      const result = constrDict.contains1D(
        [constOf(l1), constOf(r1)],
        [constOf(l2), constOf(r2)]
      );
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
      const result = constrDict.disjointScalar(
        constOf(c),
        constOf(left),
        constOf(right)
      );
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
      const result = constrDict.perpendicular(
        q.map(constOf),
        p.map(constOf),
        r.map(constOf)
      );
      expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
    }
  );

  it.each([
    [[1, 2], [1, 1], [2, 1], 0.6],
    [[1, 3], [1, 1], [3, 1], 1.2],
    [[1, 0], [1, 1], [1, 2], 0],
    [[1, 0], [1, 1], [1, 10], 0],
    [[1, 0], [1, 1], [1, -10], 2],
  ])(
    "collinear(%p, %p, %p) should return %p",
    (c1: number[], c2: number[], c3: number[], expected: number) => {
      const result = constrDict.collinear(
        c1.map(constOf),
        c2.map(constOf),
        c3.map(constOf)
      );
      expect(numOf(result)).toBeCloseTo(expected, 1);
    }
  );

  it.each([
    [[1, 2], [1, 1], [2, 1], 0.6],
    [[1, 3], [1, 1], [3, 1], 1.2],
    [[1, 0], [1, 1], [1, 2], 0],
    [[1, 0], [1, 1], [1, 10], 0],
    [[1, 0], [1, 1], [1, -10], 0],
  ])(
    "collinearUnordered(%p, %p, %p) should return %p",
    (c1: number[], c2: number[], c3: number[], expected: number) => {
      const result = constrDict.collinearUnordered(
        c1.map(constOf),
        c2.map(constOf),
        c3.map(constOf)
      );
      expect(numOf(result)).toBeCloseTo(expected, 1);
    }
  );
});

describe("general constraints", () => {
  // Overlapping shapes
  it.each([
    // Zero padding
    ["Rectangle", "Rectangle", 0, _rectangles[0], _rectangles[2]],
    ["Rectangle", "Circle", 0, _rectangles[0], _circles[2]],
    ["Circle", "Rectangle", 0, _circles[0], _rectangles[2]],
    ["Circle", "Circle", 0, _circles[0], _circles[2]],
    // Possitive padding
    ["Rectangle", "Rectangle", 150, _rectangles[3], _rectangles[1]],
    ["Rectangle", "Circle", 150, _rectangles[3], _circles[1]],
    ["Circle", "Rectangle", 150, _circles[3], _rectangles[1]],
    ["Circle", "Circle", 150, _circles[3], _circles[1]],
    // // Negative padding
    ["Rectangle", "Rectangle", -50, _rectangles[0], _rectangles[2]],
    ["Rectangle", "Circle", -50, _rectangles[0], _circles[2]],
    ["Circle", "Rectangle", -50, _circles[0], _rectangles[2]],
    ["Circle", "Circle", -50, _circles[0], _circles[2]],
  ])(
    "overlapping %p and %p with padding %p",
    (
      shapeType0: string,
      shapeType1: string,
      padding: number,
      shapeData0: any,
      shapeData1: any
    ) => {
      const shape0: [string, any] = [shapeType0, shapeData0];
      const shape1: [string, any] = [shapeType1, shapeData1];
      // The condition should be satisfied
      expect(
        numOf(constrDict.overlapping(shape0, shape1, padding))
      ).toBeLessThanOrEqual(0);
      expect(
        numOf(constrDict.overlapping(shape1, shape0, padding))
      ).toBeLessThanOrEqual(0);
      // The condition should NOT be satisfied
      expect(
        numOf(constrDict.disjoint(shape0, shape1, padding))
      ).toBeGreaterThan(toleranceValue);
      expect(
        numOf(constrDict.disjoint(shape1, shape0, padding))
      ).toBeGreaterThan(toleranceValue);
      // The condition should NOT be satisfied
      expect(
        numOf(constrDict.tangentTo(shape0, shape1, padding))
      ).toBeGreaterThan(toleranceValue);
      expect(
        numOf(constrDict.tangentTo(shape1, shape0, padding))
      ).toBeGreaterThan(toleranceValue);
      // The condition should NOT be satisfied
      expect(
        numOf(constrDict.contains(shape0, shape1, padding))
      ).toBeGreaterThan(toleranceValue);
      expect(
        numOf(constrDict.contains(shape1, shape0, padding))
      ).toBeGreaterThan(toleranceValue);
      // The condition should NOT be satisfied
      expect(numOf(constrDict.atDist(shape0, shape1, padding))).toBeGreaterThan(
        toleranceValue
      );
      expect(numOf(constrDict.atDist(shape1, shape0, padding))).toBeGreaterThan(
        toleranceValue
      );
    }
  );

  // Disjoint shapes
  it.each([
    // Zero padding
    ["Rectangle", "Rectangle", 0, _rectangles[2], _rectangles[3]],
    ["Rectangle", "Circle", 0, _rectangles[2], _circles[3]],
    ["Circle", "Rectangle", 0, _circles[2], _rectangles[3]],
    ["Circle", "Circle", 0, _circles[2], _circles[3]],
    // // Possitive padding
    ["Rectangle", "Rectangle", 10, _rectangles[1], _rectangles[3]],
    // ["Rectangle", "Circle", 10, _rectangles[1], _circles[3]],
    // ["Circle", "Rectangle", 10, _circles[1], _rectangles[3]],
    ["Circle", "Circle", 10, _circles[1], _circles[3]],
    // // Negative padding
    ["Rectangle", "Rectangle", -10, _rectangles[2], _rectangles[1]],
    // ["Rectangle", "Circle", -10, _rectangles[2], _circles[1]],
    // ["Circle", "Rectangle", -10, _circles[2], _rectangles[1]],
    ["Circle", "Circle", -10, _circles[2], _circles[1]],
  ])(
    "disjoint %p and %p with padding %p",
    (
      shapeType0: string,
      shapeType1: string,
      padding: number,
      shapeData0: any,
      shapeData1: any
    ) => {
      const shape0: [string, any] = [shapeType0, shapeData0];
      const shape1: [string, any] = [shapeType1, shapeData1];
      // The condition should NOT be satisfied
      // expect(numOf(constrDict.overlapping(shape0, shape1, padding))).toBeGreaterThan(toleranceValue);
      // expect(numOf(constrDict.overlapping(shape1, shape0, padding))).toBeGreaterThan(toleranceValue);
      // The condition should be satisfied
      expect(
        numOf(constrDict.disjoint(shape0, shape1, padding))
      ).toBeLessThanOrEqual(0);
      expect(
        numOf(constrDict.disjoint(shape1, shape0, padding))
      ).toBeLessThanOrEqual(0);
      // The condition should NOT be satisfied
      // expect(numOf(constrDict.tangentTo(shape0, shape1, padding))).toBeGreaterThan(toleranceValue);
      // expect(numOf(constrDict.tangentTo(shape1, shape0, padding))).toBeGreaterThan(toleranceValue);
      // The condition should NOT be satisfied
      expect(
        numOf(constrDict.contains(shape0, shape1, padding))
      ).toBeGreaterThan(toleranceValue);
      expect(
        numOf(constrDict.contains(shape1, shape0, padding))
      ).toBeGreaterThan(toleranceValue);
      // The condition should NOT be satisfied
      // expect(numOf(constrDict.atDist(shape0, shape1, padding))).toBeGreaterThan(toleranceValue);
      // expect(numOf(constrDict.atDist(shape1, shape0, padding))).toBeGreaterThan(toleranceValue);
    }
  );

  // Tangent shapes
  it.each([
    // Zero padding
    ["Rectangle", "Rectangle", 0, _rectangles[1], _rectangles[2]],
    // ["Rectangle", "Circle", 0, _rectangles[1], _circles[2]],
    // ["Circle", "Rectangle", 0, _circles[1], _rectangles[2]],
    // ["Circle", "Circle", 0, _circles[1], _circles[2]],
    // // Possitive padding
    ["Rectangle", "Rectangle", 100, _rectangles[1], _rectangles[3]],
    // ["Rectangle", "Circle", 100, _rectangles[1], _circles[3]],
    // ["Circle", "Rectangle", 100, _circles[1], _rectangles[3]],
    // ["Circle", "Circle", 100, _circles[1], _circles[3]],
    // // Negative padding
    ["Rectangle", "Rectangle", -100, _rectangles[0], _rectangles[2]],
    // ["Rectangle", "Circle", -100, _rectangles[0], _circles[2]],
    // ["Circle", "Rectangle", -100, _circles[0], _rectangles[2]],
    // ["Circle", "Circle", -100, _circles[0], _circles[2]],
  ])(
    "tangent %p and %p with padding %p",
    (
      shapeType0: string,
      shapeType1: string,
      padding: number,
      shapeData0: any,
      shapeData1: any
    ) => {
      const shape0: [string, any] = [shapeType0, shapeData0];
      const shape1: [string, any] = [shapeType1, shapeData1];
      // The condition should JUST be satisfied
      expect(
        numOf(constrDict.overlapping(shape0, shape1, padding))
      ).toBeCloseTo(0, digitPrecision);
      expect(
        numOf(constrDict.overlapping(shape1, shape0, padding))
      ).toBeCloseTo(0, digitPrecision);
      // The condition should JUST be satisfied
      expect(numOf(constrDict.disjoint(shape0, shape1, padding))).toBeCloseTo(
        0,
        digitPrecision
      );
      expect(numOf(constrDict.disjoint(shape1, shape0, padding))).toBeCloseTo(
        0,
        digitPrecision
      );
      // The condition should be satisfied
      expect(
        numOf(constrDict.tangentTo(shape0, shape1, padding))
      ).toBeLessThanOrEqual(0);
      expect(
        numOf(constrDict.tangentTo(shape1, shape0, padding))
      ).toBeLessThanOrEqual(0);
      // The condition should NOT be satisfied
      expect(
        numOf(constrDict.contains(shape0, shape1, padding))
      ).toBeGreaterThan(toleranceValue);
      expect(
        numOf(constrDict.contains(shape1, shape0, padding))
      ).toBeGreaterThan(toleranceValue);
      // The condition should be satisfied
      expect(
        numOf(constrDict.atDist(shape0, shape1, padding))
      ).toBeLessThanOrEqual(0);
      expect(
        numOf(constrDict.atDist(shape1, shape0, padding))
      ).toBeLessThanOrEqual(0);
    }
  );

  // The first shapes is contained in the second one
  it.each([
    // Zero padding
    ["Rectangle", "Rectangle", 0, _rectangles[0], _rectangles[1]],
    ["Rectangle", "Circle", 0, _rectangles[0], _circles[1]],
    ["Circle", "Rectangle", 0, _circles[0], _rectangles[1]],
    ["Circle", "Circle", 0, _circles[0], _circles[1]],
    // // Possitive padding
    // ["Rectangle", "Rectangle", 300, _rectangles[0], _rectangles[2]],
    // ["Rectangle", "Circle", 300, _rectangles[0], _circles[2]],
    // ["Circle", "Rectangle", 300, _circles[0], _rectangles[2]],
    // ["Circle", "Circle", 300, _circles[0], _circles[2]],
    // // Negative padding
    ["Rectangle", "Rectangle", -10, _rectangles[0], _rectangles[1]],
    ["Rectangle", "Circle", -10, _rectangles[0], _circles[1]],
    ["Circle", "Rectangle", -10, _circles[0], _rectangles[1]],
    ["Circle", "Circle", -10, _circles[0], _circles[1]],
  ])(
    "the first shape (%p) contains the second shape (%p) with padding %p",
    (
      shapeType0: string,
      shapeType1: string,
      padding: number,
      shapeData0: any,
      shapeData1: any
    ) => {
      const shape0: [string, any] = [shapeType0, shapeData0];
      const shape1: [string, any] = [shapeType1, shapeData1];
      // The condition should be satisfied
      expect(
        numOf(constrDict.overlapping(shape0, shape1, padding))
      ).toBeLessThanOrEqual(0);
      expect(
        numOf(constrDict.overlapping(shape1, shape0, padding))
      ).toBeLessThanOrEqual(0);
      // The condition should NOT be satisfied
      expect(
        numOf(constrDict.disjoint(shape0, shape1, padding))
      ).toBeGreaterThan(toleranceValue);
      expect(
        numOf(constrDict.disjoint(shape1, shape0, padding))
      ).toBeGreaterThan(toleranceValue);
      // The condition should NOT be satisfied
      expect(
        numOf(constrDict.tangentTo(shape0, shape1, padding))
      ).toBeGreaterThan(toleranceValue);
      expect(
        numOf(constrDict.tangentTo(shape1, shape0, padding))
      ).toBeGreaterThan(toleranceValue);
      // The condition should be satisfied ONLY ONE WAY
      expect(
        numOf(constrDict.contains(shape0, shape1, padding))
      ).toBeLessThanOrEqual(0);
      expect(
        numOf(constrDict.contains(shape1, shape0, padding))
      ).toBeGreaterThan(toleranceValue);
      // The condition should NOT be satisfied
      expect(numOf(constrDict.atDist(shape1, shape0, padding))).toBeGreaterThan(
        toleranceValue
      );
      expect(numOf(constrDict.atDist(shape1, shape0, padding))).toBeGreaterThan(
        toleranceValue
      );
    }
  );
});

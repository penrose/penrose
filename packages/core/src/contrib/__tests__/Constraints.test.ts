import { constrDict } from "contrib/Constraints";
import {
  _circles,
  _ellipses,
  _lines,
  _polygons,
  _rectangles,
} from "contrib/__testfixtures__/TestShapes.input";
import { genCode, secondaryGraph } from "engine/Autodiff";
import * as ad from "types/ad";

const numOf = (x: ad.Num) => {
  const g = secondaryGraph([x]);
  const f = genCode(g);
  const [y] = f([]).secondary; // no inputs, so, empty array
  return y;
};

const digitPrecision = 10;

describe("simple constraint", () => {
  it.each([
    [1, 1, 0],
    [2, 1, 1],
    [3, 5, 2],
    [4, 5, 1],
  ])(
    "equal(%p, %p) should return %p",
    (x: number, y: number, expected: number) => {
      const result = constrDict.equal(x, y);
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
      const result = constrDict.lessThan(x, y, padding);
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
      const result = constrDict.greaterThan(x, y, padding);
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
      const result = constrDict.lessThanSq(x, y);
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
      const result = constrDict.greaterThanSq(x, y);
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
      const result = constrDict.inRange(x, x0, x1);
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
      const result = constrDict.contains1D([l1, r1], [l2, r2]);
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
      const result = constrDict.disjointScalar(c, left, right);
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
      const result = constrDict.perpendicular(q, p, r);
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
      const result = constrDict.collinear(c1, c2, c3);
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
      const result = constrDict.collinearUnordered(c1, c2, c3);
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
    ["Rectangle", "Rectangle", 0, _rectangles[0], _rectangles[2]],
    ["Rectangle", "Circle", 0, _rectangles[0], _circles[2]],
    ["Circle", "Rectangle", 0, _circles[0], _rectangles[2]],
    ["Circle", "Circle", 0, _circles[0], _circles[2]],
    ["Rectangle", "Ellipse", 0, _rectangles[0], _ellipses[2]],
    ["Ellipse", "Rectangle", 0, _ellipses[0], _rectangles[2]],
    ["Line", "Line", 0, _lines[2], _lines[3]],
    ["Polygon", "Polygon", 0, _polygons[2], _polygons[3]],
    ["Polygon", "Polygon", 0, _polygons[0], _polygons[3]],
    ["Polygon", "Polygon", 0, _polygons[0], _polygons[4]],
    ["Line", "Polygon", 0, _lines[3], _polygons[0]],
    ["Line", "Polygon", 0, _lines[2], _polygons[3]],
    ["Rectangle", "Line", 0, _rectangles[0], _lines[0]],
    ["Rectangle", "Polygon", 0, _rectangles[2], _polygons[2]],
    ["Rectangle", "Polygon", 0, _rectangles[0], _polygons[0]],
    ["Ellipse", "Line", 0, _ellipses[6], _lines[0]],
    // With padding
    ["Rectangle", "Rectangle", 150, _rectangles[3], _rectangles[1]],
    ["Rectangle", "Circle", 150, _rectangles[3], _circles[1]],
    ["Circle", "Rectangle", 150, _circles[3], _rectangles[1]],
    ["Rectangle", "Ellipse", 150, _rectangles[3], _ellipses[1]],
    ["Ellipse", "Rectangle", 150, _ellipses[3], _rectangles[1]],
    ["Circle", "Circle", 150, _circles[3], _circles[1]],
    ["Line", "Line", 200, _lines[1], _lines[3]],
    ["Polygon", "Polygon", 100, _polygons[1], _polygons[2]],
    ["Polygon", "Polygon", 150, _polygons[1], _polygons[3]],
    ["Line", "Polygon", 150, _lines[1], _polygons[3]],
    ["Line", "Polygon", 150, _lines[1], _polygons[2]],
    ["Rectangle", "Line", 150, _rectangles[0], _lines[2]],
    ["Rectangle", "Polygon", 150, _rectangles[3], _polygons[1]],
    ["Rectangle", "Polygon", 150, _rectangles[1], _polygons[2]],
    [
      "Circle",
      "Rectangle",
      (100 * (Math.SQRT2 - 1)) / Math.SQRT2 + 10,
      _circles[4],
      _rectangles[2],
    ],
    [
      "Ellipse",
      "Rectangle",
      (100 * (Math.SQRT2 - 1)) / Math.SQRT2 + 10,
      _ellipses[4],
      _rectangles[2],
    ], // NEW
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
      expectSatified(constrDict.overlapping(shape0, shape1, padding));
      expectSatified(constrDict.overlapping(shape1, shape0, padding));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.disjoint(shape0, shape1, padding));
      expectNotSatisfied(constrDict.disjoint(shape1, shape0, padding));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.contains(shape0, shape1, padding));
      expectNotSatisfied(constrDict.contains(shape1, shape0, padding));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.touching(shape0, shape1, padding));
      expectNotSatisfied(constrDict.touching(shape1, shape0, padding));
    }
  );

  // Disjoint shapes
  it.each([
    // Without padding
    ["Rectangle", "Rectangle", 0, _rectangles[2], _rectangles[3]],
    ["Rectangle", "Circle", 0, _rectangles[2], _circles[3]],
    ["Circle", "Rectangle", 0, _circles[2], _rectangles[3]],
    ["Rectangle", "Ellipse", 0, _rectangles[2], _ellipses[3]],
    ["Ellipse", "Rectangle", 0, _ellipses[2], _rectangles[3]],
    ["Circle", "Circle", 0, _circles[2], _circles[3]],
    ["Line", "Line", 0, _lines[0], _lines[3]],
    ["Line", "Line", 0, _lines[1], _lines[2]],
    ["Line", "Polygon", 0, _lines[0], _polygons[2]],
    ["Line", "Polygon", 0, _lines[0], _polygons[3]],
    ["Rectangle", "Polygon", 0, _rectangles[1], _polygons[3]],
    ["Rectangle", "Line", 0, _rectangles[2], _lines[2]],
    ["Circle", "Rectangle", 0, _circles[3], _rectangles[2]],
    ["Ellipse", "Rectangle", 0, _ellipses[3], _rectangles[2]],
    ["Ellipse", "Line", 0, _ellipses[7], _lines[2]],
    // With padding
    ["Rectangle", "Rectangle", 10, _rectangles[1], _rectangles[3]],
    ["Rectangle", "Circle", 10, _rectangles[1], _circles[3]],
    ["Circle", "Rectangle", 10, _circles[1], _rectangles[3]],
    ["Rectangle", "Ellipse", 10, _rectangles[1], _ellipses[3]],
    ["Ellipse", "Rectangle", 10, _ellipses[1], _rectangles[3]],
    ["Circle", "Circle", 10, _circles[1], _circles[3]],
    ["Circle", "Circle", 110, _circles[2], _circles[3]],
    ["Line", "Line", 50, _lines[0], _lines[3]],
    ["Line", "Line", 50, _lines[1], _lines[2]],
    ["Line", "Polygon", 50, _lines[0], _polygons[2]],
    ["Line", "Polygon", 50, _lines[0], _polygons[3]],
    ["Rectangle", "Polygon", 50, _rectangles[1], _polygons[3]],
    ["Rectangle", "Line", 50, _rectangles[2], _lines[2]],
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
      expectNotSatisfied(constrDict.overlapping(shape0, shape1, padding));
      expectNotSatisfied(constrDict.overlapping(shape1, shape0, padding));
      // The condition should be satisfied
      expectSatified(constrDict.disjoint(shape0, shape1, padding));
      expectSatified(constrDict.disjoint(shape1, shape0, padding));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.contains(shape0, shape1, padding));
      expectNotSatisfied(constrDict.contains(shape1, shape0, padding));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.touching(shape0, shape1, padding));
      expectNotSatisfied(constrDict.touching(shape1, shape0, padding));
    }
  );

  // Touching shapes
  it.each([
    // Without padding
    ["Rectangle", "Rectangle", 0, _rectangles[1], _rectangles[2]],
    ["Rectangle", "Circle", 0, _rectangles[1], _circles[2]],
    ["Circle", "Rectangle", 0, _circles[1], _rectangles[2]],
    ["Rectangle", "Ellipse", 0, _rectangles[1], _ellipses[2]],
    ["Ellipse", "Rectangle", 0, _ellipses[1], _rectangles[2]],
    ["Circle", "Circle", 0, _circles[1], _circles[2]],
    ["Line", "Line", 0, _lines[0], _lines[1]],
    ["Line", "Line", 0, _lines[0], _lines[2]],
    ["Line", "Polygon", 0, _lines[2], _polygons[2]],
    ["Rectangle", "Polygon", 0, _rectangles[1], _polygons[1]],
    ["Rectangle", "Line", 0, _rectangles[1], _lines[1]],
    // With padding
    ["Rectangle", "Rectangle", 100, _rectangles[1], _rectangles[3]],
    ["Rectangle", "Circle", 100, _rectangles[1], _circles[3]],
    ["Circle", "Rectangle", 100, _circles[1], _rectangles[3]],
    ["Rectangle", "Ellipse", 100, _rectangles[1], _ellipses[3]],
    ["Ellipse", "Rectangle", 100, _ellipses[1], _rectangles[3]],
    ["Circle", "Circle", 100, _circles[1], _circles[3]],
    ["Line", "Line", 100, _lines[1], _lines[2]],
    [
      "Circle",
      "Rectangle",
      (100 * (Math.SQRT2 - 1)) / Math.SQRT2,
      _circles[4],
      _rectangles[2],
    ],
  ])(
    "touching %p and %p with padding %p",
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
      expectJustSatified(constrDict.overlapping(shape0, shape1, padding));
      expectJustSatified(constrDict.overlapping(shape1, shape0, padding));
      // The condition should JUST be satisfied
      expectJustSatified(constrDict.disjoint(shape0, shape1, padding));
      expectJustSatified(constrDict.disjoint(shape1, shape0, padding));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.contains(shape0, shape1, padding));
      expectNotSatisfied(constrDict.contains(shape1, shape0, padding));
      // The condition should be satisfied
      expectSatified(constrDict.touching(shape0, shape1, padding));
      expectSatified(constrDict.touching(shape1, shape0, padding));
    }
  );

  // The first shapes is contained in the second one
  it.each([
    // Without padding
    ["Rectangle", "Rectangle", 0, _rectangles[0], _rectangles[1]],
    ["Rectangle", "Circle", 0, _rectangles[0], _circles[1]],
    ["Circle", "Rectangle", 0, _circles[0], _rectangles[1]],
    ["Rectangle", "Ellipse", 0, _rectangles[0], _ellipses[1]],
    ["Ellipse", "Rectangle", 0, _ellipses[0], _rectangles[1]],
    ["Circle", "Circle", 0, _circles[0], _circles[1]],
    ["Polygon", "Polygon", 0, _polygons[0], _polygons[1]],
    ["Polygon", "Line", 0, _polygons[0], _lines[1]],
    ["Rectangle", "Polygon", 0, _rectangles[0], _polygons[1]],
    ["Rectangle", "Line", 0, _rectangles[0], _lines[1]],
    ["Polygon", "Circle", 0, _polygons[0], _circles[5]],
    ["Circle", "Polygon", 0, _circles[6], _polygons[1]],
    ["Polygon", "Ellipse", 0, _polygons[0], _ellipses[5]],
    ["Ellipse", "Polygon", 0, _ellipses[6], _polygons[1]],
    // With padding
    ["Rectangle", "Rectangle", 50, _rectangles[0], _rectangles[1]],
    ["Rectangle", "Circle", 50, _rectangles[0], _circles[1]],
    ["Circle", "Rectangle", 50, _circles[0], _rectangles[1]],
    ["Rectangle", "Ellipse", 50, _rectangles[0], _ellipses[1]],
    ["Ellipse", "Rectangle", 50, _ellipses[0], _rectangles[1]],
    ["Circle", "Circle", 50, _circles[0], _circles[1]],
    ["Polygon", "Polygon", 20, _polygons[0], _polygons[1]],
    ["Polygon", "Line", 20, _polygons[0], _lines[1]],
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
      expectSatified(constrDict.overlapping(shape0, shape1, padding));
      expectSatified(constrDict.overlapping(shape1, shape0, padding));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.disjoint(shape0, shape1, padding));
      expectNotSatisfied(constrDict.disjoint(shape1, shape0, padding));
      // The condition should be satisfied ONLY ONE WAY
      expectSatified(constrDict.contains(shape0, shape1, padding));
      expectNotSatisfied(constrDict.contains(shape1, shape0, padding));
      // The condition should NOT be satisfied
      expectNotSatisfied(constrDict.touching(shape1, shape0, padding));
      expectNotSatisfied(constrDict.touching(shape1, shape0, padding));
    }
  );
});

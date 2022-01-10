import { constOf, numOf } from "engine/Autodiff";
import { constrDict } from "contrib/Constraints";

const digitPrecision = 10;

describe("simple constraint", () => {

  it.each([
    [1, 1, 0],
    [2, 1, 1],
    [3, 5, 2],
    [4, 5, 1],
  ])('equal(%p, %p) should return %p', (
    x: number, 
    y: number, 
    expected: number,
  ) => {
    const result = constrDict.equal(
      constOf(x),
      constOf(y),
    );
    expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
  });

  it.each([
    [1, 1, 0, 0],
    [2, 1, 0, 1],
    [3, 5, 0, -2],
    [4, 5, 0, -1],
    [2, 1, -1, 0],
    [4, 5, 1, 0],
  ])('lessThan(%p, %p, padding=%p) should return %p', (
    x: number, 
    y: number, 
    padding: number,
    expected: number,
  ) => {
    const result = constrDict.lessThan(
      constOf(x),
      constOf(y),
      padding,
    );
    expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
  });

  it.each([
    [1, 1, 0],
    [2, 1, 1],
    [5, 3, 4],
    [4, 5, 0],
  ])('lessThanSq(%p, %p) should return %p', (
    x: number, 
    y: number, 
    expected: number,
  ) => {
    const result = constrDict.lessThanSq(
      constOf(x), 
      constOf(y),
    );
    expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
  });

  it.each([
    [0, 1, 3, 3],
    [1, 1, 3, 0],
    [2, 1, 3, -1],
    [3, 1, 3, 0],
    [4, 1, 3, 3],
  ])('inRange(%p, %p, %p) should return %p', (
    x: number, 
    x0: number, 
    x1: number,
    expected: number,
  ) => {
    const result = constrDict.inRange(
      constOf(x),
      constOf(x0),
      constOf(x1),
    );
    expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
  });

  it.each([
    [1, 4, 1, 4, 0],
    [1, 4, 1, 3, 0],
    [1, 4, 2, 4, 0],
    [1, 4, 2, 5, 1],
    [1, 4, 0, 3, 1],
    [1, 4, 6, 7, 9],
    [1, 4, -2, -1, 9],
  ])('contains1D([%p, %p], [%p, %p]) should return %p', (
    l1: number,
    r1: number,
    l2: number, 
    r2: number,
    expected: number,
  ) => {
    const result = constrDict.contains1D(
      [constOf(l1), constOf(r1)], 
      [constOf(l2), constOf(r2)],
    );
    expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
  });

  it.each([
    [0, 1, 5, 0],
    [2, 1, 5, 1],
    [3, 1, 5, 2],
    [4, 1, 5, 1],
    [6, 1, 5, 0],
  ])('disjointScalar(%p, %p, %p) should return %p', (
    c: number, 
    left: number, 
    right: number,
    expected: number,
  ) => {
    const result = constrDict.disjointScalar(
      constOf(c),
      constOf(left),
      constOf(right),
    );
    expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
  });

  it.each([
    [[1, 2], [1, 1], [2, 1], 0],
    [[1, 3], [1, 1], [3, 1], 0],
    [[1, 0], [1, 1], [1, 2], 1],
    [[1, 0], [1, 1], [1, 10], 9],
    [[1, 0], [1, 1], [1, -10], 11],
  ])('perpendicular(%p, %p, %p) should return %p', (
    q: number[], 
    p: number[], 
    r: number[],
    expected: number,
  ) => {
    const result = constrDict.perpendicular(
      q.map(constOf),
      p.map(constOf),
      r.map(constOf),
    );
    expect(numOf(result)).toBeCloseTo(expected, digitPrecision);
  });

  it.each([
    [[1, 2], [1, 1], [2, 1], 0.6],
    [[1, 3], [1, 1], [3, 1], 1.2],
    [[1, 0], [1, 1], [1, 2], 0],
    [[1, 0], [1, 1], [1, 10], 0],
    [[1, 0], [1, 1], [1, -10], 2],
  ])('collinear(%p, %p, %p) should return %p', (
    c1: number[], 
    c2: number[], 
    c3: number[],
    expected: number,
  ) => {
    const result = constrDict.collinear(
      c1.map(constOf),
      c2.map(constOf),
      c3.map(constOf),
    );
    expect(numOf(result)).toBeCloseTo(expected, 1);
  });

  it.each([
    [[1, 2], [1, 1], [2, 1], 0.6],
    [[1, 3], [1, 1], [3, 1], 1.2],
    [[1, 0], [1, 1], [1, 2], 0],
    [[1, 0], [1, 1], [1, 10], 0],
    [[1, 0], [1, 1], [1, -10], 0],
  ])('collinearUnordered(%p, %p, %p) should return %p', (
    c1: number[], 
    c2: number[], 
    c3: number[],
    expected: number,
  ) => {
    const result = constrDict.collinearUnordered(
      c1.map(constOf),
      c2.map(constOf),
      c3.map(constOf),
    );
    expect(numOf(result)).toBeCloseTo(expected, 1);
  });

});

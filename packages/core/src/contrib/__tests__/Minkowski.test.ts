import {
  rectangleDifference,
  outwardUnitNormal,
  halfPlaneSDF,
} from "contrib/Minkowski";
import * as BBox from "engine/BBox";
import { constOf, numOf, ops } from "engine/Autodiff";
import { sub } from "engine/AutodiffFunctions";

const digitPrecision = 4;

describe("rectangleDifference", () => {
  let testBBox1 = BBox.bbox(constOf(2.0), constOf(2.0), [
    constOf(0.0),
    constOf(0.0),
  ]);
  let testBBox2 = BBox.bbox(constOf(3.0), constOf(1.0), [
    constOf(0.5),
    constOf(1.5),
  ]);

  test("without padding", async () => {
    let result = rectangleDifference(testBBox1, testBBox2, constOf(0.0));
    expect(numOf(result[0][0])).toEqual(-3);
    expect(numOf(result[0][1])).toEqual(-3);
    expect(numOf(result[1][0])).toEqual(2);
    expect(numOf(result[1][1])).toEqual(0);
  });

  test("with padding", async () => {
    let result = rectangleDifference(testBBox1, testBBox2, constOf(10.0));
    expect(numOf(result[0][0])).toEqual(-13);
    expect(numOf(result[0][1])).toEqual(-13);
    expect(numOf(result[1][0])).toEqual(12);
    expect(numOf(result[1][1])).toEqual(10);
  });

  test("reversed order", async () => {
    let result = rectangleDifference(testBBox2, testBBox1, constOf(0.0));
    expect(numOf(result[0][0])).toEqual(-2);
    expect(numOf(result[0][1])).toEqual(0);
    expect(numOf(result[1][0])).toEqual(3);
    expect(numOf(result[1][1])).toEqual(3);
  });

  test("same bounding box", async () => {
    let result = rectangleDifference(testBBox1, testBBox1, constOf(0.0));
    expect(numOf(result[0][0])).toEqual(-2);
    expect(numOf(result[0][1])).toEqual(-2);
    expect(numOf(result[1][0])).toEqual(2);
    expect(numOf(result[1][1])).toEqual(2);
  });
});

let point1 = [constOf(2), constOf(3)];
let point2 = [constOf(1), constOf(2)];
let point3 = [constOf(1), constOf(4)];
let point4 = [constOf(2), constOf(2)];
let point5 = [constOf(0), constOf(0)];
let lineSegment = [point3, point4];

describe("outwardUnitNormal", () => {
  test("inside point above", async () => {
    let result = outwardUnitNormal(lineSegment, point1);
    // It is unit
    expect(numOf(ops.vnorm(result))).toBeCloseTo(1, digitPrecision);
    // It is orthogonal to the line segment
    expect(
      numOf(ops.vdot(result, ops.vsub(lineSegment[1], lineSegment[0])))
    ).toBeCloseTo(0, digitPrecision);
    // `insidePoint1` is inside
    expect(
      numOf(sub(ops.vdot(result, point1), ops.vdot(result, lineSegment[0])))
    ).toBeLessThan(0);
  });

  test("inside point below", async () => {
    let result = outwardUnitNormal(lineSegment, point2);
    // It is unit
    expect(numOf(ops.vnorm(result))).toBeCloseTo(1, digitPrecision);
    // It is orthogonal to the line segment
    expect(
      numOf(ops.vdot(result, ops.vsub(lineSegment[1], lineSegment[0])))
    ).toBeCloseTo(0, digitPrecision);
    // `insidePoint2` is inside
    expect(
      numOf(sub(ops.vdot(result, point2), ops.vdot(result, lineSegment[0])))
    ).toBeLessThan(0);
  });
});

describe("halfPlaneSDF", () => {
  test("without padding", async () => {
    let result = halfPlaneSDF(
      [point2, point3],
      [point2, point4],
      point5,
      constOf(0.0)
    );
    expect(numOf(result)).toBeCloseTo(-3, digitPrecision);
  });

  test("with padding", async () => {
    let result = halfPlaneSDF(
      [point2, point3],
      [point2, point4],
      point5,
      constOf(10.0)
    );
    expect(numOf(result)).toBeCloseTo(-13, digitPrecision);
  });

  test("zero outside", async () => {
    let result = halfPlaneSDF([point2, point3], [point5], point1, constOf(0.0));
    expect(numOf(result)).toBeCloseTo(1, digitPrecision);
  });
});

import { rectangleDifference, outwardUnitNormal } from "contrib/Minkowski";
import * as BBox from "engine/BBox";
import { constOf, numOf, ops, sub } from "engine/Autodiff";

describe("rectangleDifference", () => {

  let testBBox1 = BBox.bbox(constOf(2.0), constOf(2.0), [constOf(0.0), constOf(0.0)])
  let testBBox2 = BBox.bbox(constOf(3.0), constOf(1.0), [constOf(0.5), constOf(1.5)])

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

describe("outwardUnitNormal", () => {

  let lineSegment = [[constOf(1), constOf(4)], [constOf(2), constOf(2)]];
  let insidePoint1 = [constOf(2), constOf(3)];
  let insidePoint2 = [constOf(1), constOf(2)];

  test("inside point above", async () => {
    let result = outwardUnitNormal(lineSegment, insidePoint1);
    // It is unit
    expect(numOf(ops.vnorm(result))).toBeCloseTo(1);
    // It is orthogonal to the line segment
    expect(numOf(ops.vdot(
      result, ops.vsub(lineSegment[1], lineSegment[0])
    ))).toBeCloseTo(0);
    // `insidePoint1` is inside
    expect(numOf(sub(
      ops.vdot(result, insidePoint1),
      ops.vdot(result, lineSegment[0])
    ))).toBeLessThan(0);
  });

  test("inside point below", async () => {
    let result = outwardUnitNormal(lineSegment, insidePoint2);
    // It is unit
    expect(numOf(ops.vnorm(result))).toBeCloseTo(1);
    // It is orthogonal to the line segment
    expect(numOf(ops.vdot(
      result, ops.vsub(lineSegment[1], lineSegment[0])
    ))).toBeCloseTo(0);
    // `insidePoint2` is inside
    expect(numOf(sub(
      ops.vdot(result, insidePoint2),
      ops.vdot(result, lineSegment[0])
    ))).toBeLessThan(0);
  });

});

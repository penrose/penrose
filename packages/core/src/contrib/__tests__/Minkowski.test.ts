import {
  halfPlaneSDF,
  outwardUnitNormal,
  rectangleDifference,
} from "contrib/Minkowski";
import { constOf, numOf, ops } from "engine/Autodiff";
import { sub } from "engine/AutodiffFunctions";
import * as BBox from "engine/BBox";

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

describe("convexPartitions", () => {
  test("Figure 1 from [HM83]", () => {
    // https://link.springer.com/content/pdf/10.1007/3-540-12689-9_105.pdf
    // point locations approximated by tracing in Inkscape
    const p1 = [-316.39758, -117.40885];
    const p2 = [-291.04166, -57.877602];
    const p3 = [-243.63715, -24.804688];
    const p4 = [-189.61806, -74.414062];
    const p5 = [-155.4427, 9.921875];
    const p6 = [-173.63281, 78.823784];
    const p7 = [-202.29601, 136.15016];
    const p8 = [-257.41754, 167.01823];
    const p9 = [-301.51477, 164.81337];
    const p10 = [-359.94357, 147.72569];
    const p11 = [-54.570312, 355.53384];
    const p12 = [288.28558, 232.61285];
    const p13 = [241.43228, 148.82812];
    const p14 = [151.5842, 66.145836];
    const p15 = [186.31076, 8.8194437];
    const p16 = [269.54428, -59.53125];
    const p17 = [219.38368, -151.03297];
    const p18 = [14.882812, -128.98438];
    const p19 = [-224.34462, -165.36458];
    const p = [
      p1,
      p2,
      p3,
      p4,
      p5,
      p6,
      p7,
      p8,
      p9,
      p10,
      p11,
      p12,
      p13,
      p14,
      p15,
      p16,
      p17,
      p18,
      p19,
    ];
  });
});

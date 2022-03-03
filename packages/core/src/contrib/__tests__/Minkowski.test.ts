import {
  convexPartitions,
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
  const convexPartitionsNum = (p: number[][]): number[][][] =>
    convexPartitions(p.map(([x, y]) => [constOf(x), constOf(y)])).map((poly) =>
      poly.map((point) => point.map(numOf))
    );

  // https://link.springer.com/content/pdf/10.1007/3-540-12689-9_105.pdf
  // point locations approximated by tracing in Inkscape; also note, this
  // polygon is actually clockwise because screen coordinates are upside down
  // compared to math
  const hm83 = [
    [-316.39758, -117.40885],
    [-291.04166, -57.877602],
    [-243.63715, -24.804688],
    [-189.61806, -74.414062],
    [-155.4427, 9.921875],
    [-173.63281, 78.823784],
    [-202.29601, 136.15016],
    [-257.41754, 167.01823],
    [-301.51477, 164.81337],
    [-359.94357, 147.72569],
    [-54.570312, 355.53384],
    [288.28558, 232.61285],
    [241.43228, 148.82812],
    [151.5842, 66.145836],
    [186.31076, 8.8194437],
    [269.54428, -59.53125],
    [219.38368, -151.03297],
    [14.882812, -128.98438],
    [-224.34462, -165.36458],
  ];

  test("Figure 1 from [HM83], unflipped", () => {
    const p = hm83;
    expect(convexPartitionsNum(p)).toEqual([
      [p[10], p[9], p[8]],
      [p[10], p[8], p[7]],
      [p[10], p[7], p[6], p[13], p[12], p[11]],
      [p[13], p[6], p[5]],
      [p[13], p[5], p[4], p[17], p[16], p[14]],
      [p[2], p[1], p[0], p[18], p[3]],
      [p[3], p[18], p[17], p[4]],
      [p[14], p[16], p[15]],
    ]);
  });

  test("Figure 1 from [HM83], flipped", () => {
    const p = hm83.map(([x, y]) => [x, -y]);
    expect(convexPartitionsNum(p)).toEqual([
      [p[8], p[9], p[10]],
      [p[7], p[8], p[10]],
      [p[12], p[13], p[6], p[7], p[10], p[11]],
      [p[5], p[6], p[13]],
      [p[16], p[17], p[4], p[5], p[13], p[14]],
      [p[18], p[0], p[1], p[2], p[3]],
      [p[17], p[18], p[3], p[4]],
      [p[16], p[14], p[15]],
    ]);
  });

  test("yin-yang", () => {
    const p = [
      [-41.942347636454556, -334.57387855868393],
      [-28.8732769036464, -514.3771026404029],
      [69.71110339328084, -340.36239072939577],
      [-104.30360851772633, -241.77801043246853],
      [-91.23453778491817, -421.58123451418754],
      [-66.58844271068637, -378.07755653643574],
    ];
    expect(convexPartitionsNum(p)).toEqual([
      [p[3], p[4], p[5], p[0]],
      [p[0], p[1], p[2]],
      [p[0], p[2], p[3]],
    ]);
  });
});

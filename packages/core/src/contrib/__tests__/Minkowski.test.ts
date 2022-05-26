import {
  convexPartitions,
  ellipseToImplicit,
  halfPlaneSDF,
  halfPlaneToImplicit,
  outwardUnitNormal,
  rectangleDifference,
} from "contrib/Minkowski";
import { genCode, ops, secondaryGraph } from "engine/Autodiff";
import { sub } from "engine/AutodiffFunctions";
import * as BBox from "engine/BBox";
import { makeEllipse } from "shapes/Ellipse";
import { makeCanvas, simpleContext } from "shapes/Samplers";
import * as ad from "types/ad";
import { black, floatV, vectorV } from "utils/Util";

const digitPrecision = 4;

const numsOf = (xs: ad.Num[]) => {
  const g = secondaryGraph(xs);
  const f = genCode(g);
  return f([]).secondary;
};

describe("rectangleDifference", () => {
  const expectRectDiff = (
    result: [ad.Pt2, ad.Pt2],
    expected: [[number, number], [number, number]]
  ) => {
    const g = secondaryGraph([
      result[0][0],
      result[0][1],
      result[1][0],
      result[1][1],
    ]);
    const f = genCode(g);
    const [result00, result01, result10, result11] = f([]).secondary;
    expect(result00).toEqual(expected[0][0]);
    expect(result01).toEqual(expected[0][1]);
    expect(result10).toEqual(expected[1][0]);
    expect(result11).toEqual(expected[1][1]);
  };

  let testBBox1 = BBox.bbox(2, 2, [0, 0]);
  let testBBox2 = BBox.bbox(3, 1, [0.5, 1.5]);

  test("without padding", async () => {
    let result = rectangleDifference(testBBox1, testBBox2, 0);
    expectRectDiff(result, [
      [-3, -3],
      [2, 0],
    ]);
  });

  test("with padding", async () => {
    let result = rectangleDifference(testBBox1, testBBox2, 10);
    expectRectDiff(result, [
      [-13, -13],
      [12, 10],
    ]);
  });

  test("reversed order", async () => {
    let result = rectangleDifference(testBBox2, testBBox1, 0);
    expectRectDiff(result, [
      [-2, 0],
      [3, 3],
    ]);
  });

  test("same bounding box", async () => {
    let result = rectangleDifference(testBBox1, testBBox1, 0);
    expectRectDiff(result, [
      [-2, -2],
      [2, 2],
    ]);
  });
});

let point1 = [2, 3];
let point2 = [1, 2];
let point3 = [1, 4];
let point4 = [2, 2];
let point5 = [0, 0];
let lineSegment = [point3, point4];

describe("outwardUnitNormal", () => {
  test("inside point above", async () => {
    let result = outwardUnitNormal(lineSegment, point1);

    const [norm, dot, diff] = genCode(
      secondaryGraph([
        ops.vnorm(result),
        ops.vdot(result, ops.vsub(lineSegment[1], lineSegment[0])),
        sub(ops.vdot(result, point1), ops.vdot(result, lineSegment[0])),
      ])
    )([]).secondary;

    // It is unit
    expect(norm).toBeCloseTo(1, digitPrecision);
    // It is orthogonal to the line segment
    expect(dot).toBeCloseTo(0, digitPrecision);
    // `insidePoint1` is inside
    expect(diff).toBeLessThan(0);
  });

  test("inside point below", async () => {
    let result = outwardUnitNormal(lineSegment, point2);

    const [norm, dot, diff] = genCode(
      secondaryGraph([
        ops.vnorm(result),
        ops.vdot(result, ops.vsub(lineSegment[1], lineSegment[0])),
        sub(ops.vdot(result, point2), ops.vdot(result, lineSegment[0])),
      ])
    )([]).secondary;

    // It is unit
    expect(norm).toBeCloseTo(1, digitPrecision);
    // It is orthogonal to the line segment
    expect(dot).toBeCloseTo(0, digitPrecision);
    // `insidePoint2` is inside
    expect(diff).toBeLessThan(0);
  });
});

describe("halfPlaneSDF", () => {
  test("without padding", async () => {
    let result = halfPlaneSDF([point2, point3], [point2, point4], point5, 0);
    expect(numsOf([result])[0]).toBeCloseTo(-3, digitPrecision);
  });

  test("with padding", async () => {
    let result = halfPlaneSDF([point2, point3], [point2, point4], point5, 10);
    expect(numsOf([result])[0]).toBeCloseTo(-13, digitPrecision);
  });

  test("zero outside", async () => {
    let result = halfPlaneSDF([point2, point3], [point5], point1, 0);
    expect(numsOf([result])[0]).toBeCloseTo(1, digitPrecision);
  });
});

describe("convexPartitions", () => {
  // note: in each of these examples, we don't need to do anything special to
  // convert `ad.Num`s to numbers because all the `ad.Num`s we're using happen
  // to already be constants

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
    expect(convexPartitions(p)).toEqual([
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
    expect(convexPartitions(p)).toEqual([
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
    expect(convexPartitions(p)).toEqual([
      [p[3], p[4], p[5], p[0]],
      [p[0], p[1], p[2]],
      [p[0], p[2], p[3]],
    ]);
  });
});

describe("toImplicit", () => {
  test("halfPlaneToImplicit", async () => {
    let result = halfPlaneToImplicit(
      [
        [1, 2],
        [2, 3],
      ],
      [1, 6],
      0
    );
    let [a, b, c] = numsOf([result.a, result.b, result.c]);
    expect(a).toBeCloseTo(1 / Math.sqrt(2), 4);
    expect(b).toBeCloseTo(-1 / Math.sqrt(2), 4);
    expect(c).toBeCloseTo(-1 / Math.sqrt(2), 4);
  });

  test("halfPlaneToImplicit with padding", async () => {
    let result = halfPlaneToImplicit(
      [
        [1, 2],
        [3, 4],
      ],
      [5, 6],
      1
    );
    let [a, b, c] = numsOf([result.a, result.b, result.c]);
    expect(a).toBeCloseTo(1 / Math.sqrt(2), 4);
    expect(b).toBeCloseTo(-1 / Math.sqrt(2), 4);
    expect(c).toBeCloseTo(-1 / Math.sqrt(2) - 1, 4);
  });

  test("ellipseToImplicit", async () => {
    let ellipse = makeEllipse(simpleContext("Test"), makeCanvas(800, 700), {
      rx: floatV(6),
      ry: floatV(3),
      center: vectorV([-11, 22]),
      strokeWidth: floatV(0),
      strokeColor: black(),
    });
    let result = ellipseToImplicit(ellipse);
    let [a, b, c, x, y] = numsOf([
      result.a,
      result.b,
      result.c,
      result.x,
      result.y,
    ]);
    expect(a).toEqual(0.5);
    expect(b).toEqual(2);
    expect(c).toEqual(18);
    expect(x).toEqual(-11);
    expect(y).toEqual(22);
  });
});

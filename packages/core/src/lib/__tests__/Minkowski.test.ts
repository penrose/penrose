import { scalar } from "@tensorflow/tfjs";
import { describe, expect, test } from "vitest";
import * as BBox from "../../engine/BBox.js";
import * as ad from "../../types/ad.js";
import {
  containsConvexPolygonPoints,
  convexPartitions,
  halfPlaneSDF,
  rectangleDifference,
} from "../Minkowski.js";
import { bboxPts } from "../Queries.js";
import { numsOf } from "../Utils.js";

describe("containsConvexPolygonPoints", () => {
  test("test", () => {
    const poly = [
      [scalar(0), scalar(0)],
      [scalar(1), scalar(0)],
      [scalar(1), scalar(1)],
      [scalar(0), scalar(1)],
    ];
    const pt = [scalar(0.25), scalar(0.25)];

    expect(
      numsOf([containsConvexPolygonPoints(poly, pt, scalar(0))])[0],
    ).toBeLessThanOrEqual(0);

    expect(
      numsOf([containsConvexPolygonPoints(poly, pt, scalar(0.1))])[0],
    ).toBeLessThanOrEqual(0);

    expect(
      numsOf([containsConvexPolygonPoints(poly, pt, scalar(0.26))])[0],
    ).toBeGreaterThan(0);
  });
});

describe("rectangleDifference", () => {
  const expectRectDiff = (
    result: [ad.Pt2, ad.Pt2],
    expected: [[number, number], [number, number]],
  ) => {
    const [result00, result01, result10, result11] = numsOf([
      result[0][0],
      result[0][1],
      result[1][0],
      result[1][1],
    ]);
    expect(result00).toEqual(expected[0][0]);
    expect(result01).toEqual(expected[0][1]);
    expect(result10).toEqual(expected[1][0]);
    expect(result11).toEqual(expected[1][1]);
  };

  let testBBox1 = bboxPts(
    BBox.bbox(scalar(2), scalar(2), [scalar(0), scalar(0)]),
  );
  let testBBox2 = bboxPts(
    BBox.bbox(scalar(3), scalar(1), [scalar(0.5), scalar(1.5)]),
  );

  test("without padding", async () => {
    let result = rectangleDifference(testBBox1, testBBox2, scalar(0));
    expectRectDiff(result, [
      [-3, -3],
      [2, 0],
    ]);
  });

  test("with padding", async () => {
    let result = rectangleDifference(testBBox1, testBBox2, scalar(10));
    expectRectDiff(result, [
      [-13, -13],
      [12, 10],
    ]);
  });

  test("reversed order", async () => {
    let result = rectangleDifference(testBBox2, testBBox1, scalar(0));
    expectRectDiff(result, [
      [-2, 0],
      [3, 3],
    ]);
  });

  test("same bounding box", async () => {
    let result = rectangleDifference(testBBox1, testBBox1, scalar(0));
    expectRectDiff(result, [
      [-2, -2],
      [2, 2],
    ]);
  });
});

describe("halfPlaneSDF", () => {
  let point1 = [scalar(2), scalar(3)];
  let point2 = [scalar(1), scalar(2)];
  let point3 = [scalar(1), scalar(4)];
  let point4 = [scalar(2), scalar(2)];
  let point5 = [scalar(0), scalar(0)];

  test("without padding", async () => {
    let result = halfPlaneSDF(
      [point2, point3],
      [point2, point4],
      point5,
      scalar(0),
    );
    expect(numsOf([result])[0]).toBeCloseTo(-3, 4);
  });

  test("with padding", async () => {
    let result = halfPlaneSDF(
      [point2, point3],
      [point2, point4],
      point5,
      scalar(10),
    );
    expect(numsOf([result])[0]).toBeCloseTo(7, 4);
  });

  test("zero outside", async () => {
    let result = halfPlaneSDF([point2, point3], [point5], point1, scalar(0));
    expect(numsOf([result])[0]).toBeCloseTo(1, 4);
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
    [scalar(-316.39758), scalar(-117.40885)],
    [scalar(-291.04166), scalar(-57.877602)],
    [scalar(-243.63715), scalar(-24.804688)],
    [scalar(-189.61806), scalar(-74.414062)],
    [scalar(-155.4427), scalar(9.921875)],
    [scalar(-173.63281), scalar(78.823784)],
    [scalar(-202.29601), scalar(136.15016)],
    [scalar(-257.41754), scalar(167.01823)],
    [scalar(-301.51477), scalar(164.81337)],
    [scalar(-359.94357), scalar(147.72569)],
    [scalar(-54.570312), scalar(355.53384)],
    [scalar(288.28558), scalar(232.61285)],
    [scalar(241.43228), scalar(148.82812)],
    [scalar(151.5842), scalar(66.145836)],
    [scalar(186.31076), scalar(8.8194437)],
    [scalar(269.54428), scalar(-59.53125)],
    [scalar(219.38368), scalar(-151.03297)],
    [scalar(14.882812), scalar(-128.98438)],
    [scalar(-224.34462), scalar(-165.36458)],
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
    const p = hm83.map(([x, y]) => [x, scalar(-y.arraySync())]);
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
      [scalar(-41.942347636454556), scalar(-334.57387855868393)],
      [scalar(-28.8732769036464), scalar(-514.3771026404029)],
      [scalar(69.71110339328084), scalar(-340.36239072939577)],
      [scalar(-104.30360851772633), scalar(-241.77801043246853)],
      [scalar(-91.23453778491817), scalar(-421.58123451418754)],
      [scalar(-66.58844271068637), scalar(-378.07755653643574)],
    ];
    expect(convexPartitions(p)).toEqual([
      [p[3], p[4], p[5], p[0]],
      [p[0], p[1], p[2]],
      [p[0], p[2], p[3]],
    ]);
  });
});

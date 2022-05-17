import { compDict } from "contrib/Functions";
import { genCode, secondaryGraph } from "engine/Autodiff";
import seedrandom from "seedrandom";
import { makeCircle } from "shapes/Circle";
import { makePolygon } from "shapes/Polygon";
import { makeRectangle } from "shapes/Rectangle";
import {
  FloatV,
  makeCanvas,
  PtListV,
  sampleBlack,
  VectorV,
} from "shapes/Samplers";
import * as ad from "types/ad";
import { IShape } from "types/shapes";

const canvas = makeCanvas(800, 700);

const compareDistance = (
  shapeType: string,
  shape: IShape,
  p: ad.Num[],
  expected: number
) => {
  const result = compDict.distanceShapeToPoint(
    { rng: seedrandom("shape") },
    [shapeType, shape],
    p
  );
  const g = secondaryGraph([result.contents]);
  const f = genCode(g);
  const [dist] = f([]).secondary; // no inputs, so, empty array
  expect(dist).toBeCloseTo(expected);
};

const testRectangle = (
  center: number[],
  width: number,
  height: number,
  strokeWidth: number,
  pt: number[],
  expected: number
) => {
  const seed = seedrandom("bbox Rectangle");
  const shape = makeRectangle(seed, canvas, {
    center: VectorV(center),
    width: FloatV(width),
    height: FloatV(height),
    strokeWidth: FloatV(strokeWidth),
    strokeColor: sampleBlack(),
  });
  compareDistance("Rectangle", shape, pt, expected);
};

const testCircle = (
  center: number[],
  radius: number,
  strokeWidth: number,
  pt: number[],
  expected: number
) => {
  const seed = seedrandom("bbox Rectangle");
  const shape = makeCircle(seed, canvas, {
    center: VectorV(center),
    r: FloatV(radius),
    strokeWidth: FloatV(strokeWidth),
    strokeColor: sampleBlack(),
  });
  compareDistance("Circle", shape, pt, expected);
};

const testPolygon = (
  points: number[][],
  strokeWidth: number,
  pt: number[],
  expected: number
) => {
  const seed = seedrandom("Polygon");
  const shape = makePolygon(seed, canvas, {
    strokeWidth: FloatV(strokeWidth),
    strokeColor: sampleBlack(),
    points: PtListV(points),
  });
  compareDistance("Polygon", shape, pt, expected);
};

describe("sdf", () => {
  test("CenteredRectange", () => {
    testRectangle([0, 0], 8, 4, 0, [5, 0], 1);
    testRectangle([0, 0], 8, 4, 0, [0, 3], 1);
    testRectangle([0, 0], 8, 4, 0, [0, 1], -1);
    testRectangle([0, 0], 8, 4, 0, [3, 0], -1);
    testRectangle([0, 0], 8, 4, 0, [-3, 0], -1);
    testRectangle([0, 0], 8, 4, 0, [7, 6], 5);
    testRectangle([0, 0], 8, 4, 0, [7, -6], 5);
    testRectangle([0, 0], 8, 4, 0, [-7, -6], 5);
    testRectangle([0, 0], 8, 4, 0, [-7, 6], 5);
    testRectangle([0, 0], 8, 4, 0, [-6, 0], 2);
    testRectangle([0, 0], 8, 4, 0, [0, 6], 4);
    testRectangle([0, 0], 8, 4, 0, [0, -6], 4);
    testRectangle([0, 0], 8, 4, 0, [0, 0], -2);
    testRectangle([0, 0], 8, 4, 0, [4, 2], 0);
    testRectangle([0, 0], 8, 4, 0, [0, 2], 0);
  });

  test("OffCenterSquare", () => {
    testRectangle([-2, -2], 4, 4, 0, [0, 0], 0);
    testRectangle([-2, -2], 4, 4, 0, [-2, -2], -2);
    testRectangle([-2, -2], 4, 4, 0, [-1, -2], -1);
  });

  test("Circle", () => {
    testCircle([0, 0], 3, 0, [0, 0], -3);
    testCircle([0, 0], 3, 0, [3, 0], 0);
    testCircle([0, 0], 3, 0, [4, 0], 1);
    testCircle([0, 0], 3, 0, [-5, 0], 2);
  });

  test("OffsetCircle", () => {
    testCircle([3, 3], 3, 0, [3, 3], -3);
    testCircle([3, 3], 3, 0, [3, 6], 0);
    testCircle([3, 3], 3, 0, [3, 0], 0);
  });

  test("rectangleAsPolygon", () => {
    testPolygon(
      [
        [4, 2],
        [4, -2],
        [-4, -2],
        [-4, 2],
      ],
      0,
      [5, 0],
      1
    );
    testPolygon(
      [
        [4, 2],
        [4, -2],
        [-4, -2],
        [-4, 2],
      ],
      0,
      [0, 2],
      0
    );
  });
  testPolygon(
    [
      [-4, -4],
      [-4, 0],
      [0, 0],
      [0, -4],
    ],
    0,
    [-2, -2],
    -2
  );
});

test("convexHeptagon", () => {
  testPolygon(
    [
      [4, 8],
      [8, 8],
      [8, 0],
      [0, 0],
      [0, 4],
      [4, 4],
    ],
    0,
    [3, 6],
    1
  );
});

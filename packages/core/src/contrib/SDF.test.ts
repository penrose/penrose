import { genCode, secondaryGraph } from "engine/Autodiff";
import seedrandom from "seedrandom";
import { makeCircle } from "shapes/Circle";
import { makeEllipse } from "shapes/Ellipse";
import { makeLine } from "shapes/Line";
import { makePolygon } from "shapes/Polygon";
import { makeRectangle } from "shapes/Rectangle";
import {
  floatV,
  makeCanvas,
  ptListV,
  sampleBlack,
  vectorV,
} from "shapes/Samplers";
import * as ad from "types/ad";
import { Shape } from "types/shapes";
import { compDict } from "./Functions";

const canvas = makeCanvas(800, 700);

const compareDistance = (
  shapeType: string,
  shape: Shape,
  p: ad.Num[],
  expected: number
) => {
  const result = compDict.signedDistance(
    { rng: seedrandom("shape") },
    [shapeType, shape],
    p
  );
  const g = secondaryGraph([result.contents]);
  const f = genCode(g);
  /* const [dist] = 
  const {
    secondary: [dist],
    stmts,
  } = f([]); // no inputs, so, empty array
  const code = stmts.join("\n");
  console.log(code); */
  const [dist] = f([]).secondary;
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
    center: vectorV(center),
    width: floatV(width),
    height: floatV(height),
    strokeWidth: floatV(strokeWidth),
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
    center: vectorV(center),
    r: floatV(radius),
    strokeWidth: floatV(strokeWidth),
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
    strokeWidth: floatV(strokeWidth),
    strokeColor: sampleBlack(),
    points: ptListV(points),
  });
  compareDistance("Polygon", shape, pt, expected);
};

function testLine(
  start: number[],
  end: number[],
  strokeWidth: number,
  pt: number[],
  expected: number
) {
  const seed = seedrandom("Polygon");
  const shape = makeLine(seed, canvas, {
    strokeWidth: floatV(strokeWidth),
    strokeColor: sampleBlack(),
    start: vectorV(start),
    end: vectorV(end),
  });
  compareDistance("Line", shape, pt, expected);
}

function testEllipse(
  center: number[],
  rx: number,
  ry: number,
  strokeWidth: number,
  pt: number[],
  expected: number
) {
  const seed = seedrandom("bbox Rectangle");
  const shape = makeEllipse(seed, canvas, {
    center: vectorV(center),
    rx: floatV(rx),
    ry: floatV(ry),
    strokeWidth: floatV(strokeWidth),
    strokeColor: sampleBlack(),
  });
  compareDistance("Ellipse", shape, pt, expected);
}

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

test("line", () => {
  testLine([0, 0], [8, 0], 0, [4, 0], 0);
  testLine([0, 0], [8, 0], 0, [0, 4], 4);
  testLine([0, 0], [8, 8], 0, [0, 4], Math.cos(Math.PI / 4) * 4);
});

/* test("ellipse", () => {
  // testCircle([0, 0], 3, 0, [0, 0], -3);
  // an ellipse is defined by the following parametric equations
  // x = r_x * cos(theta)
  // y = r_x * sin(theta)
  testEllipse([0, 0], 10, 5, 0, [11, 1], 1.2);
}); */

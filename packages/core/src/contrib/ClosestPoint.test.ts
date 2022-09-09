import { genCode, input, secondaryGraph } from "engine/Autodiff";
import seedrandom from "seedrandom";
import { makeCircle } from "shapes/Circle";
import { makeLine } from "shapes/Line";
import { makeRectangle } from "shapes/Rectangle";
import {
  Context,
  InputFactory,
  makeCanvas,
  simpleContext,
} from "shapes/Samplers";
import * as ad from "types/ad";
import { black, floatV, vectorV } from "utils/Util";
import { compDict } from "./Functions";

const canvas = makeCanvas(800, 700);

const makeInputArray = (pt: number[]): ad.Input[] => {
  const inputs: ad.Input[] = [];
  const makeInput: InputFactory = (meta) => {
    const rng = seedrandom("sdf");
    const x = input({
      key: inputs.length,
      val: "pending" in meta ? meta.pending : meta.sampler(rng),
    });
    inputs.push(x);
    return x;
  };
  for (const coord of pt) {
    makeInput({ sampler: () => coord });
  }
  return [...inputs];
};

const compareClosestPoint = (
  context: Context,
  shapeType: string,
  shape: any,
  pt: [ad.Input, ad.Input],
  expected: [number, number]
) => {
  const result = compDict.closestPoint(context, [shapeType, shape], pt);
  const g = secondaryGraph(result.contents);
  const f = genCode(g);
  let [x, y] = f([]).secondary;
  if (shapeType === "Circle") {
    [x, y] = f([
      pt[0].val,
      pt[1].val,
      shape.center.contents[0].val,
      shape.center.contents[1].val,
      shape.r.contents.val,
    ]).secondary;
  } else if (
    shapeType === "Rectangle" ||
    shapeType === "Text" ||
    shapeType === "Equation" ||
    shapeType === "Image"
  ) {
    [x, y] = f([
      pt[0].val,
      pt[1].val,
      shape.center.contents[0].val,
      shape.center.contents[1].val,
      shape.width.contents.val,
      shape.height.contents.val,
    ]).secondary;
  } else if (shapeType === "Line") {
    [x, y] = f([
      pt[0].val,
      pt[1].val,
      shape.start.contents[0].val,
      shape.start.contents[1].val,
      shape.end.contents[0].val,
      shape.end.contents[1].val,
    ]).secondary;
  }
  expect(x).toBeCloseTo(expected[0]);
  expect(y).toBeCloseTo(expected[1]);
};

export const testCircle = (
  center: number[],
  radius: number,
  strokeWidth: number,
  pt: [number, number],
  expected: [number, number]
) => {
  const p = makeInputArray([pt[0], pt[1], center[0], center[1], radius]);
  const context = simpleContext("circle");
  const shape = makeCircle(context, canvas, {
    center: vectorV([p[2], p[3]]),
    r: floatV(p[4]),
    strokeWidth: floatV(strokeWidth),
    strokeColor: black(),
  });
  compareClosestPoint(context, "Circle", shape, [p[0], p[1]], expected);
};

export const testRectangle = (
  center: number[],
  width: number,
  height: number,
  strokeWidth: number,
  pt: [number, number],
  expected: [number, number]
) => {
  const context = simpleContext("rectangle");
  const p = makeInputArray([pt[0], pt[1], center[0], center[1], width, height]);
  const shape = makeRectangle(context, canvas, {
    center: vectorV([p[2], p[3]]),
    width: floatV(p[4]),
    height: floatV(p[5]),
    strokeWidth: floatV(strokeWidth),
    strokeColor: black(),
  });
  compareClosestPoint(context, "Rectangle", shape, [p[0], p[1]], expected);
};

export const testLine = (
  start: number[],
  end: number[],
  strokeWidth: number,
  pt: [number, number],
  expected: [number, number]
) => {
  const context = simpleContext("Line");
  const p = makeInputArray([pt[0], pt[1], start[0], start[1], end[0], end[1]]);
  const shape = makeLine(context, canvas, {
    start: vectorV([p[2], p[3]]),
    end: vectorV([p[4], p[5]]),
    strokeWidth: floatV(strokeWidth),
    strokeColor: black(),
  });
  compareClosestPoint(context, "Line", shape, [p[0], p[1]], expected);
};

// export const testPolyline = (
//   points: number[][],
//   strokeWidth: number,
//   pt: [number, number],
//   expected: [number, number]
// ) => {
//   const context = simpleContext("Polyline");
//   const shape = makePolyline(context, canvas, {
//     strokeWidth: floatV(strokeWidth),
//     points: ptListV(points),
//     strokeColor: black(),
//   });
//   compareClosestPoint(context, "Polyline", shape, pt, expected);
// };

// const testPolygon = (
//   points: number[][],
//   strokeWidth: number,
//   pt: [number, number],
//   expected: [number, number]
// ) => {
//   const context = simpleContext("Polygon");
//   const shape = makePolygon(context, canvas, {
//     strokeWidth: floatV(strokeWidth),
//     strokeColor: black(),
//     points: ptListV(points),
//   });
//   compareClosestPoint(context, "Polygon", shape, pt, expected);
// };

// function testEllipse(
//   center: number[],
//   rx: number,
//   ry: number,
//   pt: [number, number],
//   expected: [number, number]
// ) {
//   const context = simpleContext("Ellipse");
//   const shape = makeEllipse(context, canvas, {
//     center: vectorV(center),
//     rx: floatV(rx),
//     ry: floatV(ry),
//     strokeColor: black(),
//   });
//   compareClosestPoint(context, "Ellipse", shape, pt, expected);
// }

describe("closest point", () => {
  test("circle", () => {
    testCircle([0, 0], 3, 0, [3, 0], [3, 0]);
    testCircle([0, 0], 3, 0, [4, 0], [3, 0]);
    testCircle([0, 0], 3, 0, [-5, 0], [-3, 0]);
  });

  test("rectangle", () => {
    testRectangle([0, 0], 4, 4, 0, [2, 2], [2, 2]);
    testRectangle([0, 0], 6, 4, 0, [-6, 0], [-3, 0]);
    testRectangle([2, 3], 4, 6, 0, [-6, 0], [0, 0]);
    testRectangle([0, 0], 6, 4, 0, [-6, 2], [-3, 2]);
    testRectangle([0, 0], 6, 4, 0, [6, -3], [3, -2]);
    testRectangle([0, 0], 6, 4, 0, [-6, -5], [-3, -2]);
    testRectangle([0, 0], 6, 4, 0, [1, 1], [1, 2]);
    testRectangle([0, 0], 6, 4, 0, [0, 0], [0, 2]); //when input point is at center of rectangle, a point still gets returned.
  });

  test("Line", () => {
    testLine([0, 0], [7, 0], 4, [2, 2], [2, 0]);
    testLine([0, 0], [7, 0], 4, [8, 1], [7, 0]);
    testLine([0, 0], [7, 0], 4, [-1, 1], [0, 0]);
    testLine([0, 0], [6, 3], 4, [1, 3], [2, 1]);
    testLine([7, 0], [0, 0], 4, [2, 2], [2, 0]);
    testLine([7, 0], [0, 0], 4, [2, 2], [2, 0]);
    testLine([7, 0], [0, 0], 4, [8, 1], [7, 0]);
    testLine([7, 0], [0, 0], 4, [-1, 1], [0, 0]);
    testLine([6, 3], [0, 0], 4, [1, 3], [2, 1]);
    testLine([0, 0], [3, 2], 4, [1, 1], [1.153846, 0.7692307692307693]);
    testLine([0, 0], [3, 2], 4, [3, 3], [3, 2]);
    testLine([3, 2], [5, 0], 4, [3, 3], [3, 2]);
  });
  // test("Polyline", () => {
  //   testPolyline(
  //     [
  //       [0, 0],
  //       [3, 2],
  //       [5, 0],
  //       [5, -3],
  //     ],
  //     4,
  //     [3, 3],
  //     [3, 2]
  //   );
  //   testPolyline(
  //     [
  //       [0, 0],
  //       [3, 2],
  //       [5, 0],
  //       [5, -3],
  //     ],
  //     4,
  //     [5, 2],
  //     [4, 1]
  //   );
  // });
  // test("rectangle as polygon", () => {
  //   testPolygon(
  //     [
  //       [4, 2],
  //       [4, -2],
  //       [-4, -2],
  //       [-4, 2],
  //     ],
  //     0,
  //     [5, 0],
  //     [4, 0]
  //   );
  //   testPolygon(
  //     [
  //       [4, 2],
  //       [4, -2],
  //       [-4, -2],
  //       [-4, 2],
  //     ],
  //     0,
  //     [0, 2],
  //     [0, 2]
  //   );
  // });
  // testPolygon(
  //   [
  //     [-4, -4],
  //     [-4, 0],
  //     [0, 0],
  //     [0, -4],
  //   ],
  //   0,
  //   [-2, -2],
  //   [-2, -4]
  // );
  // test("convex heptagon", () => {
  //   testPolygon(
  //     [
  //       [4, 8],
  //       [8, 8],
  //       [8, 0],
  //       [0, 0],
  //       [0, 4],
  //       [4, 4],
  //     ],
  //     0,
  //     [3, 6],
  //     [4, 6]
  //   );
  // });
  test("ellipse", () => {
    // testEllipse([0, 0], 50, 100, [50, 0], [50, 0]);
    // testEllipse([0, 0], 100, 50, [0, 100], [0, 50]);
    // testEllipse([0, 0], 100, 50, [0, -100], [0, -50]);
    // testEllipse([0, 0], 100, 50, [0, 60], [0, 50.0]);
    // testEllipse([0, 0], 100, 50, [0, 0], [6.123233995736766e-15, 50.0]);
    // testEllipse([0, 0], 100, 50, [0, 10], [6.123233995736766e-15, 50.0]);
    // testEllipse([0, 0], 100, 50, [200, 0], [100.0, 0.0]);
    // testEllipse([0, 0], 100, 50, [0, 110], [6.123233995736766e-15, 50.0]);
    // testEllipse(
    //   [0, 0],
    //   100,
    //   50,
    //   [200, 200],
    //   [81.19841273323402, 29.183975781254418]
    // );
    // testEllipse(
    //   [0, 0],
    //   100,
    //   50,
    //   [100, 100],
    //   [69.28204652936475, 36.05550592039633]
    // );
    // testEllipse(
    //   [0, 0],
    //   50,
    //   100,
    //   [10, 10],
    //   [49.61089494163424, 12.493833223000912]
    // );
    // testEllipse([0, 0], 100, 50, [100, 0], [100, 0]);
    // testEllipse([0, 0], 100, 50, [0, 0], [0, 50.0]);
    // testEllipse([0, 0], 100, 50, [-100, 0], [-100, 0]); //[-100, 0] -> [0, 100]
    // testEllipse([0, 0], 100, 50, [0, -50], [6.123233995736766e-15, -50.0]); //[0, -50] -> [0, 50]
    // testEllipse([0, 0], 100, 50, [100, 0], [100, 0]); //[100, 0]->[0,100]
    // testEllipse([0, 0], 100, 50, [0, 50], [0, 50.0]); //fine
    // testEllipse(
    //   [0, 0],
    //   3,
    //   3,
    //   [2.121320343559643, 2.121320343559643],
    //   [2.121320343559643, 2.121320343559643]
    // );
    // //
    // testEllipse(
    //   [0, 0],
    //   50,
    //   100,
    //   [-10, -15],
    //   [-49.11524150621692, -18.728913666946834]
    // );
    // testEllipse(
    //   [0, 0],
    //   50,
    //   100,
    //   [-10, -15],
    //   [-49.11524150621692, -18.728913666946834]
    // );
    // testEllipse(
    //   [0, 0],
    //   50,
    //   100,
    //   [20, -30],
    //   [46.83455310025607, -35.015689963174765]
    // );
    // testEllipse(
    //   [0, 0],
    //   50,
    //   100,
    //   [10, -30],
    //   [46.38787139061665, -37.3183808784684]
    // );
    // testEllipse(
    //   [0, 0],
    //   50,
    //   100,
    //   [35, -30],
    //   [47.35509305915151, -32.093311537396765]
    // );
    // testEllipse(
    //   [0, 0],
    //   50,
    //   100,
    //   [-60, 10],
    //   [-49.77368387088071, 9.511432996109345]
    // );
    // testEllipse(
    //   [0, 0],
    //   50,
    //   100,
    //   [-40, 10],
    //   [-49.72375690607735, 10.513979153705371]
    // );
    // testEllipse(
    //   [0, 0],
    //   50,
    //   100,
    //   [80, -30],
    //   [48.311109867232666, -25.771042927764825]
    // );
    // testEllipse([0, 0], 50, 100, [0, -100], [0, -100]);
    // testEllipse(
    //   [10, 0],
    //   50,
    //   100,
    //   [0, -90],
    //   [13.524560987414274, -96.27224418486799]
    // );
    // testEllipse(
    //   [10, 100],
    //   50,
    //   100,
    //   [0, -90],
    //   [2.170529302886986, -99.9057316182672]
    // );
    // testEllipse([50, 50], 100, 50, [-50, 50], [-50, 50]);
    // testEllipse([50, 50], 100, 50, [150, 50], [150, 50]);
    // testEllipse([-100, 200], 100, 50, [-50, 50], [-67.59, 152.6987]);
  });
});

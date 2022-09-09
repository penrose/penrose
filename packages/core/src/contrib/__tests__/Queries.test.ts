import { compDict } from "contrib/Functions";
import {
  bboxFromShape,
  outwardUnitNormal,
  polygonLikePoints,
  shapeCenter,
  shapeSize,
} from "contrib/Queries";
import { genCode, ops, secondaryGraph } from "engine/Autodiff";
import { sub } from "engine/AutodiffFunctions";
import { makeCircle } from "shapes/Circle";
import { makeEllipse } from "shapes/Ellipse";
import { makeLine } from "shapes/Line";
import { makePath } from "shapes/Path";
import { makePolygon } from "shapes/Polygon";
import { makeRectangle } from "shapes/Rectangle";
import { makeCanvas, simpleContext } from "shapes/Samplers";
import { Pt2 } from "types/ad";
import { black, floatV, ptListV, vectorV } from "utils/Util";

const context = simpleContext("Queries");
const canvas = makeCanvas(800, 700);
const precisionDigits = 10;

const shapes: [string, any][] = [
  // shapes[0]
  [
    "Rectangle",
    makeRectangle(context, canvas, {
      center: vectorV([11, 22]),
      width: floatV(44),
      height: floatV(44),
      strokeWidth: floatV(0),
      strokeColor: black(),
    }),
  ],
  // shapes[1]
  [
    "Circle",
    makeCircle(context, canvas, {
      r: floatV(22),
      center: vectorV([11, 22]),
      strokeWidth: floatV(0),
      strokeColor: black(),
    }),
  ],
  // shapes[2]
  [
    "Ellipse",
    makeEllipse(context, canvas, {
      rx: floatV(22),
      ry: floatV(22),
      center: vectorV([11, 22]),
      strokeWidth: floatV(0),
      strokeColor: black(),
    }),
  ],
  // shapes[3]
  [
    "Path",
    makePath(context, canvas, {
      d: compDict.pathFromPoints(context, "open", [
        [-11, 0],
        [33, 0],
        [33, 44],
      ]),
    }),
  ],
  // shapes[4]
  [
    "Line",
    makeLine(context, canvas, {
      start: vectorV([-11, 0]),
      end: vectorV([33, 44]),
      strokeWidth: floatV(0),
    }),
  ],
  // shapes[5]
  [
    "Polygon",
    makePolygon(context, canvas, {
      points: ptListV([
        [-11, 0],
        [33, 0],
        [33, 44],
      ]),
      scale: floatV(1),
    }),
  ],
];

describe("simple queries", () => {
  it.each(shapes)("bboxFromShape for %p", (shapeType: string, shape: any) => {
    const bbox = bboxFromShape([shapeType, shape]);
    const [x, y, w, h] = genCode(
      secondaryGraph([bbox.center[0], bbox.center[1], bbox.width, bbox.height])
    )([]).secondary;
    expect(x).toBeCloseTo(11, precisionDigits);
    expect(y).toBeCloseTo(22, precisionDigits);
    expect(w).toBeCloseTo(44, precisionDigits);
    expect(h).toBeCloseTo(44, precisionDigits);
  });

  it.each(shapes)("shapeCenter for %p", (shapeType: string, shape: any) => {
    const center = shapeCenter([shapeType, shape]);
    const [x, y] = genCode(secondaryGraph([center[0], center[1]]))(
      []
    ).secondary;
    expect(x).toBeCloseTo(11, precisionDigits);
    expect(y).toBeCloseTo(22, precisionDigits);
  });

  it.each(shapes)("shapeSize for %p", (shapeType: string, shape: any) => {
    const size = shapeSize([shapeType, shape]);
    const [sizeNum] = genCode(secondaryGraph([size]))([]).secondary;
    expect(sizeNum).toBeCloseTo(44, precisionDigits);
  });
});

describe("polygonLikePoints", () => {
  const ptsToNums = (result: Pt2[]): [number, number][] => {
    const outputs = [];
    for (const pt of result) {
      outputs.push(...pt);
    }
    const g = secondaryGraph(outputs);
    const f = genCode(g);
    const nums = f([]).secondary; // no inputs, so, empty array
    const pts: [number, number][] = [];
    for (let i = 0; i < nums.length; i += 2) {
      pts.push([nums[i], nums[i + 1]]);
    }
    return pts;
  };

  test("Rectangle shape", async () => {
    const result = ptsToNums(polygonLikePoints(shapes[0]));
    expect(result.length).toEqual(4);
    expect(result[0]).toEqual([33, 44]);
    expect(result[1]).toEqual([-11, 44]);
    expect(result[2]).toEqual([-11, 0]);
    expect(result[3]).toEqual([33, 0]);
  });

  test("Line shape", async () => {
    const result = ptsToNums(polygonLikePoints(shapes[4]));
    expect(result.length).toEqual(2);
    expect(result[0]).toEqual([-11, 0]);
    expect(result[1]).toEqual([33, 44]);
  });

  test("Polygon shape", async () => {
    const result = ptsToNums(polygonLikePoints(shapes[5]));
    expect(result.length).toEqual(3);
    expect(result[0]).toEqual([-11, 0]);
    expect(result[1]).toEqual([33, 0]);
    expect(result[2]).toEqual([33, 44]);
  });

  it.each([shapes[1], shapes[2], shapes[3]])(
    "unsupported shape %p",
    (shapeType: string, shape: any) => {
      expect(() => polygonLikePoints([shapeType, shape])).toThrowError();
    }
  );
});

describe("outwardUnitNormal", () => {
  let point1 = [2, 3];
  let point2 = [1, 2];
  let point3 = [1, 4];
  let point4 = [2, 2];
  let lineSegment = [point3, point4];

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
    expect(norm).toBeCloseTo(1, 4);
    // It is orthogonal to the line segment
    expect(dot).toBeCloseTo(0, 4);
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
    expect(norm).toBeCloseTo(1, 4);
    // It is orthogonal to the line segment
    expect(dot).toBeCloseTo(0, 4);
    // `insidePoint2` is inside
    expect(diff).toBeLessThan(0);
  });
});

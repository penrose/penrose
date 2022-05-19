import { compDict } from "contrib/Functions";
import {
  bboxFromShape,
  polygonLikePoints,
  shapeCenter,
  shapeSize,
} from "contrib/Queries";
import { genCode, secondaryGraph } from "engine/Autodiff";
import seedrandom from "seedrandom";
import { makeCircle } from "shapes/Circle";
import { makeEllipse } from "shapes/Ellipse";
import { makeLine } from "shapes/Line";
import { makePath } from "shapes/Path";
import { makePolygon } from "shapes/Polygon";
import { makeRectangle } from "shapes/Rectangle";
import { makeCanvas, sampleBlack } from "shapes/Samplers";
import { Pt2 } from "types/ad";
import { floatV, ptListV, vectorV } from "utils/Util";

const rng = seedrandom("Queries");
const canvas = makeCanvas(800, 700);
const precisionDigits = 10;

const shapes: [string, any][] = [
  // shapes[0]
  [
    "Rectangle",
    makeRectangle(rng, canvas, {
      center: vectorV([11, 22]),
      width: floatV(44),
      height: floatV(44),
      strokeWidth: floatV(0),
      strokeColor: sampleBlack(),
    }),
  ],
  // shapes[1]
  [
    "Circle",
    makeCircle(rng, canvas, {
      r: floatV(22),
      center: vectorV([11, 22]),
      strokeWidth: floatV(0),
      strokeColor: sampleBlack(),
    }),
  ],
  // shapes[2]
  [
    "Ellipse",
    makeEllipse(rng, canvas, {
      rx: floatV(22),
      ry: floatV(22),
      center: vectorV([11, 22]),
      strokeWidth: floatV(0),
      strokeColor: sampleBlack(),
    }),
  ],
  // shapes[3]
  [
    "Path",
    makePath(rng, canvas, {
      d: compDict.pathFromPoints({ rng }, "open", [
        [-11, 0],
        [33, 0],
        [33, 44],
      ]),
    }),
  ],
  // shapes[4]
  [
    "Line",
    makeLine(rng, canvas, {
      start: vectorV([-11, 0]),
      end: vectorV([33, 44]),
      strokeWidth: floatV(0),
    }),
  ],
  // shapes[5]
  [
    "Polygon",
    makePolygon(rng, canvas, {
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

import { compDict } from "contrib/Functions";
import {
  bboxFromShape,
  polygonLikePoints,
  shapeCenter,
  shapeSize,
} from "contrib/Queries";
import { constOf, numOf } from "engine/Autodiff";
import seedrandom from "seedrandom";
import { makeCircle } from "shapes/Circle";
import { makeEllipse } from "shapes/Ellipse";
import { makeLine } from "shapes/Line";
import { makePath } from "shapes/Path";
import { makePolygon } from "shapes/Polygon";
import { makeRectangle } from "shapes/Rectangle";
import {
  FloatV,
  makeCanvas,
  PtListV,
  sampleBlack,
  VectorV,
} from "shapes/Samplers";

const rng = seedrandom("Queries");
const canvas = makeCanvas(800, 700);
const precisionDigits = 10;

const shapes: [string, any][] = [
  // shapes[0]
  [
    "Rectangle",
    makeRectangle(rng, canvas, {
      center: VectorV([11, 22].map(constOf)),
      width: FloatV(constOf(44)),
      height: FloatV(constOf(44)),
      strokeWidth: FloatV(constOf(0)),
      strokeColor: sampleBlack(),
    }),
  ],
  // shapes[1]
  [
    "Circle",
    makeCircle(rng, canvas, {
      r: FloatV(constOf(22)),
      center: VectorV([11, 22].map(constOf)),
      strokeWidth: FloatV(constOf(0)),
      strokeColor: sampleBlack(),
    }),
  ],
  // shapes[2]
  [
    "Ellipse",
    makeEllipse(rng, canvas, {
      rx: FloatV(constOf(22)),
      ry: FloatV(constOf(22)),
      center: VectorV([11, 22].map(constOf)),
      strokeWidth: FloatV(constOf(0)),
      strokeColor: sampleBlack(),
    }),
  ],
  // shapes[3]
  [
    "Path",
    makePath(rng, canvas, {
      d: compDict.pathFromPoints({ rng }, "open", [
        [constOf(-11), constOf(0)],
        [constOf(33), constOf(0)],
        [constOf(33), constOf(44)],
      ]),
    }),
  ],
  // shapes[4]
  [
    "Line",
    makeLine(rng, canvas, {
      start: VectorV([-11, 0].map(constOf)),
      end: VectorV([33, 44].map(constOf)),
      strokeWidth: FloatV(constOf(0)),
    }),
  ],
  // shapes[5]
  [
    "Polygon",
    makePolygon(rng, canvas, {
      points: PtListV(
        [
          [-11, 0],
          [33, 0],
          [33, 44],
        ].map((p) => p.map(constOf))
      ),
      scale: FloatV(constOf(1)),
    }),
  ],
];

describe("simple queries", () => {
  it.each(shapes)("bboxFromShape for %p", (shapeType: string, shape: any) => {
    const bbox = bboxFromShape([shapeType, shape]);
    expect(numOf(bbox.center[0])).toBeCloseTo(11, precisionDigits);
    expect(numOf(bbox.center[1])).toBeCloseTo(22, precisionDigits);
    expect(numOf(bbox.width)).toBeCloseTo(44, precisionDigits);
    expect(numOf(bbox.height)).toBeCloseTo(44, precisionDigits);
  });

  it.each(shapes)("shapeCenter for %p", (shapeType: string, shape: any) => {
    const center = shapeCenter([shapeType, shape]);
    expect(numOf(center[0])).toBeCloseTo(11, precisionDigits);
    expect(numOf(center[1])).toBeCloseTo(22, precisionDigits);
  });

  it.each(shapes)("shapeSize for %p", (shapeType: string, shape: any) => {
    const size = shapeSize([shapeType, shape]);
    expect(numOf(size)).toBeCloseTo(44, precisionDigits);
  });
});

describe("polygonLikePoints", () => {
  test("Rectangle shape", async () => {
    const result = polygonLikePoints(shapes[0]);
    expect(result.length).toEqual(4);
    expect(result[0].map(numOf)).toEqual([33, 44]);
    expect(result[1].map(numOf)).toEqual([-11, 44]);
    expect(result[2].map(numOf)).toEqual([-11, 0]);
    expect(result[3].map(numOf)).toEqual([33, 0]);
  });

  test("Line shape", async () => {
    const result = polygonLikePoints(shapes[4]);
    expect(result.length).toEqual(2);
    expect(result[0].map(numOf)).toEqual([-11, 0]);
    expect(result[1].map(numOf)).toEqual([33, 44]);
  });

  test("Polygon shape", async () => {
    const result = polygonLikePoints(shapes[5]);
    expect(result.length).toEqual(3);
    expect(result[0].map(numOf)).toEqual([-11, 0]);
    expect(result[1].map(numOf)).toEqual([33, 0]);
    expect(result[2].map(numOf)).toEqual([33, 44]);
  });

  it.each([shapes[1], shapes[2], shapes[3]])(
    "unsupported shape %p",
    (shapeType: string, shape: any) => {
      expect(() => polygonLikePoints([shapeType, shape])).toThrowError();
    }
  );
});

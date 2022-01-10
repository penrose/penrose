import { bboxFromShape, shapeCenter, shapeSize } from "contrib/Queries";
import { FloatV, makeCanvas, sampleBlack, VectorV } from "shapes/Samplers";
import { compDict } from "contrib/Functions";
import { constOf, numOf } from "engine/Autodiff";
import { makeRectangle } from "shapes/Rectangle";
import { makeCircle } from "shapes/Circle";
import { makeEllipse } from "shapes/Ellipse";
import { makePath } from "shapes/Path";

const canvas = makeCanvas(800, 700);
const precisionDigits = 10;

const shapes: [string, any][] = [
  [
    "Rectangle", 
    makeRectangle(canvas, {
      center: VectorV([11, 22].map(constOf)),
      width: FloatV(constOf(44)),
      height: FloatV(constOf(44)),
      strokeWidth: FloatV(constOf(0)),
      strokeColor: sampleBlack(),
    })
  ],
  [
    "Circle",
    makeCircle(canvas, {
      r: FloatV(constOf(22)),
      center: VectorV([11, 22].map(constOf)),
      strokeWidth: FloatV(constOf(0)),
      strokeColor: sampleBlack(),
    })
  ],
  [
    "Ellipse",
    makeEllipse(canvas, {
      rx: FloatV(constOf(22)),
      ry: FloatV(constOf(22)),
      center: VectorV([11, 22].map(constOf)),
      strokeWidth: FloatV(constOf(0)),
      strokeColor: sampleBlack(),
    })
  ],
  [
    "Path",
    makePath(canvas, {
      d: compDict.pathFromPoints("open", [
        [constOf(-11), constOf(0)],
        [constOf(33), constOf(0)],
        [constOf(33), constOf(44)],
      ]),
    })
  ],
];

describe("simple queries", () => {

  it.each(shapes)('bboxFromShape for %p', (
    shapeType: string,
    shape: any,
  ) => {
    const bbox = bboxFromShape([shapeType, shape]);
    expect(numOf(bbox.center[0])).toBeCloseTo(11, precisionDigits);
    expect(numOf(bbox.center[1])).toBeCloseTo(22, precisionDigits);
    expect(numOf(bbox.width)).toBeCloseTo(44, precisionDigits);
    expect(numOf(bbox.height)).toBeCloseTo(44, precisionDigits);
  });

  it.each(shapes)('shapeCenter for %p', (
    shapeType: string,
    shape: any,
  ) => {
    const center = shapeCenter([shapeType, shape]);
    expect(numOf(center[0])).toBeCloseTo(11, precisionDigits);
    expect(numOf(center[1])).toBeCloseTo(22, precisionDigits);
  });

  it.each(shapes)('shapeSize for %p', (
    shapeType: string,
    shape: any,
  ) => {
    const size = shapeSize([shapeType, shape]);
    expect(numOf(size)).toBeCloseTo(44, precisionDigits);
  });

});

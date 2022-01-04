import { constOf, numOf } from "engine/Autodiff";
import {
  FloatV,
  makeCanvas,
  PtListV,
  sampleBlack,
  VectorV,
} from "shapes/Samplers";
import { makeCircle } from "shapes/Circle";
import { Ellipse, makeEllipse } from "shapes/Ellipse";
import { makeRectangle, Rectangle } from "shapes/Rectangle";
import { IPoly, IScale } from "shapes/Shapes";
import { makePolygon, Polygon } from "shapes/Polygon";
import { makePolyline, Polyline } from "shapes/Polyline";
import { Image, makeImage } from "shapes/Image";
import { Line, makeLine } from "shapes/Line";
import { makePath, Path } from "shapes/Path";
import { compDict } from "contrib/Functions";
import {
  bboxFromCircle,
  bboxFromEllipse,
  bboxFromLinelike,
  bboxFromPath,
  bboxFromPolygon,
  bboxFromRect,
  bboxFromRectlike,
} from "./BBox";

const canvas = makeCanvas(800, 700);

const polyProps = (): IPoly & IScale => ({
  points: PtListV(
    // https://en.wikipedia.org/wiki/Polygon#/media/File:Assorted_polygons.svg
    [
      [564, 24],
      [733, 54],
      [755, 154],
      [693, 257],
      [548, 216],
      [571, 145],
      [630, 146],
      [617, 180],
      [664, 196],
      [701, 120],
      [591, 90],
      [528, 129],
    ].map((p) => p.map(constOf))
  ),
  scale: FloatV(constOf(0.5)),
});

describe("bbox", () => {
  test("Circle", () => {
    const shape = makeCircle(canvas, {
      r: FloatV(constOf(100)),
      center: VectorV([42, 121].map(constOf)),
      strokeWidth: FloatV(constOf(50)),
      strokeColor: sampleBlack(),
    });
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromCircle(shape);
    expect(numOf(w)).toBeCloseTo(250);
    expect(numOf(h)).toBeCloseTo(250);
    expect(numOf(x)).toBeCloseTo(42);
    expect(numOf(y)).toBeCloseTo(121);
  });

  test("Ellipse", () => {
    const shape = makeEllipse(canvas, {
      rx: FloatV(constOf(200)),
      ry: FloatV(constOf(100)),
      center: VectorV([42, 121].map(constOf)),
      strokeWidth: FloatV(constOf(50)),
      strokeColor: sampleBlack(),
    });
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromEllipse(shape);
    expect(numOf(w)).toBeCloseTo(450);
    expect(numOf(h)).toBeCloseTo(250);
    expect(numOf(x)).toBeCloseTo(42);
    expect(numOf(y)).toBeCloseTo(121);
  });

  test("Rectangle", () => {
    const shape = makeRectangle(canvas, {
      center: VectorV([0, 0].map(constOf)),
      width: FloatV(constOf(150)),
      height: FloatV(constOf(200)),
      strokeWidth: FloatV(constOf(50)),
      strokeColor: sampleBlack(),
    });
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromRect(shape);
    expect(numOf(w)).toBeCloseTo(200);
    expect(numOf(h)).toBeCloseTo(250);
    expect(numOf(x)).toBeCloseTo(0);
    expect(numOf(y)).toBeCloseTo(0);
  });

  test("Polygon", () => {
    const shape = makePolygon(canvas, polyProps());
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromPolygon(shape);
    expect(numOf(w)).toBeCloseTo(113.5);
    expect(numOf(h)).toBeCloseTo(116.5);
    expect(numOf(x)).toBeCloseTo(320.75);
    expect(numOf(y)).toBeCloseTo(70.25);
  });

  test("Polyline", () => {
    const shape = makePolyline(canvas, polyProps());
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromPolygon(shape);
    expect(numOf(w)).toBeCloseTo(113.5);
    expect(numOf(h)).toBeCloseTo(116.5);
    expect(numOf(x)).toBeCloseTo(320.75);
    expect(numOf(y)).toBeCloseTo(70.25);
  });

  test("Image", () => {
    const shape = makeImage(canvas, {
      center: VectorV([0, 0].map(constOf)),
      width: FloatV(constOf(150)),
      height: FloatV(constOf(200)),
    });
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromRectlike(shape);
    expect(numOf(w)).toBeCloseTo(150);
    expect(numOf(h)).toBeCloseTo(200);
    expect(numOf(x)).toBeCloseTo(0);
    expect(numOf(y)).toBeCloseTo(0);
  });

  test("Line", () => {
    const shape = makeLine(canvas, {
      start: VectorV([-300, 200].map(constOf)),
      end: VectorV([100, -150].map(constOf)),
      strokeWidth: FloatV(constOf(50)),
    });
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromLinelike(shape);
    expect(numOf(w)).toBeCloseTo(432.925);
    expect(numOf(h)).toBeCloseTo(387.629);
    expect(numOf(x)).toBeCloseTo(-100);
    expect(numOf(y)).toBeCloseTo(25);
  });

  test("Path (lines)", () => {
    const shape = makePath(canvas, {
      d: compDict.pathFromPoints("open", [
        [constOf(-100), constOf(-100)],
        [constOf(100), constOf(-50)],
        [constOf(-50), constOf(100)],
      ]),
    });
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(w)).toBeCloseTo(200);
    expect(numOf(h)).toBeCloseTo(200);
    expect(numOf(x)).toBeCloseTo(0);
    expect(numOf(y)).toBeCloseTo(0);
  });

  test("Path (quadratic)", () => {
    const shape = makePath(canvas, {
      d: compDict.makePath(
        [constOf(-100), constOf(0)],
        [constOf(100), constOf(0)],
        constOf(50),
        constOf(10)
      ),
    });
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(w)).toBeCloseTo(180);
    expect(numOf(h)).toBeCloseTo(50);
    expect(numOf(x)).toBeCloseTo(0);
    expect(numOf(y)).toBeCloseTo(-25);
  });

  test("Path (cubic)", () => {
    const shape = makePath(canvas, {
      d: compDict.cubicCurveFromPoints("open", [
        [constOf(0), constOf(0)],
        [constOf(50), constOf(50)],
        [constOf(200), constOf(0)],
        [constOf(75), constOf(-25)],
      ]),
    });
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(w)).toBeCloseTo(200);
    expect(numOf(h)).toBeCloseTo(75);
    expect(numOf(x)).toBeCloseTo(100);
    expect(numOf(y)).toBeCloseTo(12.5);
  });

  test("Path (quadratic join)", () => {
    const shape = makePath(canvas, {
      d: compDict.quadraticCurveFromPoints("open", [
        [constOf(0), constOf(0)],
        [constOf(50), constOf(50)],
        [constOf(75), constOf(-25)],
        [constOf(200), constOf(0)],
      ]),
    });
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(w)).toBeCloseTo(200);
    expect(numOf(h)).toBeCloseTo(150);
    expect(numOf(x)).toBeCloseTo(100);
    expect(numOf(y)).toBeCloseTo(-25);
  });

  test("Path (cubic join)", () => {
    const shape = makePath(canvas, {
      d: compDict.cubicCurveFromPoints("open", [
        [constOf(0), constOf(0)],
        [constOf(50), constOf(50)],
        [constOf(200), constOf(0)],
        [constOf(75), constOf(-25)],
        [constOf(0), constOf(-100)],
        [constOf(100), constOf(-75)],
      ]),
    });
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(w)).toBeCloseTo(250);
    expect(numOf(h)).toBeCloseTo(150);
    expect(numOf(x)).toBeCloseTo(75);
    expect(numOf(y)).toBeCloseTo(-25);
  });

  test("Path (arc unscaled)", () => {
    const shape = makePath(canvas, {
      d: compDict.arc(
        "open",
        [constOf(-50), constOf(50)],
        [constOf(100), constOf(-25)],
        [constOf(200), constOf(100)],
        constOf(30),
        constOf(1),
        constOf(0)
      ),
    });
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(w)).toBeCloseTo(400);
    expect(numOf(h)).toBeCloseTo(400);
    expect(numOf(x)).toBeCloseTo(-1.297);
    expect(numOf(y)).toBeCloseTo(-76.281);
  });

  test("Path (arc small)", () => {
    const shape = makePath(canvas, {
      d: compDict.arc(
        "open",
        [constOf(-50), constOf(50)],
        [constOf(100), constOf(-25)],
        [constOf(200), constOf(100)],
        constOf(30),
        constOf(0),
        constOf(0)
      ),
    });
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(w)).toBeCloseTo(400);
    expect(numOf(h)).toBeCloseTo(400);
    expect(numOf(x)).toBeCloseTo(51.297);
    expect(numOf(y)).toBeCloseTo(101.282);
  });

  test("Path (arc scaled)", () => {
    const shape = makePath(canvas, {
      d: compDict.arc(
        "open",
        [constOf(-75), constOf(-50)],
        [constOf(200), constOf(25)],
        [constOf(25), constOf(50)],
        constOf(60),
        constOf(0),
        constOf(0)
      ),
    });
    const {
      w,
      h,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(w)).toBeCloseTo(311.512);
    expect(numOf(h)).toBeCloseTo(311.512);
    expect(numOf(x)).toBeCloseTo(62.5);
    expect(numOf(y)).toBeCloseTo(-12.5);
  });
});

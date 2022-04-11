import { compDict } from "contrib/Functions";
import { numOf } from "engine/Autodiff";
import seedrandom from "seedrandom";
import { makeCircle } from "shapes/Circle";
import { makeEllipse } from "shapes/Ellipse";
import { makeImage } from "shapes/Image";
import { makeLine } from "shapes/Line";
import { makePath } from "shapes/Path";
import { makePolygon } from "shapes/Polygon";
import { makePolyline } from "shapes/Polyline";
import { makeRectangle } from "shapes/Rectangle";
import {
  FloatV,
  makeCanvas,
  PtListV,
  sampleBlack,
  VectorV,
} from "shapes/Samplers";
import { IPoly, IScale } from "types/shapes";
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
    ]
  ),
  scale: FloatV(0.5),
});

describe("bbox", () => {
  test("Circle", () => {
    const shape = makeCircle(seedrandom("bbox Circle"), canvas, {
      r: FloatV(100),
      center: VectorV([42, 121]),
      strokeWidth: FloatV(50),
      strokeColor: sampleBlack(),
    });
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromCircle(shape);
    expect(numOf(width)).toBeCloseTo(250);
    expect(numOf(height)).toBeCloseTo(250);
    expect(numOf(x)).toBeCloseTo(42);
    expect(numOf(y)).toBeCloseTo(121);
  });

  test("Ellipse", () => {
    const shape = makeEllipse(seedrandom("bbox Ellipse"), canvas, {
      rx: FloatV(200),
      ry: FloatV(100),
      center: VectorV([42, 121]),
      strokeWidth: FloatV(50),
      strokeColor: sampleBlack(),
    });
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromEllipse(shape);
    expect(numOf(width)).toBeCloseTo(450);
    expect(numOf(height)).toBeCloseTo(250);
    expect(numOf(x)).toBeCloseTo(42);
    expect(numOf(y)).toBeCloseTo(121);
  });

  test("Rectangle", () => {
    const shape = makeRectangle(seedrandom("bbox Rectangle"), canvas, {
      center: VectorV([0, 0]),
      width: FloatV(150),
      height: FloatV(200),
      strokeWidth: FloatV(50),
      strokeColor: sampleBlack(),
    });
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromRect(shape);
    expect(numOf(width)).toBeCloseTo(200);
    expect(numOf(height)).toBeCloseTo(250);
    expect(numOf(x)).toBeCloseTo(0);
    expect(numOf(y)).toBeCloseTo(0);
  });

  test("Polygon", () => {
    const shape = makePolygon(seedrandom("bbox Polygon"), canvas, polyProps());
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromPolygon(shape);
    expect(numOf(width)).toBeCloseTo(113.5);
    expect(numOf(height)).toBeCloseTo(116.5);
    expect(numOf(x)).toBeCloseTo(320.75);
    expect(numOf(y)).toBeCloseTo(70.25);
  });

  test("Polyline", () => {
    const shape = makePolyline(
      seedrandom("bbox Polyline"),
      canvas,
      polyProps()
    );
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromPolygon(shape);
    expect(numOf(width)).toBeCloseTo(113.5);
    expect(numOf(height)).toBeCloseTo(116.5);
    expect(numOf(x)).toBeCloseTo(320.75);
    expect(numOf(y)).toBeCloseTo(70.25);
  });

  test("Image", () => {
    const shape = makeImage(seedrandom("bbox Image"), canvas, {
      center: VectorV([0, 0]),
      width: FloatV(150),
      height: FloatV(200),
    });
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromRectlike(shape);
    expect(numOf(width)).toBeCloseTo(150);
    expect(numOf(height)).toBeCloseTo(200);
    expect(numOf(x)).toBeCloseTo(0);
    expect(numOf(y)).toBeCloseTo(0);
  });

  test("Line", () => {
    const shape = makeLine(seedrandom("bbox Line"), canvas, {
      start: VectorV([-300, 200]),
      end: VectorV([100, -150]),
      strokeWidth: FloatV(50),
    });
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromLinelike(shape);
    expect(numOf(width)).toBeCloseTo(432.925);
    expect(numOf(height)).toBeCloseTo(387.629);
    expect(numOf(x)).toBeCloseTo(-100);
    expect(numOf(y)).toBeCloseTo(25);
  });

  test("Path (lines)", () => {
    const rng = seedrandom("bbox Path (lines)");
    const shape = makePath(rng, canvas, {
      d: compDict.pathFromPoints({ rng }, "open", [
        [-100, -100],
        [100, -50],
        [-50, 100],
      ]),
    });
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(width)).toBeCloseTo(200);
    expect(numOf(height)).toBeCloseTo(200);
    expect(numOf(x)).toBeCloseTo(0);
    expect(numOf(y)).toBeCloseTo(0);
  });

  test("Path (quadratic)", () => {
    const rng = seedrandom("bbox Path (quadratic)");
    const shape = makePath(rng, canvas, {
      d: compDict.makePath({ rng }, [-100, 0], [100, 0], 50, 10),
    });
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(width)).toBeCloseTo(180);
    expect(numOf(height)).toBeCloseTo(50);
    expect(numOf(x)).toBeCloseTo(0);
    expect(numOf(y)).toBeCloseTo(-25);
  });

  test("Path (cubic)", () => {
    const rng = seedrandom("bbox Path (cubic)");
    const shape = makePath(rng, canvas, {
      d: compDict.cubicCurveFromPoints({ rng }, "open", [
        [0, 0],
        [50, 50],
        [200, 0],
        [75, -25],
      ]),
    });
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(width)).toBeCloseTo(200);
    expect(numOf(height)).toBeCloseTo(75);
    expect(numOf(x)).toBeCloseTo(100);
    expect(numOf(y)).toBeCloseTo(12.5);
  });

  test("Path (quadratic join)", () => {
    const rng = seedrandom("bbox Path (quadratic join)");
    const shape = makePath(rng, canvas, {
      d: compDict.quadraticCurveFromPoints({ rng }, "open", [
        [0, 0],
        [50, 50],
        [75, -25],
        [200, 0],
      ]),
    });
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(width)).toBeCloseTo(200);
    expect(numOf(height)).toBeCloseTo(150);
    expect(numOf(x)).toBeCloseTo(100);
    expect(numOf(y)).toBeCloseTo(-25);
  });

  test("Path (cubic join)", () => {
    const rng = seedrandom("bbox Path (cubic join)");
    const shape = makePath(rng, canvas, {
      d: compDict.cubicCurveFromPoints({ rng }, "open", [
        [0, 0],
        [50, 50],
        [200, 0],
        [75, -25],
        [0, -100],
        [100, -75],
      ]),
    });
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(width)).toBeCloseTo(250);
    expect(numOf(height)).toBeCloseTo(150);
    expect(numOf(x)).toBeCloseTo(75);
    expect(numOf(y)).toBeCloseTo(-25);
  });

  test("Path (arc unscaled)", () => {
    const rng = seedrandom("bbox Path (arc unscaled)");
    const shape = makePath(rng, canvas, {
      d: compDict.arc(
        { rng },
        "open",
        [-50, 50],
        [100, -25],
        [200, 100],
        30,
        1,
        0
      ),
    });
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(width)).toBeCloseTo(400);
    expect(numOf(height)).toBeCloseTo(400);
    expect(numOf(x)).toBeCloseTo(-1.297);
    expect(numOf(y)).toBeCloseTo(-76.281);
  });

  test("Path (arc small)", () => {
    const rng = seedrandom("bbox Path (arc small)");
    const shape = makePath(rng, canvas, {
      d: compDict.arc(
        { rng },
        "open",
        [-50, 50],
        [100, -25],
        [200, 100],
        30,
        0,
        0
      ),
    });
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(width)).toBeCloseTo(400);
    expect(numOf(height)).toBeCloseTo(400);
    expect(numOf(x)).toBeCloseTo(51.297);
    expect(numOf(y)).toBeCloseTo(101.282);
  });

  test("Path (arc scaled)", () => {
    const rng = seedrandom("bbox Path (arc scaled)");
    const shape = makePath(rng, canvas, {
      d: compDict.arc(
        { rng },
        "open",
        [-75, -50],
        [200, 25],
        [25, 50],
        60,
        0,
        0
      ),
    });
    const {
      width,
      height,
      center: [x, y],
    } = bboxFromPath(shape);
    expect(numOf(width)).toBeCloseTo(311.512);
    expect(numOf(height)).toBeCloseTo(311.512);
    expect(numOf(x)).toBeCloseTo(62.5);
    expect(numOf(y)).toBeCloseTo(-12.5);
  });
});

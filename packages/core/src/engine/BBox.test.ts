import { compDict } from "contrib/Functions";
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
import { genCode, secondaryGraph } from "./Autodiff";
import {
  BBox,
  bboxFromCircle,
  bboxFromEllipse,
  bboxFromLinelike,
  bboxFromPath,
  bboxFromPolygon,
  bboxFromRect,
  bboxFromRectlike,
} from "./BBox";

const canvas = makeCanvas(800, 700);

const expectBbox = (
  actual: BBox,
  expected: { width: number; height: number; center: [number, number] }
) => {
  const g = secondaryGraph([
    actual.width,
    actual.height,
    actual.center[0],
    actual.center[1],
  ]);
  const f = genCode(g);
  const [width, height, x, y] = f([]).secondary; // no inputs, so, empty array
  expect(width).toBeCloseTo(expected.width);
  expect(height).toBeCloseTo(expected.height);
  expect(x).toBeCloseTo(expected.center[0]);
  expect(y).toBeCloseTo(expected.center[1]);
};

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
    expectBbox(bboxFromCircle(shape), {
      width: 250,
      height: 250,
      center: [42, 121],
    });
  });

  test("Ellipse", () => {
    const shape = makeEllipse(seedrandom("bbox Ellipse"), canvas, {
      rx: FloatV(200),
      ry: FloatV(100),
      center: VectorV([42, 121]),
      strokeWidth: FloatV(50),
      strokeColor: sampleBlack(),
    });
    expectBbox(bboxFromEllipse(shape), {
      width: 450,
      height: 250,
      center: [42, 121],
    });
  });

  test("Rectangle", () => {
    const shape = makeRectangle(seedrandom("bbox Rectangle"), canvas, {
      center: VectorV([0, 0]),
      width: FloatV(150),
      height: FloatV(200),
      strokeWidth: FloatV(50),
      strokeColor: sampleBlack(),
    });
    expectBbox(bboxFromRect(shape), {
      width: 200,
      height: 250,
      center: [0, 0],
    });
  });

  test("Polygon", () => {
    const shape = makePolygon(seedrandom("bbox Polygon"), canvas, polyProps());
    expectBbox(bboxFromPolygon(shape), {
      width: 113.5,
      height: 116.5,
      center: [320.75, 70.25],
    });
  });

  test("Polyline", () => {
    const shape = makePolyline(
      seedrandom("bbox Polyline"),
      canvas,
      polyProps()
    );
    expectBbox(bboxFromPolygon(shape), {
      width: 113.5,
      height: 116.5,
      center: [320.75, 70.25],
    });
  });

  test("Image", () => {
    const shape = makeImage(seedrandom("bbox Image"), canvas, {
      center: VectorV([0, 0]),
      width: FloatV(150),
      height: FloatV(200),
    });
    expectBbox(bboxFromRectlike(shape), {
      width: 150,
      height: 200,
      center: [0, 0],
    });
  });

  test("Line", () => {
    const shape = makeLine(seedrandom("bbox Line"), canvas, {
      start: VectorV([-300, 200]),
      end: VectorV([100, -150]),
      strokeWidth: FloatV(50),
    });
    expectBbox(bboxFromLinelike(shape), {
      width: 432.925,
      height: 387.629,
      center: [-100, 25],
    });
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
    expectBbox(bboxFromPath(shape), {
      width: 200,
      height: 200,
      center: [0, 0],
    });
  });

  test("Path (quadratic)", () => {
    const rng = seedrandom("bbox Path (quadratic)");
    const shape = makePath(rng, canvas, {
      d: compDict.makePath({ rng }, [-100, 0], [100, 0], 50, 10),
    });
    expectBbox(bboxFromPath(shape), {
      width: 180,
      height: 50,
      center: [0, -25],
    });
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
    expectBbox(bboxFromPath(shape), {
      width: 200,
      height: 75,
      center: [100, 12.5],
    });
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
    expectBbox(bboxFromPath(shape), {
      width: 200,
      height: 150,
      center: [100, -25],
    });
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
    expectBbox(bboxFromPath(shape), {
      width: 200,
      height: 150,
      center: [75, -25],
    });
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
    expectBbox(bboxFromPath(shape), {
      width: 400,
      height: 400,
      center: [-1.297, -76.281],
    });
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
    expectBbox(bboxFromPath(shape), {
      width: 400,
      height: 400,
      center: [51.297, 101.282],
    });
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
    expectBbox(bboxFromPath(shape), {
      width: 311.512,
      height: 311.512,
      center: [62.5, -12.5],
    });
  });
});

import { scalar } from "@tensorflow/tfjs";
import { describe, expect, test } from "vitest";
import { compDict } from "../lib/Functions.js";
import { numsOf } from "../lib/Utils.js";
import { makeCircle } from "../shapes/Circle.js";
import { makeEllipse } from "../shapes/Ellipse.js";
import { makeImage } from "../shapes/Image.js";
import { makeLine } from "../shapes/Line.js";
import { makePath } from "../shapes/Path.js";
import { makePolygon } from "../shapes/Polygon.js";
import { makePolyline } from "../shapes/Polyline.js";
import { makeRectangle } from "../shapes/Rectangle.js";
import { makeCanvas, simpleContext } from "../shapes/Samplers.js";
import * as ad from "../types/ad.js";
import { Poly, Scale } from "../types/shapes.js";
import { black, floatV, ptListV, vectorV } from "../utils/Util.js";
import {
  BBox,
  bboxFromCircle,
  bboxFromEllipse,
  bboxFromLinelike,
  bboxFromPath,
  bboxFromPolygon,
  bboxFromRect,
  bboxFromRectlike,
} from "./BBox.js";

const canvas = makeCanvas(800, 700);

const expectBbox = (
  actual: BBox,
  expected: { width: number; height: number; center: [number, number] },
) => {
  const [width, height, x, y] = numsOf([
    actual.width,
    actual.height,
    actual.center[0],
    actual.center[1],
  ]);
  expect(width).toBeCloseTo(expected.width);
  expect(height).toBeCloseTo(expected.height);
  expect(x).toBeCloseTo(expected.center[0]);
  expect(y).toBeCloseTo(expected.center[1]);
};

const polyProps = (): Poly<ad.Num> & Scale<ad.Num> => ({
  points: ptListV(
    // https://en.wikipedia.org/wiki/Polygon#/media/File:Assorted_polygons.svg
    [
      [scalar(564), scalar(24)],
      [scalar(733), scalar(54)],
      [scalar(755), scalar(154)],
      [scalar(693), scalar(257)],
      [scalar(548), scalar(216)],
      [scalar(571), scalar(145)],
      [scalar(630), scalar(146)],
      [scalar(617), scalar(180)],
      [scalar(664), scalar(196)],
      [scalar(701), scalar(120)],
      [scalar(591), scalar(90)],
      [scalar(528), scalar(129)],
    ],
  ),
  scale: floatV(scalar(0.5)),
});

describe("bbox", () => {
  test("Circle", () => {
    const shape = makeCircle(simpleContext("bbox Circle"), canvas, {
      r: floatV(scalar(100)),
      center: vectorV([scalar(42), scalar(121)]),
      strokeWidth: floatV(scalar(50)),
      strokeColor: black(),
    });
    expectBbox(bboxFromCircle(shape), {
      width: 250,
      height: 250,
      center: [42, 121],
    });
  });

  test("Ellipse", () => {
    const shape = makeEllipse(simpleContext("bbox Ellipse"), canvas, {
      rx: floatV(scalar(200)),
      ry: floatV(scalar(100)),
      center: vectorV([scalar(42), scalar(121)]),
      strokeWidth: floatV(scalar(50)),
      strokeColor: black(),
    });
    expectBbox(bboxFromEllipse(shape), {
      width: 450,
      height: 250,
      center: [42, 121],
    });
  });

  test("Rectangle", () => {
    const shape = makeRectangle(simpleContext("bbox Rectangle"), canvas, {
      center: vectorV([scalar(0), scalar(0)]),
      width: floatV(scalar(150)),
      height: floatV(scalar(200)),
      strokeWidth: floatV(scalar(50)),
      strokeColor: black(),
    });
    expectBbox(bboxFromRect(shape), {
      width: 200,
      height: 250,
      center: [0, 0],
    });
  });

  test("Polygon", () => {
    const shape = makePolygon(
      simpleContext("bbox Polygon"),
      canvas,
      polyProps(),
    );
    expectBbox(bboxFromPolygon(shape), {
      width: 113.5,
      height: 116.5,
      center: [320.75, 70.25],
    });
  });

  test("Polyline", () => {
    const shape = makePolyline(
      simpleContext("bbox Polyline"),
      canvas,
      polyProps(),
    );
    expectBbox(bboxFromPolygon(shape), {
      width: 113.5,
      height: 116.5,
      center: [320.75, 70.25],
    });
  });

  test("Image", () => {
    const shape = makeImage(simpleContext("bbox Image"), canvas, {
      center: vectorV([scalar(0), scalar(0)]),
      width: floatV(scalar(150)),
      height: floatV(scalar(200)),
    });
    expectBbox(bboxFromRectlike(shape), {
      width: 150,
      height: 200,
      center: [0, 0],
    });
  });

  test("Line", () => {
    const shape = makeLine(simpleContext("bbox Line"), canvas, {
      start: vectorV([scalar(-300), scalar(200)]),
      end: vectorV([scalar(100), scalar(-150)]),
      strokeWidth: floatV(scalar(50)),
    });
    expectBbox(bboxFromLinelike(shape), {
      width: 432.925,
      height: 387.629,
      center: [-100, 25],
    });
  });

  test("Path (lines)", () => {
    const context = simpleContext("bbox Path (lines)");
    const shape = makePath(context, canvas, {
      d: compDict.pathFromPoints.body(context, "open", [
        [scalar(-100), scalar(-100)],
        [scalar(100), scalar(-50)],
        [scalar(-50), scalar(100)],
      ]).value,
    });
    expectBbox(bboxFromPath(shape), {
      width: 200,
      height: 200,
      center: [0, 0],
    });
  });

  test("Path (quadratic)", () => {
    const context = simpleContext("bbox Path (quadratic)");
    const shape = makePath(context, canvas, {
      d: compDict.makePath.body(
        context,
        [scalar(-100), scalar(0)],
        [scalar(100), scalar(0)],
        scalar(50),
        scalar(10),
      ).value,
    });
    expectBbox(bboxFromPath(shape), {
      width: 180,
      height: 50,
      center: [0, -25],
    });
  });

  test("Path (cubic)", () => {
    const context = simpleContext("bbox Path (cubic)");
    const shape = makePath(context, canvas, {
      d: compDict.cubicCurveFromPoints.body(context, "open", [
        [scalar(0), scalar(0)],
        [scalar(50), scalar(50)],
        [scalar(200), scalar(0)],
        [scalar(75), scalar(-25)],
      ]).value,
    });
    expectBbox(bboxFromPath(shape), {
      width: 200,
      height: 75,
      center: [100, 12.5],
    });
  });

  test("Path (quadratic join)", () => {
    const context = simpleContext("bbox Path (quadratic join)");
    const shape = makePath(context, canvas, {
      d: compDict.quadraticCurveFromPoints.body(context, "open", [
        [scalar(0), scalar(0)],
        [scalar(50), scalar(50)],
        [scalar(75), scalar(-25)],
        [scalar(200), scalar(0)],
      ]).value,
    });
    expectBbox(bboxFromPath(shape), {
      width: 200,
      height: 150,
      center: [100, -25],
    });
  });

  test("Path (cubic join)", () => {
    const context = simpleContext("bbox Path (cubic join)");
    const shape = makePath(context, canvas, {
      d: compDict.cubicCurveFromPoints.body(context, "open", [
        [scalar(0), scalar(0)],
        [scalar(50), scalar(50)],
        [scalar(200), scalar(0)],
        [scalar(75), scalar(-25)],
        [scalar(0), scalar(-100)],
        [scalar(100), scalar(-75)],
      ]).value,
    });
    expectBbox(bboxFromPath(shape), {
      width: 250,
      height: 150,
      center: [75, -25],
    });
  });

  test("Path (arc unscaled)", () => {
    const context = simpleContext("bbox Path (arc unscaled)");
    const shape = makePath(context, canvas, {
      d: compDict.arc.body(
        context,
        "open",
        [scalar(-50), scalar(50)],
        [scalar(100), scalar(-25)],
        [scalar(200), scalar(100)],
        scalar(30),
        scalar(1),
        scalar(0),
      ).value,
    });
    expectBbox(bboxFromPath(shape), {
      width: 400,
      height: 400,
      center: [-1.297, -76.281],
    });
  });

  test("Path (arc small)", () => {
    const context = simpleContext("bbox Path (arc small)");
    const shape = makePath(context, canvas, {
      d: compDict.arc.body(
        context,
        "open",
        [scalar(-50), scalar(50)],
        [scalar(100), scalar(-25)],
        [scalar(200), scalar(100)],
        scalar(30),
        scalar(0),
        scalar(0),
      ).value,
    });
    expectBbox(bboxFromPath(shape), {
      width: 400,
      height: 400,
      center: [51.297, 101.282],
    });
  });

  test("Path (arc scaled)", () => {
    const context = simpleContext("bbox Path (arc scaled)");
    const shape = makePath(context, canvas, {
      d: compDict.arc.body(
        context,
        "open",
        [scalar(-75), scalar(-50)],
        [scalar(200), scalar(25)],
        [scalar(25), scalar(50)],
        scalar(60),
        scalar(0),
        scalar(0),
      ).value,
    });
    expectBbox(bboxFromPath(shape), {
      width: 311.512,
      height: 311.512,
      center: [62.5, -12.5],
    });
  });
});

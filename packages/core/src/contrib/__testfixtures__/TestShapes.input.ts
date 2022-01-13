import { constOf } from "engine/Autodiff";
import {
  FloatV,
  makeCanvas,
  sampleBlack,
  VectorV,
  PtListV,
} from "shapes/Samplers";
import { makeRectangle } from "shapes/Rectangle";
import { makeCircle } from "shapes/Circle";
import { makePolygon } from "shapes/Polygon";

const canvas = makeCanvas(800, 700);

export const _rectangles = [
  {
    center: [0, 0],
    width: 400,
    height: 400,
  },
  {
    center: [0, 0],
    width: 200,
    height: 200,
  },
  {
    center: [200, 0],
    width: 200,
    height: 200,
  },
  {
    center: [0, 300],
    width: 200,
    height: 200,
  },
].map((x) =>
  makeRectangle(canvas, {
    center: VectorV(x.center.map(constOf)),
    width: FloatV(constOf(x.width)),
    height: FloatV(constOf(x.height)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  })
);

export const _circles = [
  { center: [0, 0], r: 200 },
  { center: [0, 0], r: 100 },
  { center: [200, 0], r: 100 },
  { center: [0, 300], r: 100 },
].map((x) =>
  makeCircle(canvas, {
    r: FloatV(constOf(x.r)),
    center: VectorV(x.center.map(constOf)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  })
);

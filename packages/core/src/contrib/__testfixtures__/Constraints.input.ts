import { constOf, EPS_DENOM, numOf } from "engine/Autodiff";
import { FloatV, makeCanvas, sampleBlack, VectorV } from "shapes/Samplers";
import { makeRectangle } from "shapes/Rectangle";
import { makeCircle } from "shapes/Circle";

const canvas = makeCanvas(800, 700);

export const _rectangles = [ 
  // Rectangle 0
  makeRectangle(canvas, {
    center: VectorV([0, 0].map(constOf)),
    width: FloatV(constOf(400)),
    height: FloatV(constOf(400)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  }),
  // Rectangle 1
  makeRectangle(canvas, {
    center: VectorV([0, 0].map(constOf)),
    width: FloatV(constOf(200)),
    height: FloatV(constOf(200)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  }),
  // Rectangle 2
  makeRectangle(canvas, {
    center: VectorV([200, 0].map(constOf)),
    width: FloatV(constOf(200)),
    height: FloatV(constOf(200)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  }),
  // Rectangle 3
  makeRectangle(canvas, {
    center: VectorV([0, 300].map(constOf)),
    width: FloatV(constOf(200)),
    height: FloatV(constOf(200)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  })
];

export const _circles = [
  // Circle 0
  makeCircle(canvas, {
    r: FloatV(constOf(200)),
    center: VectorV([0, 0].map(constOf)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  }),
  // Circle 1
  makeCircle(canvas, {
    r: FloatV(constOf(100)),
    center: VectorV([0, 0].map(constOf)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  }),
  // Circle 2
  makeCircle(canvas, {
    r: FloatV(constOf(100)),
    center: VectorV([200, 0].map(constOf)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  }),
  // Circle 3
  makeCircle(canvas, {
    r: FloatV(constOf(100)),
    center: VectorV([0, 300].map(constOf)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  }),
];

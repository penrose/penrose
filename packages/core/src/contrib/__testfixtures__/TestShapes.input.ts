import { constOf } from "engine/Autodiff";
import seedrandom from "seedrandom";
import { makeCircle } from "shapes/Circle";
import { makeLine } from "shapes/Line";
import { makePolygon } from "shapes/Polygon";
import { makeRectangle } from "shapes/Rectangle";
import {
  FloatV,
  makeCanvas,
  PtListV,
  sampleBlack,
  VectorV,
} from "shapes/Samplers";

const rng = seedrandom("TestShapes.input");
const canvas = makeCanvas(800, 700);

export const _rectangles = [
  { center: [0, 0], width: 400, height: 400 },
  { center: [0, 0], width: 200, height: 200 },
  { center: [200, 0], width: 200, height: 200 },
  { center: [0, 300], width: 200, height: 200 },
].map((x) =>
  makeRectangle(rng, canvas, {
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
  { center: [0, 200], r: 100 },
  { center: [150, 150], r: 50 },
  { center: [150, 150], r: 100 },
].map((x) =>
  makeCircle(rng, canvas, {
    r: FloatV(constOf(x.r)),
    center: VectorV(x.center.map(constOf)),
    strokeWidth: FloatV(constOf(0)),
    strokeColor: sampleBlack(),
  })
);

export const _lines = [
  { start: [100, 0], end: [100, 300] },
  { start: [100, 100], end: [200, 200] },
  { start: [100, 300], end: [400, 300] },
  { start: [200, 400], end: [300, 100] },
].map((x) =>
  makeLine(rng, canvas, {
    start: VectorV(x.start.map(constOf)),
    end: VectorV(x.end.map(constOf)),
    strokeWidth: FloatV(constOf(0)),
  })
);

export const _polygons = [
  [
    [100, 0],
    [300, 200],
    [100, 300],
    [0, 100],
  ],
  [
    [100, 100],
    [200, 200],
    [100, 200],
  ],
  [
    [300, 0],
    [400, 100],
    [400, 300],
    [200, 100],
  ],
  [
    [300, 100],
    [400, 200],
    [300, 400],
    [200, 400],
  ],
  [
    [100, 100],
    [300, 0],
    [200, 200],
    [100, 200],
  ],
].map((pts) =>
  makePolygon(rng, canvas, {
    points: PtListV(pts.map((p) => p.map(constOf))),
    scale: FloatV(constOf(1)),
  })
);

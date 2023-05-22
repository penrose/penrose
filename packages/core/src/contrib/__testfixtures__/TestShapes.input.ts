import { makeCircle } from "../../shapes/Circle.js";
import { makeEllipse } from "../../shapes/Ellipse.js";
import { makeLine } from "../../shapes/Line.js";
import { makePolygon } from "../../shapes/Polygon.js";
import { makePolyline } from "../../shapes/Polyline.js";
import { makeRectangle } from "../../shapes/Rectangle.js";
import { makeCanvas, simpleContext } from "../../shapes/Samplers.js";
import { Pt2 } from "../../types/ad.js";
import { black, floatV, ptListV, vectorV } from "../../utils/Util.js";

const context = simpleContext("TestShapes.input");
const canvas = makeCanvas(800, 700);

export const _rectangles = [
  { center: [0, 0], width: 400, height: 400 },
  { center: [0, 0], width: 200, height: 200 },
  { center: [200, 0], width: 200, height: 200 },
  { center: [0, 300], width: 200, height: 200 },
].map((x) =>
  makeRectangle(context, canvas, {
    center: vectorV(x.center),
    width: floatV(x.width),
    height: floatV(x.height),
    strokeWidth: floatV(0),
    strokeColor: black(),
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
  makeCircle(context, canvas, {
    r: floatV(x.r),
    center: vectorV(x.center),
    strokeWidth: floatV(0),
    strokeColor: black(),
  })
);

export const _lines = [
  { start: [100, 0], end: [100, 300] },
  { start: [100, 100], end: [200, 200] },
  { start: [100, 300], end: [400, 300] },
  { start: [200, 400], end: [300, 100] },
].map((x) =>
  makeLine(context, canvas, {
    start: vectorV(x.start),
    end: vectorV(x.end),
    strokeWidth: floatV(0),
  })
);

const polyPts: Pt2[][] = [
  [
    // 0
    [100, 0],
    [300, 200],
    [100, 300],
    [0, 100],
  ],
  [
    // 1
    [100, 100],
    [200, 200],
    [100, 200],
  ],
  [
    // 2
    [300, 0],
    [400, 100],
    [400, 300],
    [200, 100],
  ],
  [
    // 3
    [300, 100],
    [400, 200],
    [300, 400],
    [200, 400],
  ],
  [
    // 4
    [100, 100],
    [300, 0],
    [200, 200],
    [100, 200],
  ],
  [
    // 5
    [0, 0],
    [100, 100],
    [100, -100],
    [0, 100],
  ],
  [
    // 6
    [200, 0],
    [300, 0],
    [300, 100],
    [200, 100],
  ],
  [
    // 7
    [300, 0],
    [200, 0],
    [200, 200],
    [300, 200],
  ],
  [
    // 8
    [100, 0],
    [300, 0],
    [300, 200],
    [0, 200],
    [0, 100],
    [200, 100],
    [200, 200],
    [100, 200],
  ],
  [
    // 9
    [0, 0],
    [100, 0],
    [200, 0],
    [200, 100],
    [100, 100],
    [0, 100],
  ],
  [
    // 10
    [0, 0],
    [0, 100],
    [100, 100],
    [100, 200],
  ],
];

export const _ellipses = [
  // Circles
  { rx: 200, ry: 200, center: [0, 0] },
  { rx: 100, ry: 100, center: [0, 0] },
  { rx: 100, ry: 100, center: [200, 0] },
  { rx: 100, ry: 100, center: [0, 300] },
  { rx: 100, ry: 100, center: [0, 200] },
  { rx: 50, ry: 50, center: [150, 150] },
  { rx: 100, ry: 100, center: [150, 150] },
  // Non circles
  { rx: 200, ry: 100, center: [0, 0] },
  { rx: 50, ry: 150, center: [50, 150] },
  { rx: 100, ry: 200, center: [300, 0] },
  { rx: 150, ry: 100, center: [100, 250] },
  { rx: 100, ry: 50, center: [0, 0] },
].map((x) =>
  makeEllipse(context, canvas, {
    rx: floatV(x.rx),
    ry: floatV(x.ry),
    center: vectorV(x.center),
    strokeWidth: floatV(0),
    strokeColor: black(),
  })
);

export const _polygons = polyPts.map((pts) =>
  makePolygon(context, canvas, {
    points: ptListV(pts),
    scale: floatV(1),
  })
);

export const _polylines = polyPts.map((pts) =>
  makePolyline(context, canvas, {
    points: ptListV(pts),
    scale: floatV(1),
  })
);

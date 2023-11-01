import { scalar } from "@tensorflow/tfjs";
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
  { center: [scalar(0), scalar(0)], width: scalar(400), height: scalar(400) },
  { center: [scalar(0), scalar(0)], width: scalar(200), height: scalar(200) },
  { center: [scalar(200), scalar(0)], width: scalar(200), height: scalar(200) },
  { center: [scalar(0), scalar(300)], width: scalar(200), height: scalar(200) },
].map((x) =>
  makeRectangle(context, canvas, {
    center: vectorV(x.center),
    width: floatV(x.width),
    height: floatV(x.height),
    strokeWidth: floatV(scalar(0)),
    strokeColor: black(),
  }),
);

export const _circles = [
  { center: [scalar(0), scalar(0)], r: scalar(200) },
  { center: [scalar(0), scalar(0)], r: scalar(100) },
  { center: [scalar(200), scalar(0)], r: scalar(100) },
  { center: [scalar(0), scalar(300)], r: scalar(100) },
  { center: [scalar(0), scalar(200)], r: scalar(100) },
  { center: [scalar(150), scalar(150)], r: scalar(50) },
  { center: [scalar(150), scalar(150)], r: scalar(100) },
].map((x) =>
  makeCircle(context, canvas, {
    r: floatV(x.r),
    center: vectorV(x.center),
    strokeWidth: floatV(scalar(0)),
    strokeColor: black(),
  }),
);

export const _lines = [
  { start: [scalar(100), scalar(0)], end: [scalar(100), scalar(300)] },
  { start: [scalar(100), scalar(100)], end: [scalar(200), scalar(200)] },
  { start: [scalar(100), scalar(300)], end: [scalar(400), scalar(300)] },
  { start: [scalar(200), scalar(400)], end: [scalar(300), scalar(100)] },
].map((x) =>
  makeLine(context, canvas, {
    start: vectorV(x.start),
    end: vectorV(x.end),
    strokeWidth: floatV(scalar(0)),
  }),
);

const polyPts: Pt2[][] = [
  [
    // 0
    [scalar(100), scalar(0)],
    [scalar(300), scalar(200)],
    [scalar(100), scalar(300)],
    [scalar(0), scalar(100)],
  ],
  [
    // 1
    [scalar(100), scalar(100)],
    [scalar(200), scalar(200)],
    [scalar(100), scalar(200)],
  ],
  [
    // 2
    [scalar(300), scalar(0)],
    [scalar(400), scalar(100)],
    [scalar(400), scalar(300)],
    [scalar(200), scalar(100)],
  ],
  [
    // 3
    [scalar(300), scalar(100)],
    [scalar(400), scalar(200)],
    [scalar(300), scalar(400)],
    [scalar(200), scalar(400)],
  ],
  [
    // 4
    [scalar(100), scalar(100)],
    [scalar(300), scalar(0)],
    [scalar(200), scalar(200)],
    [scalar(100), scalar(200)],
  ],
  [
    // 5
    [scalar(0), scalar(0)],
    [scalar(100), scalar(100)],
    [scalar(100), scalar(-100)],
    [scalar(0), scalar(100)],
  ],
  [
    // 6
    [scalar(200), scalar(0)],
    [scalar(300), scalar(0)],
    [scalar(300), scalar(100)],
    [scalar(200), scalar(100)],
  ],
  [
    // 7
    [scalar(300), scalar(0)],
    [scalar(200), scalar(0)],
    [scalar(200), scalar(200)],
    [scalar(300), scalar(200)],
  ],
  [
    // 8
    [scalar(100), scalar(0)],
    [scalar(300), scalar(0)],
    [scalar(300), scalar(200)],
    [scalar(0), scalar(200)],
    [scalar(0), scalar(100)],
    [scalar(200), scalar(100)],
    [scalar(200), scalar(200)],
    [scalar(100), scalar(200)],
  ],
  [
    // 9
    [scalar(0), scalar(0)],
    [scalar(100), scalar(0)],
    [scalar(200), scalar(0)],
    [scalar(200), scalar(100)],
    [scalar(100), scalar(100)],
    [scalar(0), scalar(100)],
  ],
  [
    // 10
    [scalar(0), scalar(0)],
    [scalar(0), scalar(100)],
    [scalar(100), scalar(100)],
    [scalar(100), scalar(200)],
  ],
];

export const _ellipses = [
  // Circles
  { rx: scalar(200), ry: scalar(200), center: [scalar(0), scalar(0)] },
  { rx: scalar(100), ry: scalar(100), center: [scalar(0), scalar(0)] },
  { rx: scalar(100), ry: scalar(100), center: [scalar(200), scalar(0)] },
  { rx: scalar(100), ry: scalar(100), center: [scalar(0), scalar(300)] },
  { rx: scalar(100), ry: scalar(100), center: [scalar(0), scalar(200)] },
  { rx: scalar(50), ry: scalar(50), center: [scalar(150), scalar(150)] },
  { rx: scalar(100), ry: scalar(100), center: [scalar(150), scalar(150)] },
  // Non circles
  { rx: scalar(200), ry: scalar(100), center: [scalar(0), scalar(0)] },
  { rx: scalar(50), ry: scalar(150), center: [scalar(50), scalar(150)] },
  { rx: scalar(100), ry: scalar(200), center: [scalar(300), scalar(0)] },
  { rx: scalar(150), ry: scalar(100), center: [scalar(100), scalar(250)] },
  { rx: scalar(100), ry: scalar(50), center: [scalar(0), scalar(0)] },
].map((x) =>
  makeEllipse(context, canvas, {
    rx: floatV(x.rx),
    ry: floatV(x.ry),
    center: vectorV(x.center),
    strokeWidth: floatV(scalar(0)),
    strokeColor: black(),
  }),
);

export const _polygons = polyPts.map((pts) =>
  makePolygon(context, canvas, {
    points: ptListV(pts),
    scale: floatV(scalar(1)),
  }),
);

export const _polylines = polyPts.map((pts) =>
  makePolyline(context, canvas, {
    points: ptListV(pts),
    scale: floatV(scalar(1)),
  }),
);

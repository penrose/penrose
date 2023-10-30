import { ops } from "../engine/Autodiff.js";
import {
  add,
  div,
  gt,
  ifCond,
  lt,
  lte,
  max,
  maxN,
  min,
  minN,
  mul,
  neg,
  sqrt,
  squared,
  sub,
  xor,
} from "../engine/AutodiffFunctions.js";
import * as BBox from "../engine/BBox.js";
import { Shape, computeShapeBbox } from "../shapes/Shapes.js";
import * as ad from "../types/ad.js";
import { MayWarn } from "../types/functions.js";
import { noWarn } from "../utils/Util.js";
import { msign, signedDistancePolygon } from "./Functions.js";
import {
  convexPartitions,
  overlappingPolygonPoints,
  overlappingPolygonPointsEllipse,
  rectangleDifference,
  rectangleSignedDistance,
} from "./Minkowski.js";
import { isLinelike, isPolygonlike, isRectlike, toPt } from "./Utils.js";

/**
 * Return bounding box from any provided shape.
 */
export const bboxFromShape = (shape: Shape<ad.Num>): BBox.BBox => {
  return computeShapeBbox(shape);
};

export const bboxPts = (bbox: BBox.BBox): ad.Pt2[] => {
  const { topLeft, topRight, bottomLeft, bottomRight } = BBox.corners(bbox);
  return [topRight, topLeft, bottomLeft, bottomRight];
};

export const rectPts = (
  center: ad.Num[],
  width: ad.Num,
  height: ad.Num,
  clockwise: ad.Num = 0,
): ad.Pt2[] => {
  const ccw = neg(clockwise);

  const hw = div(width, 2);
  const hh = div(height, 2);

  const tl0 = [neg(hw), hh];
  const tr0 = [hw, hh];
  const br0 = [hw, neg(hh)];
  const bl0 = [neg(hw), neg(hh)];

  const pts0 = [tr0, tl0, bl0, br0];
  const pts1 = pts0.map((pt) => ops.vrot(pt, ccw));
  const pts2 = pts1.map((pt) => ops.vadd(pt, center));

  const [tr2, tl2, bl2, br2] = pts2;
  if (!(ad.isPt2(tr2) && ad.isPt2(tl2) && ad.isPt2(bl2) && ad.isPt2(br2))) {
    throw new Error("ops.vadd did not preserve dimension");
  }
  return [tr2, tl2, bl2, br2];
};

/**
 * Return center of the shape `shape`.
 * For shapes without the property `center`, the center of their bounding box is returned.
 */
export const shapeCenter = (s: Shape<ad.Num>): ad.Pt2 => {
  if ("center" in s) {
    return [s.center.contents[0], s.center.contents[1]];
  } else {
    // Return center of bounding box
    const bbox = bboxFromShape(s);
    return bbox.center;
  }
};

/**
 * Return vertices of polygon-like shapes.
 */
export const polygonLikePoints = (s: Shape<ad.Num>): ad.Pt2[] => {
  const t = s.shapeType;
  if (t === "Polygon" || t === "Polyline")
    return s.points.contents.map((point) => [point[0], point[1]]);
  else if (isLinelike(s))
    return [
      [s.start.contents[0], s.start.contents[1]],
      [s.end.contents[0], s.end.contents[1]],
    ];
  else if (isRectlike(s)) {
    // TODO: add support for rotated rectangles
    const bbox = bboxFromShape(s);
    const corners = BBox.corners(bbox);
    return [
      corners.topRight,
      corners.topLeft,
      corners.bottomLeft,
      corners.bottomRight,
    ];
  } else {
    throw new Error(`${t} not supported for polygonLikePoints.`);
  }
};

/**
 * Return outward unit normal vector to `lineSegment` with respect to `insidePoint`.
 * @param lineSegment Two points defining the line segment.
 * @param insidePoint Any point inside of the half-plane.
 */
export const outwardUnitNormal = (
  lineSegment: ad.Num[][],
  insidePoint: ad.Num[],
): ad.Num[] => {
  const normal = ops.vnormalize(
    ops.rot90(ops.vsub(lineSegment[1], lineSegment[0])),
  );
  const insideValue = ops.vdot(ops.vsub(insidePoint, lineSegment[0]), normal);
  return ops.vmul(neg(msign(insideValue)), normal);
};

/**
 * Return the signed distance from the origin to the convex
 * counterclockwise-oriented polygon `p`.
 */
export const convexPolygonOriginSignedDistance = (p: ad.Pt2[]): ad.Num => {
  const n = p.length;
  const segments = p.map(([x0, y0], i) => {
    // call (x0, y0) "p0"
    const [x1, y1] = p[(i + 1) % n]; // call this "p1"
    const dx = sub(x1, x0);
    const dy = sub(y1, y0);
    // call (dx, dy) "v"
    const squaredLen = add(squared(dx), squared(dy));

    // Consider the line through p0 and p1. Call the vector from p0 to the
    // origin (that is, -p0) "u". If we call the dot product of u and v `along`,
    // then dividing that by the dot product of v with itself (`squaredLen`)
    // gives us a number that locates for us the closest point to the origin on
    // this line. If this number is between zero and one, then the closest point
    // lies on the line segment; otherwise, it lies on the line but outside the
    // segment between p0 and p1. But instead of dividing by `squaredLen`, we
    // can just compare `along` to zero and `squaredLen` to get the booleans
    // `goodLeft` and `goodRight` directly.
    const along = neg(add(mul(x0, dx), mul(y0, dy)));

    return {
      goodLeft: lte(0, along),
      goodRight: lt(along, squaredLen),

      // distance from origin to p0
      fromStart: sqrt(add(squared(x0), squared(y0))),

      // signed distance from origin using the half-plane defined by the
      // directed line from p0 to p1 (not the line segment)
      edgeSignedDist: div(sub(mul(dx, y0), mul(x0, dy)), sqrt(squaredLen)),
    };
  });

  // Since the polygon is convex, either the origin is inside the polygon (in
  // which case the closest point must be on an edge, not a vertex) or the
  // origin is outside the polygon. We compose the signed distance function as
  // the maximum of several components, taking one component for each edge; we
  // can think of these components as partial functions, where we set them to
  // negative infinity outside their "domain" since we're taking the maximum. So
  // for each edge, we include the region between the two lines perpendicular to
  // that edge passing through its two vertices (which is defined by the
  // half-plane SDF) and the region outside the polygon where the closest point
  // is the second endpoint of the edge. This particular choice of regions is
  // nice because it means that the only discontinuities in the computation of
  // the SDF are actual discontinuities in the form of its derivative.
  return maxN(
    segments.map(({ goodLeft, goodRight, edgeSignedDist }, i) => {
      const next = segments[(i + 1) % n];
      return ifCond(
        goodLeft,
        ifCond(
          goodRight,
          edgeSignedDist,
          ifCond(next.goodLeft, -Infinity, next.fromStart),
        ),
        -Infinity,
      );
    }),
  );
};

/**
 * Return the signed distance from the origin to the Minkowski sum of `rect` and
 * the negative of `line` (that is, `start` and `end` points both multiplied by
 * `-1`).
 */
export const rectLineDist = (
  rx0: ad.Num,
  ry0: ad.Num,
  rx1: ad.Num,
  ry1: ad.Num,
  lxs: ad.Num,
  lys: ad.Num,
  lxe: ad.Num,
  lye: ad.Num,
): ad.Num => {
  const px = gt(lxs, lxe);
  const py = gt(lys, lye);

  // 0 means the negative is lesser, 1 means the negative is greater
  const lx0 = ifCond(px, lxs, lxe);
  const lx1 = ifCond(px, lxe, lxs);
  const ly0 = ifCond(py, lys, lye);
  const ly1 = ifCond(py, lye, lys);

  const x00 = sub(rx0, lx0);
  const x01 = sub(rx0, lx1);
  const x10 = sub(rx1, lx0);
  const x11 = sub(rx1, lx1);

  const y00 = sub(ry0, ly0);
  const y01 = sub(ry0, ly1);
  const y10 = sub(ry1, ly0);
  const y11 = sub(ry1, ly1);

  const p = xor(px, py); // true iff negative slope

  // if `p`:
  //
  // 4 - 3
  // |    \
  // 5     2
  //  \    |
  //   0 - 1
  //
  // point 0 is `ops.vsub([rx0, ry0], [lx1, ly0])`
  // point 1 is `ops.vsub([rx1, rx0], [lx1, ly0])`
  // point 2 is `ops.vsub([rx1, ry1], [lx1, ly0])`
  // point 3 is `ops.vsub([rx1, ry1], [lx0, ly1])`
  // point 4 is `ops.vsub([rx0, ry1], [lx0, ly1])`
  // point 5 is `ops.vsub([rx0, ry0], [lx0, ly1])`

  // if not `p`:
  //
  //   4 - 3
  //  /    |
  // 5     2
  // |    /
  // 0 - 1
  //
  // point 0 is `ops.vsub([rx0, ry0], [lx0, ly0])`
  // point 1 is `ops.vsub([rx1, ry0], [lx0, ly0])`
  // point 2 is `ops.vsub([rx1, ry0], [lx1, ly1])`
  // point 3 is `ops.vsub([rx1, ry1], [lx1, ly1])`
  // point 4 is `ops.vsub([rx0, ry1], [lx1, ly1])`
  // point 5 is `ops.vsub([rx0, ry1], [lx0, ly0])`

  const x0 = ifCond(p, x01, x00);
  const x1 = ifCond(p, x11, x10);
  const x2 = x11;
  const x3 = ifCond(p, x10, x11);
  const x4 = ifCond(p, x00, x01);
  const x5 = x00;

  const y0 = y00;
  const y1 = y00;
  const y2 = ifCond(p, y10, y01);
  const y3 = y11;
  const y4 = y11;
  const y5 = ifCond(p, y01, y10);

  return convexPolygonOriginSignedDistance([
    [x0, y0],
    [x1, y1],
    [x2, y2],
    [x3, y3],
    [x4, y4],
    [x5, y5],
  ]);
};

export const shapeDistance = (
  s1: Shape<ad.Num>,
  s2: Shape<ad.Num>,
): MayWarn<ad.Num> => {
  const t1 = s1.shapeType;
  const t2 = s2.shapeType;
  // Same shapes
  if (t1 === "Circle" && t2 === "Circle") {
    return noWarn(
      shapeDistanceCircles(
        toPt(s1.center.contents),
        s1.r.contents,
        toPt(s2.center.contents),
        s2.r.contents,
      ),
    );
  } else if (isRectlike(s1) && isRectlike(s2)) {
    return noWarn(
      shapeDistanceRects(
        bboxPts(BBox.bboxFromRectlike(s1)),
        bboxPts(BBox.bboxFromRectlike(s2)),
      ),
    );
  }
  // HACK: text/label-line, mainly to skip convex partitioning
  else if (isRectlike(s1) && t2 === "Line") {
    return noWarn(
      shapeDistanceRectLine(
        bboxPts(BBox.bboxFromRectlike(s1)),
        toPt(s2.start.contents),
        toPt(s2.end.contents),
      ),
    );
  } else if (t1 === "Line" && isRectlike(s2)) {
    return noWarn(
      shapeDistanceRectLine(
        bboxPts(BBox.bboxFromRectlike(s2)),
        toPt(s1.start.contents),
        toPt(s1.end.contents),
      ),
    );
  } else if (isPolygonlike(s1) && isPolygonlike(s2)) {
    return noWarn(
      shapeDistancePolys(polygonLikePoints(s1), polygonLikePoints(s2)),
    );
  }
  // Rectangle x Circle
  else if (isRectlike(s1) && t2 === "Circle") {
    return noWarn(
      shapeDistanceRectCircle(
        bboxPts(BBox.bboxFromRectlike(s1)),
        toPt(s2.center.contents),
        s2.r.contents,
      ),
    );
  } else if (t1 === "Circle" && isRectlike(s2)) {
    return noWarn(
      shapeDistanceRectCircle(
        bboxPts(BBox.bboxFromRectlike(s2)),
        toPt(s1.center.contents),
        s1.r.contents,
      ),
    );
  }
  // Polygon x Ellipse
  else if (isPolygonlike(s1) && t2 === "Ellipse") {
    return noWarn(
      shapeDistancePolyEllipse(
        polygonLikePoints(s1),
        toPt(s2.center.contents),
        s2.rx.contents,
        s2.ry.contents,
      ),
    );
  } else if (t1 === "Ellipse" && isPolygonlike(s2)) {
    return noWarn(
      shapeDistancePolyEllipse(
        polygonLikePoints(s2),
        toPt(s1.center.contents),
        s1.rx.contents,
        s1.ry.contents,
      ),
    );
  }
  // Circle x Line
  else if (t1 === "Circle" && t2 === "Line") {
    return noWarn(
      shapeDistanceCircleLine(
        toPt(s1.center.contents),
        s1.r.contents,
        toPt(s2.start.contents),
        toPt(s2.end.contents),
      ),
    );
  } else if (t1 === "Line" && t2 === "Circle") {
    return noWarn(
      shapeDistanceCircleLine(
        toPt(s2.center.contents),
        s2.r.contents,
        toPt(s1.start.contents),
        toPt(s1.end.contents),
      ),
    );
  }
  // Line x Line
  else if (t1 === "Line" && t2 === "Line") {
    return noWarn(
      shapeDistanceLines(
        toPt(s1.start.contents),
        toPt(s1.end.contents),
        toPt(s2.start.contents),
        toPt(s2.end.contents),
      ),
    );
  } else if (t1 === "Polyline" && t2 === "Circle") {
    return noWarn(
      shapeDistanceCirclePolyline(
        s2.center.contents,
        s2.r.contents,
        s1.points.contents,
      ),
    );
  } else if (t2 === "Polyline" && t1 === "Circle") {
    return noWarn(
      shapeDistanceCirclePolyline(
        s1.center.contents,
        s1.r.contents,
        s2.points.contents,
      ),
    );
  } else if (t1 === "Polyline" && isRectlike(s2)) {
    const bbox = bboxFromShape(s2);
    const corners = BBox.corners(bbox);
    return noWarn(
      shapeDistanceRectlikePolyline(
        [
          corners.topRight,
          corners.topLeft,
          corners.bottomLeft,
          corners.bottomRight,
        ],
        s1.points.contents,
      ),
    );
  } else if (t2 === "Polyline" && isRectlike(s1)) {
    const bbox = bboxFromShape(s1);
    const corners = BBox.corners(bbox);
    return noWarn(
      shapeDistanceRectlikePolyline(
        [
          corners.topRight,
          corners.topLeft,
          corners.bottomLeft,
          corners.bottomRight,
        ],
        s2.points.contents,
      ),
    );
  }
  // Default to axis-aligned bounding boxes
  else {
    return {
      value: shapeDistanceRects(
        bboxPts(bboxFromShape(s1)),
        bboxPts(bboxFromShape(s2)),
      ),
      warnings: [
        {
          tag: "BBoxApproximationWarning",
          stack: [
            {
              signature: `shapeDistance(${t1}, ${t2})`,
            },
          ],
        },
      ],
    };
  }
};

export const shapeDistanceCircles = (
  c1: ad.Pt2,
  r1: ad.Num,
  c2: ad.Pt2,
  r2: ad.Num,
): ad.Num => sub(ops.vdist(c1, c2), add(r1, r2));

export const shapeDistanceRects = (
  rect1: ad.Pt2[],
  rect2: ad.Pt2[],
): ad.Num => {
  if (rect1.length !== 4 || rect2.length !== 4) {
    throw new Error("Expects rect1 and rect2 to have four points each");
  }
  const [diffBL, diffTR] = rectangleDifference(rect1, rect2, 0);
  return rectangleSignedDistance(diffBL, diffTR);
};

export const shapeDistanceRectLine = (
  rect: ad.Pt2[],
  start: ad.Pt2,
  end: ad.Pt2,
): ad.Num => {
  if (rect.length !== 4) {
    throw new Error("Expects rect to have four points");
  }
  const bbox = BBox.bboxFromPoints(rect);
  const halfW = div(bbox.width, 2);
  const halfH = div(bbox.height, 2);
  const [cx, cy] = bbox.center;
  return rectLineDist(
    sub(cx, halfW),
    sub(cy, halfH),
    add(cx, halfW),
    add(cy, halfH),
    start[0],
    start[1],
    end[0],
    end[1],
  );
};

export const shapeDistanceRectlikePolyline = (
  rect: ad.Pt2[],
  points: ad.Num[][],
): ad.Num => {
  let dMin: ad.Num = Infinity;

  const topRight = rect[0];
  const bottomLeft = rect[2];

  // take minimum distance to rect R over all segments in polyline M
  for (let i = 0; i < points.length - 1; i++) {
    const a = points[i];
    const b = points[i + 1];

    const d = rectLineDist(
      bottomLeft[0],
      bottomLeft[1],
      topRight[0],
      topRight[1],
      a[0],
      a[1],
      b[0],
      b[1],
    );
    dMin = min(dMin, d);
  }
  return dMin;
};

export const shapeDistancePolys = (pts1: ad.Pt2[], pts2: ad.Pt2[]): ad.Num =>
  overlappingPolygonPoints(pts1, pts2, 0);

export const shapeDistanceRectCircle = (
  rect: ad.Pt2[],
  c: ad.Pt2,
  r: ad.Num,
): ad.Num => {
  if (rect.length !== 4) {
    throw new Error("Expects rect to have four points");
  }
  const bbox = BBox.bboxFromPoints(rect);
  const halfW = div(bbox.width, 2);
  const halfH = div(bbox.height, 2);
  const [cx1, cy1] = bbox.center;
  const [cx2, cy2] = c;
  const x = sub(cx1, cx2);
  const y = sub(cy1, cy2);
  const bottomLeft: ad.Pt2 = [sub(x, halfW), sub(y, halfH)];
  const topRight: ad.Pt2 = [add(x, halfW), add(y, halfH)];
  return sub(rectangleSignedDistance(bottomLeft, topRight), r);
};

export const shapeDistancePolyEllipse = (
  pts: ad.Pt2[],
  c: ad.Pt2,
  rx: ad.Num,
  ry: ad.Num,
): ad.Num => {
  const cp = convexPartitions(pts);
  return minN(cp.map((p) => overlappingPolygonPointsEllipse(p, c, rx, ry, 0)));
};

export const shapeDistanceCircleLine = (
  c: ad.Pt2,
  r: ad.Num,
  start: ad.Pt2,
  end: ad.Pt2,
): ad.Num => {
  const [a, b] = [start, end];
  // Return the distance between the circle center c and the
  // segment ab, minus the circle radius r and offset o.  This
  // quantity will be negative of the circular disk intersects
  // a thickened "capsule" associated with the line (of radius o).
  // The expression for the point-segment distance d comes from
  // https://iquilezles.org/www/articles/distfunctions2d/distfunctions2d.htm
  // (see "Segment - exact").
  const u = ops.vsub(c, a); // u = c-a
  const v = ops.vsub(b, a); // v - b-a
  // h = clamp( <u,v>/<v,v>, 0, 1 )
  const h = max(0, min(1, div(ops.vdot(u, v), ops.vdot(v, v))));
  // d = | u - h*v |
  const d = ops.vnorm(ops.vsub(u, ops.vmul(h, v)));
  // return d - (r+o)
  return sub(d, r);
};

const shapeDistanceCirclePolyline = (
  c: ad.Num[],
  r: ad.Num,
  points: ad.Num[][],
): ad.Num => {
  // compute the smallest distance to any segment
  let dMin: ad.Num = Infinity;
  for (let i = 0; i < points.length - 1; i++) {
    const a = points[i];
    const b = points[i + 1];
    const d = shapeDistanceCircleLine(toPt(c), r, toPt(a), toPt(b));
    dMin = min(dMin, d);
  }
  return dMin;
};

export const shapeDistanceLines = (
  start1: ad.Pt2,
  end1: ad.Pt2,
  start2: ad.Pt2,
  end2: ad.Pt2,
): ad.Num => {
  // line endpoints
  const a0 = start1;
  const a1 = end1;
  const b0 = start2;
  const b1 = end2;

  // vertices of Minkowski difference polygon
  const p0 = ops.vsub(a0, b0);
  const p1 = ops.vsub(a0, b1);
  const p2 = ops.vsub(a1, b1);
  const p3 = ops.vsub(a1, b0);

  // how far is Minkowski polygon from containing the origin?
  return signedDistancePolygon(
    [toPt(p0), toPt(p1), toPt(p2), toPt(p3)],
    [0, 0],
  );
};

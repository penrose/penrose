import { ops } from "../engine/Autodiff";
import {
  add,
  div,
  max,
  min,
  minN,
  mul,
  neg,
  sqrt,
  sub,
} from "../engine/AutodiffFunctions";
import * as BBox from "../engine/BBox";
import { Circle } from "../shapes/Circle";
import { Ellipse } from "../shapes/Ellipse";
import { Line } from "../shapes/Line";
import { Shape, shapedefs } from "../shapes/Shapes";
import * as ad from "../types/ad";
import { msign } from "./Functions";
import {
  convexPartitions,
  overlappingPolygonPoints,
  overlappingPolygonPointsEllipse,
  rectangleDifference,
  rectangleSignedDistance,
} from "./Minkowski";
import { isPolygonlike, isRectlike, Polygonlike, Rectlike } from "./Utils";

/**
 * Return bounding box from any provided shape.
 */
export const bboxFromShape = ([t, s]: [string, any]): BBox.BBox => {
  return shapedefs[t].bbox(s);
};

/**
 * Return center of the shape `shape`.
 * For shapes without the property `center`, the center of their bounding box is returned.
 */
export const shapeCenter = ([t, s]: [string, any]): ad.Pt2 => {
  if ("center" in s) {
    return s.center.contents;
  } else {
    // Return center of bounding box
    const bbox = bboxFromShape([t, s]);
    return bbox.center;
  }
};

/**
 * Return size of the shape `shape`.
 * - `radius` for circles.
 * - `sqrt( w * h )`, where `w` and `h` are the width and height of the bounding box, for all other shapes.
 */
export const shapeSize = ([t, s]: [string, any]): ad.Num => {
  if (t === "Circle") {
    return mul(2, s.r.contents);
  } else {
    const bbox = bboxFromShape([t, s]);
    return sqrt(mul(bbox.width, bbox.height));
  }
};

/**
 * Return vertices of polygon-like shapes.
 */
export const polygonLikePoints = ([t, s]: [string, any]): ad.Pt2[] => {
  if (t === "Polygon") return s.points.contents;
  else if (shapedefs[t].isLinelike) return [s.start.contents, s.end.contents];
  else if (shapedefs[t].isRectlike) {
    // TODO: add support for rotated rectangles
    const bbox = bboxFromShape([t, s]);
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
  insidePoint: ad.Num[]
): ad.Num[] => {
  const normal = ops.vnormalize(
    ops.rot90(ops.vsub(lineSegment[1], lineSegment[0]))
  );
  const insideValue = ops.vdot(ops.vsub(insidePoint, lineSegment[0]), normal);
  return ops.vmul(neg(msign(insideValue)), normal);
};

export const rectLineDist = (
  rect: { bottomLeft: ad.Pt2; topRight: ad.Pt2 },
  line: { start: ad.Pt2; end: ad.Pt2 }
): ad.Num => {
  throw Error("TODO");
};

export const shapeDistance = (s1: Shape, s2: Shape): ad.Num => {
  const t1 = s1.shapeType;
  const t2 = s2.shapeType;
  // Same shapes
  if (t1 === "Circle" && t2 === "Circle") return shapeDistanceCircles(s1, s2);
  else if (isRectlike(s1) && isRectlike(s2))
    return shapeDistanceRectlikes(s1, s2);
  // HACK: text/label-line, mainly to skip convex partitioning
  else if ((t1 === "Text" || t1 === "Equation") && t2 === "Line")
    return shapeDistanceRectlikeLine(s1, s2);
  else if (t1 === "Line" && (t2 === "Text" || t2 === "Equation"))
    return shapeDistanceRectlikeLine(s2, s1);
  else if (isPolygonlike(s1) && isPolygonlike(s2))
    return shapeDistancePolygonlikes(s1, s2);
  // Rectangle x Circle
  else if (isRectlike(s1) && t2 === "Circle")
    return shapeDistanceRectlikeCircle(s1, s2);
  else if (t1 === "Circle" && isRectlike(s2))
    return shapeDistanceRectlikeCircle(s2, s1);
  // Polygon x Ellipse
  else if (isPolygonlike(s1) && t2 === "Ellipse")
    return shapeDistancePolygonlikeEllipse(s1, s2);
  else if (t1 === "Ellipse" && isPolygonlike(s2))
    return shapeDistancePolygonlikeEllipse(s2, s1);
  // Circle x Line
  else if (t1 === "Circle" && t2 === "Line")
    return shapeDistanceCircleLine(s1, s2);
  else if (t1 === "Line" && t2 === "Circle")
    return shapeDistanceCircleLine(s2, s1);
  // Default to axis-aligned bounding boxes
  else return shapeDistanceAABBs(s1, s2);
};

const shapeDistanceCircles = (s1: Circle, s2: Circle): ad.Num =>
  sub(
    ops.vdist(s1.center.contents, s2.center.contents),
    add(s1.r.contents, s2.r.contents)
  );

const shapeDistanceRectlikes = (s1: Rectlike, s2: Rectlike): ad.Num =>
  shapeDistanceAABBs(s1, s2);

const shapeDistanceRectlikeLine = (s1: Rectlike, s2: Line): ad.Num => {
  const start = s2.start.contents;
  const end = s2.end.contents;
  // https://github.com/penrose/penrose/issues/715
  if (!ad.isPt2(start)) {
    throw new Error(
      `shapeDistance expected start to be Pt2, but got length ${start.length}`
    );
  }
  if (!ad.isPt2(end)) {
    throw new Error(
      `shapeDistance expected end to be Pt2, but got length ${end.length}`
    );
  }

  const halfW = div(s1.width.contents, 2);
  const halfH = div(s1.height.contents, 2);
  const [cx, cy] = s1.center.contents;
  return rectLineDist(
    {
      bottomLeft: [sub(cx, halfW), sub(cy, halfH)],
      topRight: [add(cx, halfW), add(cy, halfH)],
    },
    { start, end }
  );
};

export const shapeDistancePolygonlikes = (
  s1: Polygonlike,
  s2: Polygonlike
): ad.Num =>
  overlappingPolygonPoints(
    polygonLikePoints([s1.shapeType, s1]),
    polygonLikePoints([s2.shapeType, s2]),
    0
  );

const shapeDistanceRectlikeCircle = (s1: Rectlike, s2: Circle): ad.Num => {
  const halfW = div(s1.width.contents, 2);
  const halfH = div(s1.height.contents, 2);
  const [cx1, cy1] = s1.center.contents;
  const [cx2, cy2] = s2.center.contents;
  const x = sub(cx1, cx2);
  const y = sub(cy1, cy2);
  const bottomLeft: ad.Pt2 = [sub(x, halfW), sub(y, halfH)];
  const topRight: ad.Pt2 = [add(x, halfW), add(y, halfH)];
  return sub(rectangleSignedDistance(bottomLeft, topRight), s2.r.contents);
};

const shapeDistancePolygonlikeEllipse = (
  s1: Polygonlike,
  s2: Ellipse
): ad.Num => {
  const points = polygonLikePoints([s1.shapeType, s1]);
  const cp = convexPartitions(points);
  return minN(cp.map((p) => overlappingPolygonPointsEllipse(p, s2, 0)));
};

const shapeDistanceCircleLine = (s1: Circle, s2: Line): ad.Num => {
  // collect constants
  const c = s1.center.contents;
  const r = s1.r.contents;
  const a = s2.start.contents;
  const b = s2.end.contents;

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

export const shapeDistanceAABBs = (s1: Shape, s2: Shape): ad.Num => {
  // Prepare axis-aligned bounding boxes
  const box1 = bboxFromShape([s1.shapeType, s1]);
  const box2 = bboxFromShape([s2.shapeType, s2]);
  // Get the Minkowski difference rectangle
  const [bottomLeft, topRight] = rectangleDifference(box1, box2, 0);
  // Return the signed distance
  return rectangleSignedDistance(bottomLeft, topRight);
};

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
  squared,
  sub,
} from "../engine/AutodiffFunctions";
import * as BBox from "../engine/BBox";
import { Circle } from "../shapes/Circle";
import { Ellipse } from "../shapes/Ellipse";
import { Line } from "../shapes/Line";
import { computeShapeBbox, Shape } from "../shapes/Shapes";
import * as ad from "../types/ad";
import { msign } from "./Functions";
import {
  convexPartitions,
  overlappingPolygonPoints,
  overlappingPolygonPointsEllipse,
  rectangleDifference,
  rectangleSignedDistance,
} from "./Minkowski";
import {
  isLinelike,
  isPolygonlike,
  isRectlike,
  Polygonlike,
  Rectlike,
} from "./Utils";

/**
 * Return bounding box from any provided shape.
 */
export const bboxFromShape = (shape: Shape<ad.Num>): BBox.BBox => {
  return computeShapeBbox(shape);
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
 * Return size of the shape `shape`.
 * - `radius` for circles.
 * - `sqrt( w * h )`, where `w` and `h` are the width and height of the bounding box, for all other shapes.
 */
export const shapeSize = (s: Shape<ad.Num>): ad.Num => {
  if (s.shapeType === "Circle") {
    return mul(2, s.r.contents);
  } else {
    const bbox = bboxFromShape(s);
    return sqrt(mul(bbox.width, bbox.height));
  }
};

/**
 * Return vertices of polygon-like shapes.
 */
export const polygonLikePoints = (s: Shape<ad.Num>): ad.Pt2[] => {
  const t = s.shapeType;
  if (t === "Polygon")
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
  insidePoint: ad.Num[]
): ad.Num[] => {
  const normal = ops.vnormalize(
    ops.rot90(ops.vsub(lineSegment[1], lineSegment[0]))
  );
  const insideValue = ops.vdot(ops.vsub(insidePoint, lineSegment[0]), normal);
  return ops.vmul(neg(msign(insideValue)), normal);
};

export const shapeDistance = (s1: Shape<ad.Num>, s2: Shape<ad.Num>): ad.Num => {
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

const shapeDistanceCircles = (s1: Circle<ad.Num>, s2: Circle<ad.Num>): ad.Num =>
  sub(
    ops.vdist(s1.center.contents, s2.center.contents),
    add(s1.r.contents, s2.r.contents)
  );

const shapeDistanceRectlikes = (
  s1: Rectlike<ad.Num>,
  s2: Rectlike<ad.Num>
): ad.Num => shapeDistanceAABBs(s1, s2);

const shapeDistanceRectlikeLine = (
  s1: Rectlike<ad.Num>,
  s2: Line<ad.Num>
): ad.Num => {
  // TODO: temporary bounding circle based solution
  // collect constants
  const rect = bboxFromShape(s1);
  const c = s1.center.contents;
  const a = s2.start.contents;
  const b = s2.end.contents;
  const r = sqrt(
    add(squared(div(rect.width, 2)), squared(div(rect.height, 2)))
  );
  const u = ops.vsub(c, a); // u = c-a
  const v = ops.vsub(b, a); // v - b-a
  // h = clamp( <u,v>/<v,v>, 0, 1 )
  const h = max(0, min(1, div(ops.vdot(u, v), ops.vdot(v, v))));
  // d = | u - h*v |
  const d = ops.vnorm(ops.vsub(u, ops.vmul(h, v)));
  // return d - (r+o)
  return sub(d, r);

  // TODO: fix minkowski SDF and use this instead
  // const { topLeft, topRight, bottomLeft, bottomRight } = BBox.corners(
  //   bboxFromShape(t)
  // );
  // const p1: ad.Num[][] = [topLeft, topRight, bottomLeft, bottomRight];
  // const p2: ad.Num[][] = [s2.start.contents, s2.end.contents];
  // return convexPolygonMinkowskiSDF(p1, p2, padding);
};

export const shapeDistancePolygonlikes = (
  s1: Polygonlike<ad.Num>,
  s2: Polygonlike<ad.Num>
): ad.Num =>
  overlappingPolygonPoints(polygonLikePoints(s1), polygonLikePoints(s2), 0);

const shapeDistanceRectlikeCircle = (
  s1: Rectlike<ad.Num>,
  s2: Circle<ad.Num>
): ad.Num => {
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
  s1: Polygonlike<ad.Num>,
  s2: Ellipse<ad.Num>
): ad.Num => {
  const points = polygonLikePoints(s1);
  const cp = convexPartitions(points);
  return minN(cp.map((p) => overlappingPolygonPointsEllipse(p, s2, 0)));
};

const shapeDistanceCircleLine = (
  s1: Circle<ad.Num>,
  s2: Line<ad.Num>
): ad.Num => {
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

export const shapeDistanceAABBs = (
  s1: Shape<ad.Num>,
  s2: Shape<ad.Num>
): ad.Num => {
  // Prepare axis-aligned bounding boxes
  const box1 = bboxFromShape(s1);
  const box2 = bboxFromShape(s2);
  // Get the Minkowski difference rectangle
  const [bottomLeft, topRight] = rectangleDifference(box1, box2, 0);
  // Return the signed distance
  return rectangleSignedDistance(bottomLeft, topRight);
};

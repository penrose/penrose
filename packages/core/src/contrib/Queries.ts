import { ops } from "../engine/Autodiff";
import { add, mul, neg, sqrt, sub } from "../engine/AutodiffFunctions";
import * as BBox from "../engine/BBox";
import { Circle } from "../shapes/Circle";
import { Ellipse } from "../shapes/Ellipse";
import { Line } from "../shapes/Line";
import { Shape, shapedefs } from "../shapes/Shapes";
import * as ad from "../types/ad";
import { msign } from "./Functions";
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
  else if (t1 === "Ellipse" && t2 === "Ellipse")
    return shapeDistanceEllipses(s1, s2);
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
  // Circle x Ellipse
  else if (t1 === "Circle" && t2 === "Ellipse")
    return shapeDistanceCircleEllipse(s1, s2);
  else if (t1 === "Ellipse" && t2 === "Circle")
    return shapeDistanceCircleEllipse(s2, s1);
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

const shapeDistanceRectlikes = (s1: Rectlike, s2: Rectlike): ad.Num => {
  throw Error();
};

const shapeDistanceRectlikeLine = (s1: Rectlike, s2: Line): ad.Num => {
  throw Error();
};

const shapeDistancePolygonlikes = (
  s1: Polygonlike,
  s2: Polygonlike
): ad.Num => {
  throw Error();
};

const shapeDistanceEllipses = (s1: Ellipse, s2: Ellipse): ad.Num => {
  throw Error();
};

const shapeDistanceRectlikeCircle = (s1: Rectlike, s2: Circle): ad.Num => {
  throw Error();
};

const shapeDistancePolygonlikeEllipse = (
  s1: Polygonlike,
  s2: Ellipse
): ad.Num => {
  throw Error();
};

const shapeDistanceCircleEllipse = (s1: Circle, s2: Ellipse): ad.Num => {
  throw Error();
};

const shapeDistanceCircleLine = (s1: Circle, s2: Line): ad.Num => {
  throw Error();
};

const shapeDistanceAABBs = (s1: Shape, s2: Shape): ad.Num => {
  throw Error();
};

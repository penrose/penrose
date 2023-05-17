import { ifCond } from "../engine/AutodiffFunctions";
import { Shape } from "../shapes/Shapes";
import * as ad from "../types/ad";
import { bboxFromShape, shapeCenter } from "./Queries";
import {
  Rectlike,
  atDistOutside,
  isLinelike,
  noIntersectCircles,
  pointInBox,
} from "./Utils";

// -------- Ovelapping helpers

/**
 * Require that shape `s1` is at a distance of `distance` from shape `s2`.
 */
export const atDistLabel = (
  s1: Shape<ad.Num>,
  s2: Shape<ad.Num>,
  distance: ad.Num
): ad.Num => {
  let pt;
  if (isLinelike(s1)) {
    // Position label close to the arrow's end
    pt = s1.end.contents;
  } else {
    // Only assume shape1 has a center
    pt = shapeCenter(s1);
  }

  // Get bounding box
  const rect = bboxFromShape(s2);

  return ifCond(
    pointInBox([pt[0], pt[1]], rect),
    // If the point is inside the box, push it outside w/ `noIntersect`
    noIntersectCircles(rect.center, rect.width, pt, 2),
    // If the point is outside the box, try to get the distance from the point to equal the desired distance
    atDistOutside([pt[0], pt[1]], rect, distance)
  );
};

// -------- Contains helpers

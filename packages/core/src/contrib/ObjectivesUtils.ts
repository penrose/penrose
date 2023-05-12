import { ops } from "../engine/Autodiff";
import { squared, sub } from "../engine/AutodiffFunctions";
import { Shape } from "../shapes/Shapes";
import * as ad from "../types/ad";
import { shapeCenter } from "./Queries";

/**
 * Encourage the center of the shape `shape` to be in the direction `direction` with respect to shape `shapeRef`.
 * For shapes without the property `center`, the center of their bounding box is used.
 */
export const inDirection = (
  shape: Shape<ad.Num>,
  shapeRef: Shape<ad.Num>,
  unitDirectionVector: ad.Pt2,
  offset: ad.Num
): ad.Num => {
  const center = shapeCenter(shape);
  const centerRef = shapeCenter(shapeRef);
  const dotProduct = ops.vdot(ops.vsub(center, centerRef), unitDirectionVector);
  return squared(sub(dotProduct, offset));
};

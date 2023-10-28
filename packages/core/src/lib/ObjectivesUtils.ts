import { ops } from "../engine/Autodiff.js";
import { squared, sub } from "../engine/AutodiffFunctions.js";
import * as ad from "../types/ad.js";

/**
 * Encourage the point `p` to be in the direction `direction` with respect to point `pRef`.
 * The `direction` vector does not need to be normalized.
 * The `offset` parameter is the distance between the points.
 */
export const inDirection = (
  p: ad.Pt2,
  pRef: ad.Pt2,
  direction: ad.Pt2,
  offset: ad.Num,
): ad.Num => {
  const unitDirectionVector = ops.vnormalize(direction);
  const dotProduct = ops.vdot(ops.vsub(p, pRef), unitDirectionVector);
  return squared(sub(dotProduct, offset));
};

import { shapeCenter } from "contrib/Queries";
import { ops } from "engine/Autodiff";
import { squared, sub } from "engine/AutodiffFunctions";
import { Pt2, VarAD } from "types/ad";

/**
 * Encourage the center of the shape `shape` to be in the direction `direction` with respect to shape `shapeRef`.
 * For shapes without the property `center`, the center of their bounding box is used.
 */
export const inDirection = (
  [t, shape]: [string, any],
  [tRef, shapeRef]: [string, any],
  unitDirectionVector: Pt2,
  offset: VarAD
): VarAD => {
  const center = shapeCenter([t, shape]);
  const centerRef = shapeCenter([tRef, shapeRef]);
  const dotProduct = ops.vdot(ops.vsub(center, centerRef), unitDirectionVector);
  return squared(sub(dotProduct, offset));
};

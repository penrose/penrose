import { constOf, squared, sub, varOf, max, ops} from "engine/Autodiff";
import { shapeCenter } from "contrib/Queries";
import { Pt2, VarAD } from "types/ad";

// -------- New helpers

/**
 * Encourage the center of the shape `shape` to be in the direction `direction` with respect to shape `shapeRef`.
 * For shapes without the property `center`, the center of their bounding box is used.
 */
export const inDirection = (
  [t, shape]: [string, any],
  [tRef, shapeRef]: [string, any],
  direction: Pt2,
  offset: number,
  normalizeDirection: boolean,
): VarAD => {
  const center = shapeCenter([t, shape]);
  const centerRef = shapeCenter([tRef, shapeRef]);
  var dotProduct;
  if (normalizeDirection) {
    dotProduct = ops.vdot(ops.vsub(center, centerRef), ops.vnormalize(direction));
  }
  else {
    dotProduct = ops.vdot(ops.vsub(center, centerRef), direction);
  }
  return squared(max(constOf(0.0), sub(dotProduct, varOf(offset))));
};

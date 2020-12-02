import { Tensor, stack, scalar, maximum, norm, abs, square, squaredDifference } from "@tensorflow/tfjs";
import { canvasSize } from "./Canvas";
import * as _ from "lodash";

const TOL = 1e-4;
const DEBUG_ENERGY = false;

export const objDict = {
    equal: (x: Tensor, y: Tensor): Tensor => squaredDifference(x, y),

  above: ([t1, top]: [string, any], [t2, bottom]: [string, any], offset = 100): Tensor =>
    // (getY top - getY bottom - offset) ^ 2
    square(sub(sub(top.y.contents, bottom.y.contents), scalar(offset))),

  sameCenter: ([t1, s1]: [string, any], [t2, s2]: [string, any]): Tensor => {
    return distsq(center(s1), center(s2));
  },

  nearHead: ([t1, s1]: [string, any], [t2, s2]: [string, any]): Tensor => {
    return distsq(center(s1), stack([add(s2.endX.contents, 10.0), add(s2.endY.contents, 10.0)]));
  },
  // distsq (getX lab, getY lab) (end `plus2` offset)

  below: ([t1, bottom]: [string, any], [t2, top]: [string, any], offset = 100): Tensor => 
    square(sub(sub(top.y.contents, bottom.y.contents), scalar(offset))),
    // can this be made more efficient (code-wise) by calling "above" and swapping arguments? - stella


  centerLabel: ([t1, arr]: [string, any], [t2, text1]: [string, any], w: number): Tensor => {
    if (typesAre([t1,t2], ["Arrow", "Text"])) {
      const mx = div(add(arr.startX.contents, arr.endX.contents), scalar(2.0));
      const my = div(add(arr.startY.contents, arr.endY.contents), scalar(2.0));
      // entire equation is (mx - lx) ^ 2 + (my + 1.1 * text.h - ly) ^ 2 from Functions.hs - split it into two halves below for readability
      const lh = sub(mx, text1.x.contents).square();
      const rh = sub(add(my, mul(text1.h.contents, scalar(1.1))), text1.y.contents).square();
      return mul(add(lh, rh), w);
    } else throw new Error(`${[t1, t2]} not supported for centerLabel`)
  },

  // Generic repel function for two GPIs with centers
  repel: ([t1, s1]: [string, any], [t2, s2]: [string, any]): Tensor => {
    // HACK: `repel` typically needs to have a weight multiplied since its magnitude is small
    // TODO: find this out programmatically
    const repelWeight = 10e6;
    // 1 / (d^2(cx, cy) + eps)
    return mul(add(distsq(center(s1), center(s2)), epsd).reciprocal(), repelWeight);
  }
}
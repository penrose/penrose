import { canvasSize } from "./Canvas";
import * as _ from "lodash";

const TOL = 1e-4;
const DEBUG_ENERGY = false;

export const objDict = {
  // (x - y)^2
  // mtrans
  equal: (x: number, y: number) => squared(x - y),

  // mtrans
  above: ([t1, top]: [string, any], [t2, bottom]: [string, any], offset = 100) =>
    // (getY top - getY bottom - offset) ^ 2
    squared(top.y.contents - bottom.y.contents - varOf(offset)),

  // mtrans
  sameCenter: ([t1, s1]: [string, any], [t2, s2]: [string, any]) =>
    ops.vdistsq(fns.center(s1), fns.center(s2)),

  // mtrans
  repel: ([t1, s1]: [string, any], [t2, s2]: [string, any], weight = 10.0) => {
    // HACK: `repel` typically needs to have a weight multiplied since its magnitude is small
    // TODO: find this out programmatically
    const repelWeight = 10e6;

    let res;

    if (t1 === "Line") {
      const line = s1;
      const c2 = fns.center(s2);
      const lineSamplePts = sampleSeg(linePts(line));
      const allForces = addN(lineSamplePts.map(p => repelPt(constOfIf(weight), c2, p)));
      // TODO: fix all the repel weights being zeroed for some reason
      res = constOfIf(weight) * allForces;
    } else {
      // 1 / (d^2(cx, cy) + eps)
      res = inverse(ops.vdistsq(fns.center(s1), fns.center(s2)));
    }

    return res * constOf(repelWeight);
  },
 
  // mtrans
  centerArrow: ([t1, arr]: [string, any], [t2, text1]: [string, any], [t3, text2]: [string, any]): number => {
    const spacing = varOf(1.1); // arbitrary

    if (typesAre([t1, t2, t3], ["Arrow", "Text", "Text"])) {
      // HACK: Arbitrarily pick the height of the text
      // [spacing * getNum text1 "h", negate $ 2 * spacing * getNum text2 "h"]
      return centerArrow2(arr, fns.center(text1), fns.center(text2),
        [spacing * (text1.h.contents),
        -(text2.h.contents * spacing)]);

    } else throw new Error(`${[t1, t2, t3]} not supported for centerArrow`);
  },

  // can this be made more efficient (code-wise) by calling "above" and swapping arguments? - stella
  // mtrans
  below: ([t1, bottom]: [string, any], [t2, top]: [string, any], offset = 100) =>
    squared(top.y.contents - bottom.y.contents - constOfIf(offset)),

    // mtrans
  centerLabel: ([t1, arr]: [string, any], [t2, text1]: [string, any], w: number): number => {
    if (typesAre([t1, t2], ["Arrow", "Text"])) {
      const mx = (arr.startX.contents + arr.endX.contents) / constOf(2.0);
      const my = (arr.startY.contents + arr.endY.contents) / constOf(2.0);

      // entire equation is (mx - lx) ^ 2 + (my + 1.1 * text.h - ly) ^ 2 from Functions.hs - split it into two halves below for readability
      const lh = squared(mx - text1.x.contents);
      const rh = squared(my + text1.h.contents * constOf(1.1) - text1.y.contents);
      return (lh + rh) * constOfIf(w);
    } else throw new Error(`${[t1, t2]} not supported for centerLabel`)
  },
  // mtrans
  negTest: ([t1, s1]: [string, any]): number => -s1,

};
// mtrans
const looseIntersect = (center1: number[], r1: number, center2: number[], r2: number, padding: number): number => {
  const res = r1 + r2 - padding;
  return ops.vdist(center1, center2) - res;
};
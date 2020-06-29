import { Tensor, Scalar, Rank, stack, scalar, maximum, tensor, norm, abs, square, squaredDifference } from "@tensorflow/tfjs";
import { canvasSize } from "./Canvas";
import * as _ from "lodash";

export const objDict = {
  equal: (x: Tensor, y: Tensor) => squaredDifference(x, y),

  above: ([t1, top]: [string, any], [t2, bottom]: [string, any], offset = 100) =>
    // (getY top - getY bottom - offset) ^ 2
    square(top.y.contents.sub(bottom.y.contents).sub(scalar(offset))),

  sameCenter: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    return distsq(center(s1), center(s2));
  },

  nearHead: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    return distsq(center(s1), stack([s2.endX.contents.add(10.0), s2.endY.contents.add(10.0)]));
  },
  // distsq (getX lab, getY lab) (end `plus2` offset)

  // Stella function for testing (TODO: Replace w/ most recent version)
  centerLabel: ([t1, arr]: [string, any], [t2, text1]: [string, any], w: number): Tensor => {

    // The tensors seem to have different disposed values, but their numeric values all seem to be available, so this is fine?

    if (typesAre([t1, t2], ["Arrow", "Text"])) {
      const mx = arr.startX.contents.add(arr.endX.contents).div(scalar(2.0));
      const my = arr.startY.contents.add(arr.endY.contents).div(scalar(2.0));
      // entire equation is (mx - lx) ^ 2 + (my + 1.1 * text.h - ly) ^ 2 from Functions.hs - split it into two halves below for readability
      const lh = mx.sub(text1.x.contents).square();
      const rh = my.add(text1.h.contents.mul(scalar(1.1))).sub(text1.y.contents).square();
      return lh.add(rh).mul(w);
    } else throw new Error(`${[t1, t2]} not supported for centerLabel`)
  },

  // Generic repel function for two GPIs with centers
  repel: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    // HACK: `repel` typically needs to have a weight multiplied since its magnitude is small
    // TODO: find this out programmatically
    const repelWeight = 10e6;
    // 1 / (d^2(cx, cy) + eps)
    return distsq(center(s1), center(s2)).add(epsd).reciprocal().mul(repelWeight);
  },

  centerArrow: ([t1, arr]: [string, any], [t2, text1]: [string, any], [t3, text2]: [string, any]): Tensor => {
    const spacing = scalar(1.1); // arbitrary

    if (typesAre([t1, t2, t3], ["Arrow", "Text", "Text"])) {
      // HACK: Arbitrarily pick the height of the text
      // [spacing * getNum text1 "h", negate $ 2 * spacing * getNum text2 "h"]
      return centerArrow2(arr, center(text1), center(text2),
        [spacing.mul(text1.h.contents),
        text2.h.contents.mul(spacing).mul(scalar(1.0)).neg()]);
    } else throw new Error(`${[t1, t2, t3]} not supported for centerArrow`);
  },

};

export const constrDict = {
  maxSize: ([shapeType, props]: [string, any]) => {
    const limit = scalar(Math.max(...canvasSize) / 6);
    switch (shapeType) {
      case "Circle":
        return stack([props.r.contents, limit.neg()]).sum();
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a maxSize`);
    }
  },

  minSize: ([shapeType, props]: [string, any]) => {
    const limit = scalar(20);
    switch (shapeType) {
      case "Circle":
        return stack([limit, props.r.contents.neg()]).sum();
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a minSize`);
    }
  },

  at: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    return squaredDifference(s1.x.contents, s2.x.contents).add(squaredDifference(s1.y.contents, s2.y.contents))
  },

  sameHeight: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    return squaredDifference(s1.h.contents, s2.h.contents);
  },

  contains: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    offset: Tensor
  ) => {
    if (t1 === "Circle" && t2 === "Circle") {
      const d = dist(center(s1), center(s2));
      // const o = s1.r.contents.sub(s2.r.contents);
      const o = offset
        ? s1.r.contents.sub(s2.r.contents).sub(offset)
        : s1.r.contents.sub(s2.r.contents);
      return d.sub(o);
    } else if (t1 === "Circle" && t2 === "Text") {
      const d = dist(center(s1), center(s2));
      const textR = maximum(s2.w.contents, s2.h.contents);
      return d.sub(s1.r.contents).add(textR);
    } else {
      console.error(`${[t1, t2]} not supported for contains`);
      return scalar(0.0);

      // TODO revert
      // throw new Error(`${[t1, t2]} not supported for contains`);
    }
  },

  disjoint: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    if (t1 === "Circle" && t2 === "Circle") {
      const d = dist(center(s1), center(s2));
      const o = stack([s1.r.contents, s2.r.contents, 10]);
      return o.sum().sub(d);
    } else throw new Error(`${[t1, t2]} not supported for disjoint`);
  },

  smallerThan: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    // s1 is smaller than s2
    const offset = scalar(0.4).mul(s2.r.contents); // take 0.4 as param
    return s1.r.contents.sub(s2.r.contents).sub(offset);
  },

  outsideOf: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding = 10
  ) => {
    if (t1 === "Text" && t2 === "Circle") {
      const textR = maximum(s1.w.contents, s1.h.contents);
      const d = dist(center(s1), center(s2));
      return s2.r.contents
        .add(textR)
        .add(padding)
        .sub(d);
    } else throw new Error(`${[t1, t2]} not supported for outsideOf`);
  },

  overlapping: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding = 10
  ) => {
    if (t1 === "Circle" && t2 === "Circle") {
      return looseIntersect(center(s1), s1.r.contents,
        center(s2), s2.r.contents, padding);
    } else throw new Error(`${[t1, t2]} not supported for overlapping`);
  },

};

// -------- Helpers for writing objectives

const typesAre = (inputs: string[], expected: string[]) =>
  (inputs.length === expected.length) && _.zip(inputs, expected).map(([i, e]) => i === e);

// -------- (Hidden) helpers for objective/constraints/computations

const centerArrow2 = (arr: any, center1: Tensor, center2: Tensor, [o1, o2]: Tensor[]): Tensor => {
  const vec = center2.sub(center1); // direction the arrow should point to
  const dir = normalize(vec);

  let start = center1;
  let end = center2;

  // TODO: take in spacing, use the right text dimension/distance?, note on arrow directionality
  // won't this always return true? greater returns an array of values
  if (norm(vec).greater(o1.add(abs(o2)))) {
    start = center1.add(o1.mul(dir));
    end = center2.add(o2.mul(dir));
  }

  const fromPt = stack([arr.startX.contents, arr.startY.contents]);
  const toPt = stack([arr.endX.contents, arr.endY.contents]);

  return distsq(fromPt, start).add(distsq(toPt, end));
}


// -------- Utils for objective/constraints/computations

const sc = (x: any): number => x.dataSync()[0];
const scs = (xs: any[]) => xs.map((e) => sc(e));

export const zero: Tensor = scalar(0);

// to prevent 1/0 (infinity). put it in the denominator
export const epsd: Tensor = scalar(10e-10);

export const looseIntersect = (center1: Tensor, r1: Tensor, center2: Tensor, r2: Tensor, padding: number) =>
  dist(center1, center2).sub(r1.add(r2).sub(scalar(padding)));
// dist (x1, y1) (x2, y2) - (s1 + s2 - 10)

export const center = (props: any): Tensor =>
  stack([props.x.contents, props.y.contents]); // HACK: need to annotate the types of x and y to be Tensor

export const dist = (p1: Tensor, p2: Tensor): Tensor => p1.sub(p2).norm();

// Be careful not to use element-wise operations. This should return a scalar.
// Apparently typescript can't check a return type of `Tensor<Rank.R0>`?
export const distsq = (p1: Tensor, p2: Tensor): Tensor => {
  const dp = p1.sub(p2);
  return dp.dot(dp);
}

// with epsilon to avoid NaNs
export const normalize = (v: Tensor): Tensor => v.div(v.norm().add(epsd));

// TODO: use it
// const getConstraint = (name: string) => {
//   if (!constrDict[name]) throw new Error(`Constraint "${name}" not found`);
//   // TODO: types for args
//   return (...args: any[]) => toPenalty(constrDict[name]);
// };

import {
  varOf,
  constOf,
  add,
  addN,
  mul,
  sub,
  div,
  max,
  min,
  sin,
  cos,
  neg,
  squared,
  sqrt,
  inverse,
  absVal,
  gt,
  lt,
  ifCond,
  ops
} from "./Autodiff";

import { Tensor, stack, scalar, maximum, norm, abs, square, squaredDifference } from "@tensorflow/tfjs";

import { canvasSize } from "./Canvas";
import * as ad from "./Autodiff";
import * as _ from "lodash";

export const objDict = {
  // (x - y)^2
  equal: (x: VarAD, y: VarAD) => squared(sub(x, y)),

  // equalOld: (x: Tensor, y: Tensor) => squaredDifference(x, y),

  above: ([t1, top]: [string, any], [t2, bottom]: [string, any], offset = 100) =>
    // (getY top - getY bottom - offset) ^ 2
    squared(
      sub(sub(top.y.contents, bottom.y.contents),
        varOf(offset))),

  // aboveOld: ([t1, top]: [string, any], [t2, bottom]: [string, any], offset = 100) =>
  //     // (getY top - getY bottom - offset) ^ 2
  //     square(top.y.contents.sub(bottom.y.contents).sub(scalar(offset))),

  sameCenter: ([t1, s1]: [string, any], [t2, s2]: [string, any]) =>
    distsq(center(s1), center(s2)),

  repel: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    // HACK: `repel` typically needs to have a weight multiplied since its magnitude is small
    // TODO: find this out programmatically
    const repelWeight = 10e6;

    console.log("shapes", s1, s2);

    // TODO: this only works for shapes with a center (x,y)

    // 1 / (d^2(cx, cy) + eps)
    return mul(inverse(ops.vdistsq(centerList(s1), centerList(s2))), varOf(repelWeight));
  },

  atDist: ([t1, s1]: [string, any], [t2, s2]: [string, any], offset: any) => {
    // Place the latter at a distance from the center of the point
    // TODO: Account for the size/radius of the initial point, rather than just the center

    if (t2 === "Text") {
      // TODO: What type is the offset?

      // Get polygon of text (box)
      // TODO: Make this a GPI property
      // TODO: Port the matrix stuff in `textPolygonFn` / `textPolygonFn2` in Shapes.hs

      // If the point is inside the box, push it outside w/ `noIntersect`

      // If the point is outside the box, try to get the distance from the point to equal the desired distance

    } else {
      throw Error(`unsupported shapes for 'atDist': ${t1}, ${t2}`);
    }
  },

  // Generic repel function for two GPIs with centers
  // repelOld: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
  //     // HACK: `repel` typically needs to have a weight multiplied since its magnitude is small
  //     // TODO: find this out programmatically
  //     const repelWeight = 10e6;
  //     // 1 / (d^2(cx, cy) + eps)
  //     return distsq(center(s1), center(s2)).add(epsd2).reciprocal().mul(repelWeight);
  // },

  centerArrow: ([t1, arr]: [string, any], [t2, text1]: [string, any], [t3, text2]: [string, any]): DiffVar => {
    const spacing = varOf(1.1); // arbitrary

    if (typesAre([t1, t2, t3], ["Arrow", "Text", "Text"])) {
      // HACK: Arbitrarily pick the height of the text
      // [spacing * getNum text1 "h", negate $ 2 * spacing * getNum text2 "h"]
      return centerArrow2(arr, centerList(text1), centerList(text2),
        [mul(spacing, (text1.h.contents)),
        neg(mul(text2.h.contents, spacing))]);

    } else throw new Error(`${[t1, t2, t3]} not supported for centerArrow`);
  },

  below: ([t1, bottom]: [string, any], [t2, top]: [string, any], offset = 100) =>
    square(top.y.contents.sub(bottom.y.contents).sub(scalar(offset))),
  // can this be made more efficient (code-wise) by calling "above" and swapping arguments? - stella

  // centerArrowOld: ([t1, arr]: [string, any], [t2, text1]: [string, any], [t3, text2]: [string, any]): DiffVar => {
  //     const spacing = scalar(1.1); // arbitrary

  //     if (typesAre([t1, t2, t3], ["Arrow", "Text", "Text"])) {
  //         // HACK: Arbitrarily pick the height of the text
  //         // [spacing * getNum text1 "h", negate $ 2 * spacing * getNum text2 "h"]
  //         return centerArrow2Old(arr, center(text1), center(text2),
  //             [spacing.mul(text1.h.contents),
  //             text2.h.contents.mul(spacing).mul(scalar(1.0)).neg()]);
  //     } else throw new Error(`${[t1, t2, t3]} not supported for centerArrow`);
  // },

  centerLabel: ([t1, arr]: [string, any], [t2, text1]: [string, any], w: number): Tensor => {
    if (typesAre([t1, t2], ["Arrow", "Text"])) {
      const mx = arr.startX.contents.add(arr.endX.contents).div(scalar(2.0));
      const my = arr.startY.contents.add(arr.endY.contents).div(scalar(2.0));
      // entire equation is (mx - lx) ^ 2 + (my + 1.1 * text.h - ly) ^ 2 from Functions.hs - split it into two halves below for readability
      const lh = mx.sub(text1.x.contents).square();
      const rh = my.add(text1.h.contents.mul(scalar(1.1))).sub(text1.y.contents).square();
      return lh.add(rh).mul(w);
    } else throw new Error(`${[t1, t2]} not supported for centerLabel`)
  },

};

export const constrDict = {
  maxSize: ([shapeType, props]: [string, any]) => {
    const limit = Math.max(...canvasSize) / 6;
    switch (shapeType) {
      case "Circle":
        return sub(props.r.contents, varOf(limit));
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a maxSize`);
    }
  },

  // maxSizeOld: ([shapeType, props]: [string, any]) => {
  //     const limit = scalar(Math.max(...canvasSize) / 6);
  //     switch (shapeType) {
  //         case "Circle":
  //             return stack([props.r.contents, limit.neg()]).sum();
  //         default:
  //             // HACK: report errors systematically
  //             throw new Error(`${shapeType} doesn't have a maxSize`);
  //     }
  // },

  minSize: ([shapeType, props]: [string, any]) => {
    const limit = 20;
    switch (shapeType) {
      case "Circle":
        return sub(varOf(limit), props.r.contents);
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a minSize`);
    }
  },

  // minSizeOld: ([shapeType, props]: [string, any]) => {
  //     const limit = scalar(20);
  //     switch (shapeType) {
  //         case "Circle":
  //             return stack([limit, props.r.contents.neg()]).sum();
  //         default:
  //             // HACK: report errors systematically
  //             throw new Error(`${shapeType} doesn't have a minSize`);
  //     }
  // },

  // containsOld: (
  //     [t1, s1]: [string, any],
  //     [t2, s2]: [string, any],
  //     offset: DiffVar
  // ) => {
  //     if (t1 === "Circle" && t2 === "Circle") {
  //         const d = distOld(center(s1), center(s2));
  //         // const o = s1.r.contents.sub(s2.r.contents);
  //         const o = offset
  //             ? s1.r.contents.sub(s2.r.contents).sub(offset)
  //             : s1.r.contents.sub(s2.r.contents);
  //         return d.sub(o);
  //     } else if (t1 === "Circle" && t2 === "Text") {
  //         const d = distOld(center(s1), center(s2));
  //         const textR = maximum(s2.w.contents, s2.h.contents);
  //         return d.sub(s1.r.contents).add(textR);
  //     } else throw new Error(`${[t1, t2]} not supported for contains`);
  // },

  // TODO: Had to rename due to needing to match funciton names in backend
  contains: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    offset: DiffVar
  ) => {

    if (t1 === "Circle" && t2 === "Circle") {
      console.error("circle, circle", s1, s2, offset);

      const d = ops.vdist(centerList(s1), centerList(s2));
      const o = offset
        ? sub(sub(s1.r.contents, s2.r.contents), offset)
        : sub(s1.r.contents, s2.r.contents);
      const res = sub(d, o);
      // console.error("contains circle circle res", res, res.val);

      return res;

    } else if (t1 === "Circle" && t2 === "Text") {
      // Note: The print/debug output will be compiled out in the computational graph! (So it will not display)
      // Note: The shapes' properties are still floats, so each time it's used, it's compiled to a NEW var here
      // (TODO: One question is whether they should be shared root variables?)

      // The problem is that: when a GPI is passed in here, is one of its properties varying? If so, was it looked up from varyingMap? Looks like it gets looked up in varyingMap first; if so, then non-varying properties should be var-ified beforehand so the objective function doesn't have to deal with var-ifying constants
      // TODO: Check the gradients for 'contains' as well (automatically, e.g. manual diff?)
      const d = ops.vdist(centerList(s1), centerList(s2));
      const textR = max((s2.w.contents), s2.h.contents);
      const res = add(sub(d, s1.r.contents), textR);

      return res;
    } else if (t1 === "Rectangle" && t2 === "Circle") {
      // contains [GPI r@("Rectangle", _), GPI c@("Circle", _), Val (FloatV padding)] =
      // -- HACK: reusing test impl, revert later
      //    let r_l = min (getNum r "w") (getNum r "h") / 2
      //        diff = r_l - getNum c "r"
      //    in dist (getX r, getY r) (getX c, getY c) - diff + padding

      // TODO: `rL` is probably a hack for dimensions
      const rL = min(s1.w.contents, div(s1.h.contents, varOf(2.0)));
      const diff = sub(rL, s2.r.contents);
      const d = ops.vdist(centerList(s1), centerList(s2));
      return add(sub(d, diff), offset);

    } else if (t1 === "Rectangle" && t2 === "Text") {
      // contains [GPI r@("Rectangle", _), GPI l@("Text", _), Val (FloatV padding)] =
      // TODO: implement precisely, max (w, h)? How about diagonal case?
      // dist (getX l, getY l) (getX r, getY r) - getNum r "w" / 2 +
      //   getNum l "w" / 2 + padding

      const a1 = ops.vdist(centerList(s1), centerList(s2));
      const a2 = div(s1.w.contents, varOf(2.0));
      const a3 = div(s2.w.contents, varOf(2.0));
      return add(add(sub(a1, a2), a3), offset);

    } else throw new Error(`${[t1, t2]} not supported for contains2`);

  },

  disjoint: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    if (t1 === "Circle" && t2 === "Circle") {
      const d = ops.vdist(centerList(s1), centerList(s2));
      const o = [s1.r.contents, s2.r.contents, varOf(10.0)];
      return sub(addN(o), d);
    } else throw new Error(`${[t1, t2]} not supported for disjoint`);
  },

  // disjointOld: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
  //     if (t1 === "Circle" && t2 === "Circle") {
  //         const d = distOld(center(s1), center(s2));
  //         const o = stack([s1.r.contents, s2.r.contents, 10]);
  //         return o.sum().sub(d);
  //     } else throw new Error(`${[t1, t2]} not supported for disjoint`);
  // },

  smallerThan: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
    // s1 is smaller than s2
    const offset = mul(varOf(0.4), s2.r.contents);
    return sub(sub(s1.r.contents, s2.r.contents), offset);
  },

  // smallerThanOld: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {
  //     // s1 is smaller than s2
  //     const offset = scalar(0.4).mul(s2.r.contents); // take 0.4 as param
  //     return s1.r.contents.sub(s2.r.contents).sub(offset);
  // },

  outsideOf: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding = 10
  ) => {
    if (t1 === "Text" && t2 === "Circle") {
      const textR = max(s1.w.contents, s1.h.contents);
      const d = ops.vdist(centerList(s1), centerList(s2));
      return sub(add(add(s2.r.contents, textR),
        varOf(padding)),
        d);
    } else throw new Error(`${[t1, t2]} not supported for outsideOf`);
  },

  // outsideOfOld: (
  //     [t1, s1]: [string, any],
  //     [t2, s2]: [string, any],
  //     padding = 10
  // ) => {
  //     if (t1 === "Text" && t2 === "Circle") {
  //         const textR = maximum(s1.w.contents, s1.h.contents);
  //         const d = distOld(center(s1), center(s2));
  //         return s2.r.contents
  //             .add(textR)
  //             .add(scalar(padding))
  //             .sub(d);
  //     } else throw new Error(`${[t1, t2]} not supported for outsideOf`);
  // },

  overlapping: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding = 10
  ) => {
    if (t1 === "Circle" && t2 === "Circle") {
      return looseIntersectOld(centerOld(s1), s1.r.contents,
        centerOld(s2), s2.r.contents, padding);
    } else throw new Error(`${[t1, t2]} not supported for overlapping`);
  },

  tangentTo: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any]
  ) => {
    // Inner tangency -- assuming circle1 contains circle2
    if (t1 === "Circle" && t2 === "Circle") {
      const d = distOld(centerOld(s1), centerOld(s2));
      const r1 = s1.r.contents;
      const r2 = s2.r.contents;
      // Should we bring back the polygon code?
      // ||c_a - c_b|| - (r1 - r2)
      // Outer tangency would be `||c_a - c_b|| - (r1 + r2)`
      return d.sub(r1.sub(r2));
    } else throw new Error(`${[t1, t2]} not supported for tangentTo`);
  },
};

// -------- Helpers for writing objectives

const typesAre = (inputs: string[], expected: string[]) =>
  (inputs.length === expected.length) && _.zip(inputs, expected).map(([i, e]) => i === e);

// -------- (Hidden) helpers for objective/constraints/computations

const centerArrow2 = (arr: any, center1: VarAD[], center2: VarAD[], [o1, o2]: VarAD[]): VarAD => {
  const vec = ops.vsub(center2, center1); // direction the arrow should point to
  const dir = ops.vnormalize(vec);

  let start = center1;
  let end = center2;

  // TODO: take in spacing, use the right text dimension/distance?, note on arrow directionality

  // TODO: add abs
  if (gt(ops.vnorm(vec), add(o1, absVal(o2)))) {
    start = ops.vadd(center1, ops.vmul(o1, dir));
    end = ops.vadd(center2, ops.vmul(o2, dir));
  }

  const fromPt = [arr.startX.contents, arr.startY.contents];
  const toPt = [arr.endX.contents, arr.endY.contents];

  return add(ops.vdistsq(fromPt, start), ops.vdistsq(toPt, end));
}

// const centerArrow2Old = (arr: any, center1: DiffVar, center2: DiffVar, [o1, o2]: DiffVar[]): DiffVar => {
//     const vec = center2.sub(center1); // direction the arrow should point to
//     const dir = normalize(vec);

//     let start = center1;
//     let end = center2;

//     // TODO: take in spacing, use the right text dimension/distance?, note on arrow directionality
//     if (norm(vec).greater(o1.add(abs(o2)))) {
//         start = center1.add(o1.mul(dir));
//         end = center2.add(o2.mul(dir));
//     }

//     const fromPt = stack([arr.startX.contents, arr.startY.contents]);
//     const toPt = stack([arr.endX.contents, arr.endY.contents]);

//     return distsq(fromPt, start).add(distsq(toPt, end));
// }


// -------- Utils for objective/constraints/computations

const sc = (x: any): number => x.dataSync()[0];

export const epsd2: Tensor = scalar(10e-10);

export const looseIntersectOld = (center1: Tensor, r1: Tensor, center2: Tensor, r2: Tensor, padding: number) =>
  distOld(center1, center2).sub(r1.add(r2).sub(scalar(padding)));
// dist (x1, y1) (x2, y2) - (s1 + s2 - 10)

export const centerOld = (props: any): Tensor => {
  const [x, y] = [props.x.contents, props.y.contents];
  return stack([props.x.contents, props.y.contents]);
};

export const center = (props: any): VarAD[] => {
  return [props.x.contents, props.y.contents];
};

export const centerList = (props: any): VarAD[] => {
  return [props.x.contents, props.y.contents];
};

const distOld = (p1: Tensor, p2: Tensor): Tensor => p1.sub(p2).norm();

// Be careful not to use element-wise operations. This should return a scalar.
export const distsq = (p1: VarAD[], p2: VarAD[]): VarAD => {
  const dv = ops.vsub(p1, p2);
  const res = ops.vnormsq(dv);
  return res;
};

export const distsqOld = (p1: Tensor, p2: Tensor): Tensor => {
  const dp = p1.sub(p2);
  return dp.dot(dp);
};

// with epsilon to avoid NaNs
export const normalize = (v: Tensor): Tensor => v.div(v.norm().add(epsd2));

// TODO: use it
// const getConstraint = (name: string) => {
//   if (!constrDict[name]) throw new Error(`Constraint "${name}" not found`);
//   // TODO: types for args
//   return (...args: any[]) => toPenalty(constrDict[name]);
// };

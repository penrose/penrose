import {
  varOf,
  constOf,
  constOfIf,
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
  ops,
  fns,
  epsd,
  EPS_DENOM
} from "engine/Autodiff";
import { linePts } from "utils/OtherUtils";
import { canvasSize } from "ui/Canvas";
import * as _ from "lodash";

export const objDict = {
  // (x - y)^2
  equal: (x: VarAD, y: VarAD) => squared(sub(x, y)),

  above: ([t1, top]: [string, any], [t2, bottom]: [string, any], offset = 100) =>
    // (getY top - getY bottom - offset) ^ 2
    squared(
      sub(sub(top.y.contents, bottom.y.contents),
        varOf(offset))),

  sameCenter: ([t1, s1]: [string, any], [t2, s2]: [string, any]) =>
    ops.vdistsq(fns.center(s1), fns.center(s2)),

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
      res = mul(constOfIf(weight), allForces);
    } else {
      // 1 / (d^2(cx, cy) + eps)
      res = inverse(ops.vdistsq(fns.center(s1), fns.center(s2)));
    }

    return mul(res, constOf(repelWeight));
  },

  centerArrow: ([t1, arr]: [string, any], [t2, text1]: [string, any], [t3, text2]: [string, any]): VarAD => {
    const spacing = varOf(1.1); // arbitrary

    if (typesAre([t1, t2, t3], ["Arrow", "Text", "Text"])) {
      // HACK: Arbitrarily pick the height of the text
      // [spacing * getNum text1 "h", negate $ 2 * spacing * getNum text2 "h"]
      return centerArrow2(arr, fns.center(text1), fns.center(text2),
        [mul(spacing, (text1.h.contents)),
        neg(mul(text2.h.contents, spacing))]);

    } else throw new Error(`${[t1, t2, t3]} not supported for centerArrow`);
  },

  // can this be made more efficient (code-wise) by calling "above" and swapping arguments? - stella
  below: ([t1, bottom]: [string, any], [t2, top]: [string, any], offset = 100) =>
    squared(sub(sub(top.y.contents, bottom.y.contents), constOfIf(offset))),

  centerLabel: ([t1, s1]: [string, any], [t2, s2]: [string, any], w: number): VarAD => {

    if (typesAre([t1, t2], ["Arrow", "Text"])) {
      const arr = s1;
      const text1 = s2;
      const mx = div(add(arr.startX.contents, arr.endX.contents), constOf(2.0));
      const my = div(add(arr.startY.contents, arr.endY.contents), constOf(2.0));

      // entire equation is (mx - lx) ^ 2 + (my + 1.1 * text.h - ly) ^ 2 from Functions.hs - split it into two halves below for readability
      const lh = squared(sub(mx, text1.x.contents));
      const rh = squared(sub(add(my, mul(text1.h.contents, constOf(1.1))), text1.y.contents));
      return mul(add(lh, rh), constOfIf(w));

    } else if (typesAre([t1, t2], ["Rectangle", "Text"])) {
      // TODO: This should be applied generically on any two GPIs with a center
      return objDict.sameCenter([t1, s1], [t2, s2]);

    } else throw new Error(`${[t1, t2]} not supported for centerLabel`)
  },

  near: ([t1, s1]: [string, any], [t2, s2]: [string, any], offset = 10.0) => {
    // This only works for two objects with centers (x,y)
    const res = absVal(ops.vdistsq(fns.center(s1), fns.center(s2)));
    return sub(res, squared(constOfIf(offset)));
  },

  nearPt: ([t1, s1]: [string, any], x: any, y: any) => {
    return ops.vdistsq(fns.center(s1), [constOfIf(x), constOfIf(y)]);
  },

};

// constrDict is the dictionary of all constraints available in Style These are
// called by the key of the dictionary (e.g., maxSize is called via the string
// "maxSize" in Style)
export const constrDict = {
  maxSize: ([shapeType, props]: [string, any]) => {
    const limit = Math.max(...canvasSize);
    switch (shapeType) {
      case "Circle":
        return sub(props.r.contents, constOf(limit / 6.0));
      case "Square":
        return sub(props.side.contents, constOf(limit / 3.0));
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a maxSize`);
    }
  },

  minSize: ([shapeType, props]: [string, any]) => {
    const limit = 20;
    switch (shapeType) {
      case "Circle":
        return sub(constOf(limit), props.r.contents);
      case "Square":
        return sub(constOf(limit), props.side.contents);
      default:
        // HACK: report errors systematically
        throw new Error(`${shapeType} doesn't have a minSize`);
    }
  },

  contains: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    offset: VarAD
  ) => {

    if (t1 === "Circle" && t2 === "Circle") {
      const d = ops.vdist(fns.center(s1), fns.center(s2));
      const o = offset
        ? sub(sub(s1.r.contents, s2.r.contents), offset)
        : sub(s1.r.contents, s2.r.contents);
      const res = sub(d, o);
      return res;

    } else if (t1 === "Circle" && t2 === "Text") {
      const d = ops.vdist(fns.center(s1), fns.center(s2));
      const textR = max((s2.w.contents), s2.h.contents);
      return add(sub(d, s1.r.contents), textR);
    } else if (t1 === "Rectangle" && t2 === "Circle") {
      // contains [GPI r@("Rectangle", _), GPI c@("Circle", _), Val (FloatV padding)] =
      // -- HACK: reusing test impl, revert later
      //    let r_l = min (getNum r "w") (getNum r "h") / 2
      //        diff = r_l - getNum c "r"
      //    in dist (getX r, getY r) (getX c, getY c) - diff + padding

      // TODO: `rL` is probably a hack for dimensions
      const rL = div(min(s1.w.contents, s1.h.contents), varOf(2.0));
      const diff = sub(rL, s2.r.contents);
      const d = ops.vdist(fns.center(s1), fns.center(s2));
      return add(sub(d, diff), offset);

    } else if (t1 === "Square" && t2 === "Circle") {
      // dist (outerx, outery) (innerx, innery) - (0.5 * outer.side - inner.radius)
      const sq = [s1.x.contents, s1.y.contents];
      const d = ops.vdist(sq, fns.center(s2));
      return sub(d, sub(mul(constOf(0.5), s1.side.contents), s2.r.contents));

    } else if (t1 === "Rectangle" && t2 === "Text") {
      // contains [GPI r@("Rectangle", _), GPI l@("Text", _), Val (FloatV padding)] =
      // TODO: implement precisely, max (w, h)? How about diagonal case?
      // dist (getX l, getY l) (getX r, getY r) - getNum r "w" / 2 +
      //   getNum l "w" / 2 + padding

      const a1 = ops.vdist(fns.center(s1), fns.center(s2));
      const a2 = div(s1.w.contents, varOf(2.0));
      const a3 = div(s2.w.contents, varOf(2.0));
      return add(add(sub(a1, a2), a3), offset);

    } else throw new Error(`${[t1, t2]} not supported for contains`);

  },

  disjoint: ([t1, s1]: [string, any], [t2, s2]: [string, any], offset = 5.0) => {

    if (t1 === "Circle" && t2 === "Circle") {
      const d = ops.vdist(fns.center(s1), fns.center(s2));
      const o = [s1.r.contents, s2.r.contents, varOf(10.0)];
      return sub(addN(o), d);

    } else if (typesAre([t1, t2], ["Text", "Line"])) {
      const [text, seg] = [s1, s2];
      const centerT = fns.center(text);
      const endpts = linePts(seg);
      const cp = closestPt_PtSeg(centerT, endpts);
      const lenApprox = div(text.w.contents, constOf(2.0));
      return sub(add(lenApprox, constOfIf(offset)), ops.vdist(centerT, cp));

    } else throw new Error(`${[t1, t2]} not supported for disjoint`);
  },

  smallerThan: ([t1, s1]: [string, any], [t2, s2]: [string, any]) => {

     if(t1 === "Circle" && t2 === "Circle") {
        // s1 is smaller than s2
        const offset = mul(varOf(0.4), s2.r.contents);
        return sub(sub(s1.r.contents, s2.r.contents), offset);
     } else throw new Error(`${[t1, t2]} not supported for smallerThan`);
  },

  equalTo: (x: VarAD, y: VarAD) => {
     return mul(sub(x,x0),sub(x,x1));
     return absVal(sub(x, y));
  },

  inRange: (x: VarAD, x0: VarAD, x1: VarAD) => {
     return mul(sub(x,x0),sub(x,x1));
  },

  outsideOf: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding = 10
  ) => {
    if (t1 === "Text" && t2 === "Circle") {
      const textR = max(s1.w.contents, s1.h.contents);
      const d = ops.vdist(fns.center(s1), fns.center(s2));
      return sub(add(add(s2.r.contents, textR),
        constOfIf(padding)),
        d);
    } else throw new Error(`${[t1, t2]} not supported for outsideOf`);
  },

  overlapping: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any],
    padding = 10
  ) => {
    if (t1 === "Circle" && t2 === "Circle") {
      return looseIntersect(fns.center(s1), s1.r.contents,
        fns.center(s2), s2.r.contents, constOfIf(padding));
    } else throw new Error(`${[t1, t2]} not supported for overlapping`);
  },

  tangentTo: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any]
  ) => {
    if (t1 === "Circle" && t2 === "Circle") {
      const d = ops.vdist(fns.center(s1), fns.center(s2));
      const r1 = s1.r.contents;
      const r2 = s2.r.contents;
      // Since we want equality
      return absVal(sub(d, sub(r1, r2)));
    } else throw new Error(`${[t1, t2]} not supported for tangentTo`);
  },

  orthogonalCircles: (
    [t1, s1]: [string, any],
    [t2, s2]: [string, any]
  ) => {
    if (t1 === "Circle" && t2 === "Circle") {
       // Two circles of radii r1,r2 at a distance d will be
       // orthogonal iff d^2 = r1^2 + r2^2.
      const d = ops.vdist(fns.center(s1), fns.center(s2));
      const d2 = squared(d);
      const r1 = s1.r.contents;
      const r2 = s2.r.contents;
      const r12 = squared(r1);
      const r22 = squared(r2);
      const e = sub(d2,add(r12,r22));
      return absVal(e);
    } else throw new Error(`${[t1, t2]} not supported for orthogonalCircles`);
  },

  atDist: ([t1, s1]: [string, any], [t2, s2]: [string, any], offset: VarAD) => {
    // Place the latter at a distance from the center of the point
    // TODO: Account for the size/radius of the initial point, rather than just the center

    if (t2 === "Text") {
      // Get polygon of text (box)
      // TODO: Make this a GPI property
      // TODO: Do this properly; Port the matrix stuff in `textPolygonFn` / `textPolygonFn2` in Shapes.hs
      // I wrote a version simplified to work for rectangles

      const text = s2;
      // TODO: Simplify this code since I don't actually use `textPts`
      const halfWidth = div(text.w.contents, constOf(2.0));
      const halfHeight = div(text.h.contents, constOf(2.0));
      const nhalfWidth = neg(halfWidth);
      const nhalfHeight = neg(halfHeight);
      const textCenter = fns.center(text);
      // CCW: TR, TL, BL, BR
      const textPts = [[halfWidth, halfHeight], [nhalfWidth, halfHeight],
      [nhalfWidth, nhalfHeight], [halfWidth, nhalfHeight]].map(p => ops.vadd(textCenter, p));

      const pt = { x: s1.x.contents, y: s1.y.contents };
      const rect = {
        minX: textPts[1][0], maxX: textPts[0][0],
        minY: textPts[2][1], maxY: textPts[0][1]
      };

      // If the point is inside the box, push it outside w/ `noIntersect`
      if (pointInBox(pt, rect)) {
        return noIntersect(textCenter, text.w.contents, fns.center(s1), constOf(2.0));
      } else {
        // If the point is outside the box, try to get the distance from the point to equal the desired distance
        const dsqRes = dsqBP(pt, rect);
        const WEIGHT = 1;
        return mul(constOf(WEIGHT), equalHard(dsqRes, squared(offset)));
      }

    } else {
      throw Error(`unsupported shapes for 'atDist': ${t1}, ${t2}`);
    }
  },

  perpendicular: (q: ITupV<VarAD>, p: ITupV<VarAD>, r: ITupV<VarAD>): VarAD => {
    const v1 = ops.vsub(q.contents, p.contents);
    const v2 = ops.vsub(r.contents, p.contents);
    const dotProd = ops.vdot(v1, v2);
    return equalHard(dotProd, constOf(0.0));
  },

};

// -------- Helpers for writing objectives

const typesAre = (inputs: string[], expected: string[]): boolean =>
  (inputs.length === expected.length) && _.every(_.zip(inputs, expected).map(([i, e]) => i === e));

// -------- (Hidden) helpers for objective/constraints/computations

// This is an equality constraint (x = c) via two inequality constraints (x <= c and x >= c)
const equalHard = (x: VarAD, y: VarAD) => {
  const valMax = max(x, y);
  const valMin = min(x, y);
  // TODO: I guess you could also use an absolute value?
  return sub(valMax, valMin);
};

const noIntersect = (center1: VarAD[], r1: VarAD, center2: VarAD[], r2: VarAD, padding = 10): VarAD => {
  // noIntersect [[x1, y1, s1], [x2, y2, s2]] = - dist (x1, y1) (x2, y2) + (s1 + s2 + 10)
  const res = add(add(r1, r2), constOfIf(padding));
  return sub(res, ops.vdist(center1, center2));
};

const looseIntersect = (center1: VarAD[], r1: VarAD, center2: VarAD[], r2: VarAD, padding: VarAD): VarAD => {
  // looseIntersect [[x1, y1, s1], [x2, y2, s2]] = dist (x1, y1) (x2, y2) - (s1 + s2 - 10)
  const res = sub(add(r1, r2), padding);
  return sub(ops.vdist(center1, center2), res);
};

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

const repelPt = (c: VarAD, a: VarAD[], b: VarAD[]) => div(c, add(ops.vdistsq(a, b), constOf(EPS_DENOM)));

// ------- Polygon-related helpers

// Assuming `rect` is an axis-aligned bounding box (AABB)
const pointInBox = (p: any, rect: any): boolean => {
  return p.x > rect.minX && p.x < rect.maxX && p.y > rect.minY && p.y < rect.maxY;
};

// Assuming `rect` is an axis-aligned bounding box (AABB)
// Compute the positive distance squared from point to box (NOT the signed distance)
// https://stackoverflow.com/questions/5254838/calculating-distance-between-a-point-and-a-rectangular-box-nearest-point
const dsqBP = (p: any, rect: any): VarAD => {
  const dx = max(max(sub(rect.minX, p.x), constOf(0.0)), sub(p.x, rect.maxX));
  const dy = max(max(sub(rect.minY, p.y), constOf(0.0)), sub(p.y, rect.maxY));
  return sqrt(add(squared(dx), squared(dy)));
};

// TODO: Rewrite the lerp code to be more concise

// `k` is the fraction of interpolation
const lerp = (l: VarAD, r: VarAD, k: VarAD): VarAD => {
  return add(mul(l, sub(constOf(1.0), k)), mul(r, k));
};

const lerp2 = (l: VarAD[], r: VarAD[], k: VarAD): [VarAD, VarAD] => {
  return [lerp(l[0], r[0], k), lerp(l[1], r[1], k)];
};

const sampleSeg = (line: VarAD[][]) => {
  const NUM_SAMPLES = 15;
  const NUM_SAMPLES2 = constOf(1 + NUM_SAMPLES);
  // TODO: Check that this covers the whole line, i.e. no off-by-one error
  const samples = _.range(1 + NUM_SAMPLES).map(i => {
    const k = div(constOf(i), NUM_SAMPLES2);
    return lerp2(line[0], line[1], k);
  });

  return samples;
};

const closestPt_PtSeg = (pt: VarAD[], [start, end]: VarAD[][]): VarAD[] => {
  const EPS0 = varOf(10e-3);
  const lensq = max(ops.vdistsq(start, end), EPS0); // Avoid a divide-by-0 if the line is too small

  // If line seg looks like a point, the calculation just returns (something close to) `v`
  const dir = ops.vsub(end, start);
  // t = ((p -: v) `dotv` dir) / lensq -- project vector onto line seg and normalize
  const t = div(ops.vdot(ops.vsub(pt, start), dir), add(lensq, constOf(EPS_DENOM)));
  const t1 = clamp([0.0, 1.0], t);

  // v +: (t' *: dir) -- walk along vector of line seg
  return ops.vadd(start, ops.vmul(t1, dir));
};

const clamp = ([l, r]: number[], x: VarAD): VarAD => {
  return max(constOf(l), min(constOf(r), x));
}

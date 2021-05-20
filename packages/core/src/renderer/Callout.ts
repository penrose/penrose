import { toScreen } from "utils/Util";
import {
  attrFill,
  attrRadiusX,
  attrRadiusY,
  attrStroke,
  attrTitle,
  attrWH,
  attrXY,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

import * as v from "@thi.ng/vectors";
import * as _ from "lodash";

/**
 * Some vector operations that can be used on lists.
 */
export const ops = {
  /**
   * Return the norm of the 2-vector `[c1, c2]`.
   */
  norm: (c1: number, c2: number) => ops.vnorm([c1, c2]),

  /**
   * Return the Euclidean distance between scalars `c1, c2`.
   */
  dist: (c1: number, c2: number) => ops.vnorm([c1, c2]),

  /**
   * Return the sum of vectors `v1, v2.
   */
  vadd: (v1: number[], v2: number[]): number[] => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, (a, b) => a + b);
    return res;
  },

  /**
   * Return the difference of vectors `v1, v2.
   */
  vsub: (v1: number[], v2: number[]): number[] => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, (a, b) => a - b);
    return res;
  },

  /**
   * Return the Euclidean norm squared of vector `v`.
   */
  vnormsq: (v: number[]): number => {
    const res = v.map((e) => e * e);
    return _.reduce(res, (x, y) => x + y, 0.0); // TODO: Will this one (var(0)) have its memory freed?
    // Note (performance): the use of 0 adds an extra +0 to the comp graph, but lets us prevent undefined if the list is empty
  },

  /**
   * Return the Euclidean norm of vector `v`.
   */
  vnorm: (v: number[]): number => {
    const res = ops.vnormsq(v);
    return Math.sqrt(res);
  },

  /**
   * Return the vector `v` scaled by scalar `c`.
   */
  vmul: (c: number, v: number[]): number[] => {
    return v.map((e) => c * e);
  },

  /**
   * Return the vector `v`, scaled by `-1`.
   */
  vneg: (v: number[]): number[] => {
    return ops.vmul(-1.0, v);
  },

  /**
   * Return the vector `v` divided by scalar `c`.
   */
  vdiv: (v: number[], c: number): number[] => {
    return v.map((e) => e / c);
  },

  /**
   * Return the vector `v`, normalized.
   */
  vnormalize: (v: number[]): number[] => {
    const vsize = ops.vnorm(v) + 10e-10;
    return ops.vdiv(v, vsize);
  },

  /**
   * Return the Euclidean distance between vectors `v` and `w`.
   */
  vdist: (v: number[], w: number[]): number => {
    if (v.length !== w.length) {
      throw Error("expected vectors of same length");
    }

    return ops.vnorm(ops.vsub(v, w));
  },

  /**
   * Return the Euclidean distance squared between vectors `v` and `w`.
   */
  vdistsq: (v: number[], w: number[]): number => {
    if (v.length !== w.length) {
      throw Error("expected vectors of same length");
    }

    return ops.vnormsq(ops.vsub(v, w));
  },

  /**
   * Return the dot product of vectors `v1, v2`.
   * Note: if you want to compute a norm squared, use `vnormsq` instead, it generates a smaller computational graph
   */
  vdot: (v1: number[], v2: number[]): number => {
    if (v1.length !== v2.length) {
      throw Error("expected vectors of same length");
    }

    const res = _.zipWith(v1, v2, (a, b) => a * b);
    return _.reduce(res, (x, y) => x + y, 0.0);
  },

  /**
   * Return the sum of elements in vector `v`.
   */
  vsum: (v: number[]): number => {
    return _.reduce(v, (x, y) => x + y, 0.0);
  },

  /**
   * Return `v + c * u`.
   */
  vmove: (v: number[], c: number, u: number[]) => {
    return ops.vadd(v, ops.vmul(c, u));
  },

  /**
   * Rotate a 2D point `[x, y]` by 90 degrees clockwise.
   */
  rot90: ([x, y]: number[]): number[] => {
    return [-y, x];
  },
};

/**
 * Return the bounding box (as 4 segments) of an axis-aligned box-like shape given by `center`, width `w`, height `h` as an object with `top, bot, left, right`.
 */
export const bboxSegs = (center: number[], w: number, h: number): any => {
  const halfWidth = w / 2;
  const halfHeight = h / 2;
  const nhalfWidth = -halfWidth;
  const nhalfHeight = -halfHeight;
  // CCW: TR, TL, BL, BR
  const ptsR = [
    [halfWidth, halfHeight],
    [nhalfWidth, halfHeight],
    [nhalfWidth, nhalfHeight],
    [halfWidth, nhalfHeight],
  ].map((p) => ops.vadd(center, p));

  const cornersR = {
    topRight: ptsR[0],
    topLeft: ptsR[1],
    bottomLeft: ptsR[2],
    bottomRight: ptsR[3],
  };

  const segsR = {
    top: [cornersR.topLeft, cornersR.topRight],
    bot: [cornersR.bottomLeft, cornersR.bottomRight],
    left: [cornersR.bottomLeft, cornersR.topLeft],
    right: [cornersR.bottomRight, cornersR.topRight],
  };

  const linesR = {
    minX: cornersR.topLeft[0],
    maxX: cornersR.topRight[0],
    minY: cornersR.bottomLeft[1],
    maxY: cornersR.topLeft[1],
  };

  return { ptsR, cornersR, segsR, linesR };
};

// returns true if the line from (a,b)->(c,d) intersects with (p,q)->(r,s)
const intersects = (s1: number[][], s2: number[][]): boolean => {
  const [l1_p1, l1_p2, l2_p1, l2_p2] = [s1[0], s1[1], s2[0], s2[1]];
  const [[a, b], [c, d]] = [l1_p1, l1_p2];
  const [[p, q], [r, s]] = [l2_p1, l2_p2];

  var det, gamma, lambda;
  det = (c - a) * (s - q) - (r - p) * (d - b);
  if (det === 0) {
    return false;
  } else {
    lambda = ((s - q) * (r - a) + (p - r) * (s - b)) / det;
    gamma = ((b - d) * (r - a) + (c - a) * (s - b)) / det;
    return 0 < lambda && lambda < 1 && 0 < gamma && gamma < 1;
  }
};

interface Point2D {
  x: number;
  y: number;
}

const toPt = (v: number[]): Point2D => ({ x: v[0], y: v[1] });

function intersection(s1: number[][], s2: number[][]): number[] {
  const [from1, to1, from2, to2] = [s1[0], s1[1], s2[0], s2[1]];
  const res = intersection2(toPt(from1), toPt(to1), toPt(from2), toPt(to2));
  return [res.x, res.y];
}

// https://stackoverflow.com/posts/58657254/revisions
// Assumes lines don'intersect! Use intersect to check it first
function intersection2(
  from1: Point2D,
  to1: Point2D,
  from2: Point2D,
  to2: Point2D
): Point2D {
  const dX: number = to1.x - from1.x;
  const dY: number = to1.y - from1.y;

  const determinant: number = dX * (to2.y - from2.y) - (to2.x - from2.x) * dY;
  if (determinant === 0) {
    throw Error("parallel lines");
  } // parallel lines

  const lambda: number =
    ((to2.y - from2.y) * (to2.x - from1.x) +
      (from2.x - to2.x) * (to2.y - from1.y)) /
    determinant;
  const gamma: number =
    ((from1.y - to1.y) * (to2.x - from1.x) + dX * (to2.y - from1.y)) /
    determinant;

  // check if there is an intersection
  if (!(0 <= lambda && lambda <= 1) || !(0 <= gamma && gamma <= 1)) {
    throw Error("lines don't intersect");
  }

  return {
    x: from1.x + lambda * dX,
    y: from1.y + lambda * dY,
  };
}

/**
 * Return true iff `p` is in rect `b`, assuming `rect` is an axis-aligned bounding box (AABB) with properties `minX, maxX, minY, maxY`.
 */
const pointInBox = (p: any, rect: any): boolean => {
  return (
    p.x > rect.minX && p.x < rect.maxX && p.y > rect.minY && p.y < rect.maxY
  );
};

const makeCallout = (
  anchor: [number, number],
  center: [number, number],
  contentsW: number,
  contentsH: number
): [number, number][] => {
  // const makeCallout = (anchor: any, center: any, w: any, h: any): [number, number][] => {
  console.log("params", [anchor, center, contentsW, contentsH]);

  const calloutPadding = 30; // Padding around the text for rect
  const calloutThickness = 30; // Thickness of base of stem
  const calloutEndPadding = 40; // Space between the external anchor point and the stem
  const maxCalloutDist = 200;

  // Rectangle segments
  const { ptsR, cornersR, segsR, linesR } = bboxSegs(
    center,
    contentsW,
    contentsH
  );

  if (pointInBox(toPt(anchor), linesR)) {
    console.log("Anchor in box");
    return ptsR;
  }

  // callout center -> anchor, parallel to callout direction
  const vec = ops.vnormalize(ops.vsub(center, anchor));
  const stemStart = ops.vmove(anchor, calloutEndPadding, vec); // Pointy part

  // stemSeg_i = one side of the speech bubble "tail"
  // Extrusions from normal on either side of the stem
  const t = ops.vnorm(ops.vsub(center, stemStart));
  const stemSide1Start = ops.vmove(
    stemStart,
    calloutThickness / 2,
    ops.rot90(vec)
  );
  const stemSide1End = ops.vmove(stemSide1Start, t, vec);
  const stemSeg1 = [stemSide1Start, stemSide1End];

  const stemSide2Start = ops.vmove(
    stemStart,
    calloutThickness / 2,
    ops.rot90(ops.rot90(ops.rot90(vec)))
  );
  const stemSide2End = ops.vmove(stemSide2Start, t, vec);
  const stemSeg2 = [stemSide2Start, stemSide2End];

  // intersectPts = the places where the extruded line intersects with the rectangle segments, where the stem should be drawn in the polygon
  let intersectPt1 = undefined;
  let side1 = undefined;

  if (intersects(segsR.top, stemSeg1)) {
    intersectPt1 = intersection(segsR.top, stemSeg1);
    side1 = "top";
  } else if (intersects(segsR.bot, stemSeg1)) {
    intersectPt1 = intersection(segsR.bot, stemSeg1);
    side1 = "bot";
  } else if (intersects(segsR.left, stemSeg1)) {
    intersectPt1 = intersection(segsR.left, stemSeg1);
    side1 = "left";
  } else if (intersects(segsR.right, stemSeg1)) {
    intersectPt1 = intersection(segsR.right, stemSeg1);
    side1 = "right";
  } else {
    console.log("no intersection 1", segsR, stemSeg1);
    // throw Error("no intersection for point 1");
  }

  // console.log("rect", ptsR, cornersR, segsR);
  // console.log("intersectPt1", intersectPt1);

  let intersectPt2 = undefined;
  let side2 = undefined;

  if (intersects(segsR.top, stemSeg2)) {
    intersectPt2 = intersection(segsR.top, stemSeg2);
    side2 = "top";
  } else if (intersects(segsR.bot, stemSeg2)) {
    intersectPt2 = intersection(segsR.bot, stemSeg2);
    side2 = "bot";
  } else if (intersects(segsR.left, stemSeg2)) {
    intersectPt2 = intersection(segsR.left, stemSeg2);
    side2 = "left";
  } else if (intersects(segsR.right, stemSeg2)) {
    intersectPt2 = intersection(segsR.right, stemSeg2);
    side2 = "right";
  } else {
    console.log("no intersection 2", segsR, stemSeg2);
    // throw Error("no intersection for point 2");
  }

  // console.log("rect", ptsR, cornersR, segsR);
  // console.log("intersectPt1", intersectPt1);

  const stemPts = [
    intersectPt1 ? intersectPt1 : stemSide1End,
    anchor,
    intersectPt2 ? intersectPt2 : stemSide2End,
  ];

  // TODO: The corner should be skipped if the points "include" it?

  // [ ] <- rect center to middle of right segment is zero angle
  const zeroAngleVec = ops.vdiv(
    ops.vadd(cornersR.bottomRight, cornersR.topRight),
    2
  );

  // Sort points by their angle to the center of the callout
  const ptsSorted = _.sortBy(ptsR.concat(stemPts), (pt) =>
    v.angleBetween2(
      ops.vsub(zeroAngleVec, center),
      v.normalize([], ops.vsub(pt, center))
    )
  );

  const pts = ptsSorted;
  return pts as [number, number][];
};

const Callout = ({ shape, canvasSize }: ShapeProps) => {
  const elem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "polygon"
  );
  attrFill(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);

  const [anchor, center, w, h] = [
    shape.properties.anchor.contents as [number, number],
    shape.properties.center.contents as [number, number],
    shape.properties.w.contents as number,
    shape.properties.h.contents as number,
  ];

  const pts = makeCallout(anchor, center, w, h);
  const ptsScreen = pts.map((p) => toScreen(p, canvasSize));
  elem.setAttribute("points", ptsScreen.toString());

  return elem;
};
export default Callout;

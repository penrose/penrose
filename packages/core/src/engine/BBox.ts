import { Pt2, VarAD } from "types/ad";
import { add, constOf, div, neg, ops } from "./Autodiff";

interface IBBox {
  w: VarAD;
  h: VarAD;
  center: Pt2;
}

interface ICorners {
  topRight: Pt2;
  topLeft: Pt2;
  bottomLeft: Pt2;
  bottomRight: Pt2;
}

interface IIntervals {
  xRange: [VarAD, VarAD];
  yRange: [VarAD, VarAD];
}

interface IEdges {
  top: [Pt2, Pt2];
  bot: [Pt2, Pt2];
  left: [Pt2, Pt2];
  right: [Pt2, Pt2];
}

export type BBox = IBBox;

export type Corners = ICorners;

export type Intervals = IIntervals;

export type Edges = IEdges;

/**
 * Input: A width, height, and center.
 * Output: A new BBox.
 */
export const bbox = (w: VarAD, h: VarAD, center: Pt2): BBox => {
  return {
    w,
    h,
    center,
  };
};

export const corners = (b: BBox): Corners => {
  const halfWidth = div(b.w, constOf(2.0));
  const halfHeight = div(b.h, constOf(2.0));
  const nhalfWidth = neg(halfWidth);
  const nhalfHeight = neg(halfHeight);
  const pts = <Pt2[]>[
    [halfWidth, halfHeight],
    [nhalfWidth, halfHeight],
    [nhalfWidth, nhalfHeight],
    [halfWidth, nhalfHeight],
  ].map((p) => ops.vadd(b.center, p));

  return {
    topRight: pts[0],
    topLeft: pts[1],
    bottomLeft: pts[2],
    bottomRight: pts[3],
  };
};

/**
 * Input: A BBox and an inflation parameter delta.
 * Output: A BBox inflated on all sides by delta.
 */
export const inflate = (b: BBox, delta: VarAD): BBox => {
  return bbox(
    add(b.w, add(delta, delta)),
    add(b.h, add(delta, delta)),
    b.center
  );
};

/**
 * Input: A BBox.
 * Output: The min X of the BBox.
 */
export const minX = (b: BBox): VarAD => {
  return corners(b).topLeft[0];
};

/**
 * Input: A BBox.
 * Output: The max X of the BBox.
 */
export const maxX = (b: BBox): VarAD => {
  return corners(b).bottomRight[0];
};

/**
 * Input: A BBox.
 * Output: The min Y of the BBox.
 */
export const minY = (b: BBox): VarAD => {
  return corners(b).bottomRight[1];
};

/**
 * Input: A BBox.
 * Output: The max Y of the BBox.
 */
export const maxY = (b: BBox): VarAD => {
  return corners(b).topLeft[1];
};

/**
 * Input: A BBox.
 * Output: The X interval of the BBox.
 */
export const xRange = (b: BBox): [VarAD, VarAD] => {
  return [minX(b), maxX(b)];
};

/**
 * Input: A BBox.
 * Output: The Y interval of the BBox.
 */
export const yRange = (b: BBox): [VarAD, VarAD] => {
  return [minY(b), maxY(b)];
};

/**
 * Input: A BBox.
 * The four edges of the BBox.
 */
export const edges = (b: BBox): Edges => {
  return {
    top: <[Pt2, Pt2]>[corners(b).topLeft, corners(b).topRight],
    bot: <[Pt2, Pt2]>[corners(b).bottomLeft, corners(b).bottomRight],
    left: <[Pt2, Pt2]>[corners(b).bottomLeft, corners(b).topLeft],
    right: <[Pt2, Pt2]>[corners(b).bottomRight, corners(b).topRight],
  };
};

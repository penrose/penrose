import _ from "lodash";
import { Pt2, VarAD } from "types/ad";
import {
  absVal,
  add,
  constOf,
  div,
  max,
  min,
  mul,
  neg,
  ops,
  sub,
  debug,
} from "./Autodiff";

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

/**
 * Preconditions:
 *   If the input is line-like, it must be axis-aligned.
 *   Assumes line-like shapes are longer than they are thick.
 * Input: A rect- or line-like shape.
 * Output: A new BBox
 * Errors: Throws an error if the input shape is not rect- or line-like.
 */
export const overboxFromShape = (t: string, s: any): BBox => {
  let w, h: VarAD;
  let center: Pt2;
  switch (t) {
    case "Circle":
      w = mul(s.r.contents, constOf(2));
      h = mul(s.r.contents, constOf(2));
      center = s.center.contents;
      break;
    case "Ellipse":
      w = mul(s.rx.contents, constOf(2));
      h = mul(s.ry.contents, constOf(2));
      center = s.center.contents;
      break;
    case "Line":
    case "Arrow":
      const dir = ops.vnormalize(ops.vsub(s.end.contents, s.start.contents));
      const segmentWidth = absVal(sub(s.start.contents[0], s.end.contents[0]));
      const thicknessWidth = absVal(
        ops.vmul(s.thickness.contents, ops.rot90(dir))[0]
      );
      const segmentHeight = absVal(sub(s.start.contents[1], s.end.contents[1]));
      const thicknessHeight = absVal(
        ops.vmul(s.thickness.contents, ops.rot90(dir))[1]
      );
      w = add(segmentWidth, thicknessWidth);
      h = add(segmentHeight, thicknessHeight);
      center = ops.vdiv(
        ops.vadd(s.start.contents, s.end.contents),
        constOf(2)
      ) as Pt2;
      break;
    case "Rectangle":
    case "Text":
    case "Image":
    case "Callout":
      w = s.w.contents;
      h = s.h.contents;
      center = s.center.contents;
      break;
    case "Square":
      w = s.side.contents;
      h = s.side.contents;
      center = s.center.contents;
      break;
    // TODO: incorporate thickness/width somehow????
    case "Polygon":
    case "FreeformPolygon":
    case "Polyline":
      const xVals = s.points.contents.map((p: Pt2) => p[0]);
      const yVals = s.points.contents.map((p: Pt2) => p[1]);
      const xMin = _.reduce(xVals, (v: VarAD, w: VarAD) => min(v, w))!;
      const xMax = _.reduce(xVals, (v: VarAD, w: VarAD) => max(v, w))!;
      const yMin = _.reduce(yVals, (v: VarAD, w: VarAD) => min(v, w))!;
      const yMax = _.reduce(yVals, (v: VarAD, w: VarAD) => max(v, w))!;
      w = sub(xMax, xMin);
      h = sub(yMax, yMin);
      center = [
        div(add(xMin, xMax), constOf(2)),
        div(add(yMin, yMax), constOf(2)),
      ];
      break;
    case "Path":
    case "PathString":
    default:
      throw new Error(`Shape with type ${t} doesn't support an overbox.`);
  }

  w = absVal(w);
  h = absVal(h);

  return bbox(w, h, center);
};

/**
 * Preconditions:
 *   If the input is line-like, it must be axis-aligned.
 *   Assumes line-like shapes are longer than they are thick.
 * Input: A rect- or line-like shape.
 * Output: A new BBox
 * Errors: Throws an error if the input shape is not rect- or line-like.
 */
export const underboxFromShape = (t: string, s: any): BBox => {
  let w, h: VarAD;
  let center: Pt2;
  switch (t) {
    case "Circle":
      w = mul(s.r.contents, constOf(Math.sqrt(2)));
      h = mul(s.r.contents, constOf(Math.sqrt(2)));
      center = s.center.contents;
      break;
    case "Ellipse":
      w = mul(s.rx.contents, constOf(Math.sqrt(2)));
      h = mul(s.ry.contents, constOf(Math.sqrt(2)));
      center = s.center.contents;
      break;
    // TODO: very crude approximation
    // TODO: maybe add a version when things are axis aligned
    case "Line":
    case "Arrow":
      const minDim = min(
        ops.vnorm(ops.vsub(s.start.contents, s.end.contents)),
        s.thickness.contents
      );
      w = minDim;
      h = minDim;
      center = ops.vdiv(
        ops.vadd(s.start.contents, s.end.contents),
        constOf(2)
      ) as Pt2;
      break;
    case "Rectangle":
    case "Text":
    case "Image":
    case "Callout":
      w = s.w.contents;
      h = s.h.contents;
      center = s.center.contents;
      break;
    case "Square":
      w = s.side.contents;
      h = s.side.contents;
      center = s.center.contents;
      break;
    // TODO: incorporate thickness/width somehow????
    case "Polygon":
    case "FreeformPolygon":
    case "Polyline":
    // break;
    case "Path":
    case "PathString":
    default:
      throw new Error(`Shape with type ${t} doesn't support an underbox.`);
  }

  w = absVal(w);
  h = absVal(h);

  return bbox(w, h, center);
};

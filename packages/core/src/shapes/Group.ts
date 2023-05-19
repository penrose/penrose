import * as ad from "../types/ad";
import { Named, ShapeCommon } from "../types/shapes";
import { ClipDataV, ShapeListV } from "../types/value";
import { boolV, clipDataV, noClip, shapeListV, strV } from "../utils/Util";
import { Canvas, Context } from "./Samplers";

export interface GroupProps<T> extends Named<T> {
  shapes: ShapeListV<T>;
  clipPath: ClipDataV<T>;
}

export const sampleGroup = (
  context: Context,
  canvas: Canvas
): GroupProps<ad.Num> => {
  return {
    name: strV("defaultGroup"),
    style: strV(""),
    ensureOnCanvas: boolV(true),
    shapes: shapeListV([]),
    clipPath: clipDataV(noClip()),
  };
};

export type Group<T> = ShapeCommon<T> & { shapeType: "Group" } & GroupProps<T>;

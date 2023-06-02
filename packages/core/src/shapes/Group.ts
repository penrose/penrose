import * as ad from "../types/ad.js";
import { Named, ShapeCommon } from "../types/shapes.js";
import { ClipDataV, ShapeListV } from "../types/value.js";
import { boolV, clipDataV, noClip, shapeListV, strV } from "../utils/Util.js";
import { Canvas, Context } from "./Samplers.js";

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

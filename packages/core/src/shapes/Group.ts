import * as ad from "../types/ad.js";
import { Named, ShapeCommon } from "../types/shapes.js";
import { ShapeListV } from "../types/value.js";
import { boolV, shapeListV, strV } from "../utils/Util.js";
import { Canvas, Context } from "./Samplers.js";

export interface GroupProps<T> extends Named<T> {
  shapes: ShapeListV<T>;
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
  };
};

export type Group<T> = ShapeCommon<T> & { shapeType: "Group" } & GroupProps<T>;

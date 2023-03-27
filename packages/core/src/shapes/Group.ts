import * as ad from "../types/ad";
import { Named, ShapeCommon, Transform } from "../types/shapes";
import { ShapeListV } from "../types/value";
import { boolV, id3x3, shapeListV, strV } from "../utils/Util";
import { Canvas, Context } from "./Samplers";

export interface GroupProps<T> extends Named<T>, Transform<T> {
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
    transform: id3x3(),
  };
};

export type Group<T> = ShapeCommon<T> & { shapeType: "Group" } & GroupProps<T>;

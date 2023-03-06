import * as ad from "../types/ad";
import { Named, Shape } from "../types/shapes";
import { ShapeListV } from "../types/value";
import { boolV, shapeListV, strV } from "../utils/Util";
import { Canvas, Context } from "./Samplers";

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

export type Group<T> = Shape<T> & { shapeType: "Group" } & GroupProps<T>;

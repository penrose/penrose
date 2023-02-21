import * as ad from "../types/ad";
import { Named, Shape } from "../types/shapes";
import { ShapeListV } from "../types/value";
import { boolV, shapeListV, strV } from "../utils/Util";
import { Canvas, Context } from "./Samplers";

export interface GroupProps extends Named {
  shapes: ShapeListV<ad.Num>;
}

export const sampleGroup = (context: Context, canvas: Canvas): GroupProps => {
  return {
    name: strV("defaultGroup"),
    style: strV(""),
    ensureOnCanvas: boolV(true),
    shapes: shapeListV([]),
  };
};

export type Group = Shape & { shapeType: "Group" } & GroupProps;

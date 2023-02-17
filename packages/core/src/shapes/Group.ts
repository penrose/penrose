import * as ad from "../types/ad";
import { Named, Shape } from "../types/shapes";
import { GPIListV } from "../types/value";
import { boolV, shapeListV, strV } from "../utils/Util";
import { Canvas, Context } from "./Samplers";

export interface GroupProps extends Named {
  shapes: GPIListV<ad.Num>;
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

export const makeGroup = (
  context: Context,
  canvas: Canvas,
  properties: Partial<GroupProps>
): Group => ({
  ...sampleGroup(context, canvas),
  ...properties,
  shapeType: "Group",
});

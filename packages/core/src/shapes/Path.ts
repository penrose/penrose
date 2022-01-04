import { constOf } from "engine/Autodiff";
import { bboxFromPath } from "engine/BBox";
import { VarAD } from "types/ad";
import { IPathDataV } from "types/value";
import { IArrow, IFill, INamed, IShape, IStroke, ShapeDef } from "./Shapes";
import {
  BoolV,
  Canvas,
  FloatV,
  PathDataV,
  sampleColor,
  sampleNoPaint,
  sampleStroke,
  StrV,
} from "./Samplers";

export interface IPath extends INamed, IStroke, IFill, IArrow {
  d: IPathDataV<VarAD>;
}

export const samplePath = (_canvas: Canvas): IPath => ({
  name: StrV("defaultPath"),
  strokeWidth: sampleStroke(),
  strokeStyle: StrV("solid"),
  strokeColor: sampleColor(),
  strokeDashArray: StrV(""),
  fillColor: sampleNoPaint(),
  arrowheadSize: FloatV(constOf(1)),
  arrowheadStyle: StrV("arrowhead-2"),
  startArrowhead: BoolV(false),
  endArrowhead: BoolV(false),
  d: PathDataV([]),
});

export type Path = IShape & { shapeType: "Path" } & IPath;

export const makePath = (canvas: Canvas, properties: Partial<IPath>): Path => ({
  ...samplePath(canvas),
  ...properties,
  shapeType: "Path",
});

export const Path = ShapeDef({
  sampler: samplePath,
  constr: makePath,

  bbox: bboxFromPath,
});

import { constOf } from "engine/Autodiff";
import { VarAD } from "types/ad";
import { INamed, IStroke, IFill, IArrow, IShape } from "types/shapes";
import { IPathDataV } from "types/value";
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
  style: StrV(""),
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
  ensureOnCanvas: BoolV(true),
});

export type Path = IShape & { shapeType: "Path" } & IPath;

export const makePath = (canvas: Canvas, properties: Partial<IPath>): Path => ({
  ...samplePath(canvas),
  ...properties,
  shapeType: "Path",
});

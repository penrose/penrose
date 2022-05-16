import * as ad from "types/ad";
import { IArrow, IFill, INamed, IShape, IStroke } from "types/shapes";
import { IPathDataV } from "types/value";
import {
  BoolV,
  Canvas,
  FloatV,
  PathDataV,
  sampleColor,
  sampleNoPaint,
  StrV,
} from "./Samplers";

export interface IPath extends INamed, IStroke, IFill, IArrow {
  d: IPathDataV<ad.Num>;
}

export const samplePath = (rng: seedrandom.prng, _canvas: Canvas): IPath => ({
  name: StrV("defaultPath"),
  style: StrV(""),
  strokeWidth: FloatV(1),
  strokeStyle: StrV("solid"),
  strokeColor: sampleColor(rng),
  strokeDasharray: StrV(""),
  fillColor: sampleNoPaint(),
  arrowheadSize: FloatV(1),
  arrowheadStyle: StrV("arrowhead-2"),
  startArrowhead: BoolV(false),
  endArrowhead: BoolV(false),
  d: PathDataV([]),
  ensureOnCanvas: BoolV(true),
});

export type Path = IShape & { shapeType: "Path" } & IPath;

export const makePath = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<IPath>
): Path => ({
  ...samplePath(rng, canvas),
  ...properties,
  shapeType: "Path",
});

import * as ad from "types/ad";
import { BoolV, ColorV, FloatV, PtListV, StrV, VectorV } from "./value";

//#region shape hierarchy interfaces
export interface Named {
  name: StrV;
  style: StrV; // TODO: very temporary; remove this and just use passthrough
  ensureOnCanvas: BoolV<ad.Num>;
}

export interface Stroke {
  strokeWidth: FloatV<ad.Num>;
  strokeStyle: StrV;
  strokeColor: ColorV<ad.Num>;
  strokeDasharray: StrV;
}

export interface Fill {
  fillColor: ColorV<ad.Num>;
}

export interface Center {
  center: VectorV<ad.Num>; // corresponds to (cx, cy), or other things, in SVG
}

export interface Rect {
  width: FloatV<ad.Num>;
  height: FloatV<ad.Num>;
}

export interface Arrow {
  arrowheadSize: FloatV<ad.Num>;
  arrowheadStyle: StrV;
  startArrowhead: BoolV<ad.Num>;
  endArrowhead: BoolV<ad.Num>;
}

export interface Corner {
  cornerRadius: FloatV<ad.Num>; // note: corresponds to rx in SVG
}

// TODO: don't use these
export interface Rotate {
  rotation: FloatV<ad.Num>; // about the top-left corner
}
export interface Scale {
  scale: FloatV<ad.Num>; // doesn't work correctly
}

export interface Poly {
  points: PtListV<ad.Num>;
}

export interface String {
  string: StrV;
  fontSize: StrV;
}

export interface Shape {
  shapeType: string;
}

//#endregion

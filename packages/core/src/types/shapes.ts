import {
  BoolV,
  ColorV,
  ConstStrV,
  FloatV,
  PtListV,
  StrV,
  VectorV,
} from "./value.js";

//#region shape hierarchy interfaces
export interface Named<T> {
  name: ConstStrV<T>;
  ensureOnCanvas: BoolV;
}

export interface Stroke<T> {
  strokeWidth: FloatV<T>;
  strokeStyle: StrV<T>;
  strokeColor: ColorV<T>;
  strokeDasharray: StrV<T>;
}

export interface Fill<T> {
  fillColor: ColorV<T>;
}

export interface Center<T> {
  center: VectorV<T>; // corresponds to (cx, cy), or other things, in SVG
}

export interface Rect<T> {
  width: FloatV<T>;
  height: FloatV<T>;
}

export interface Arrow<T> {
  startArrowheadSize: FloatV<T>;
  endArrowheadSize: FloatV<T>;
  startArrowhead: StrV<T>;
  endArrowhead: StrV<T>;
  flipStartArrowhead: BoolV;
}

export interface Corner<T> {
  cornerRadius: FloatV<T>; // note: corresponds to rx in SVG
}

// TODO: don't use these
export interface Rotate<T> {
  rotation: FloatV<T>; // about the top-left corner
}
export interface Scale<T> {
  scale: FloatV<T>; // doesn't work correctly
}

// // TODO: use this: https://github.com/penrose/penrose/issues/713
// export interface Transform {
//   matrix: MatrixV<ad.Num>;
// }

export interface Poly<T> {
  points: PtListV<T>;
}

export interface String<T> {
  string: StrV<T>;
  fontSize: StrV<T>;
}

export interface ShapeCommon<T> {
  shapeType: string;
  passthrough: Map<string, CanPassthrough<T>>;
}

export type CanPassthrough<T> = StrV<T> | FloatV<T>;

//#endregion

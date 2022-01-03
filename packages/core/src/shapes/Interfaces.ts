import * as BBox from "engine/BBox";
import { VarAD } from "types/ad";
import {
  IBoolV,
  IColorV,
  IFloatV,
  ILListV,
  IStrV,
  IVectorV,
} from "types/value";

export interface IShape {
  shapeType: string;
  bbox(): BBox.BBox;
}

export interface INamed {
  name: IStrV;
}

export interface IStroke {
  strokeWidth: IFloatV<VarAD>;
  strokeStyle: IStrV;
  strokeColor: IColorV<VarAD>;
  strokeDashArray: IStrV;
}

export interface IFill {
  fillColor: IColorV<VarAD>;
}

export interface ICenter {
  center: IVectorV<VarAD>; // corresponds to (cx, cy), or other things, in SVG
}

export interface IRect {
  width: IFloatV<VarAD>;
  height: IFloatV<VarAD>;
}

export interface IArrow {
  arrowheadSize: IFloatV<VarAD>;
  arrowheadStyle: IStrV;
  startArrowhead: IBoolV<VarAD>;
  endArrowhead: IBoolV<VarAD>;
}

export interface ICorner {
  cornerRadius: IFloatV<VarAD>; // note: corresponds to rx in SVG
}

// TODO: don't use these
export interface IRotate {
  rotation: IFloatV<VarAD>; // about the top-left corner
}
export interface IScale {
  scale: IFloatV<VarAD>; // doesn't work correctly
}

// // TODO: use this
// export interface ITransform {
//   matrix: IMatrixV<VarAD>;
// }

export interface IPoly {
  points: ILListV<VarAD>;
}

export interface IString {
  string: IStrV;
}

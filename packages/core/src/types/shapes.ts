import * as ad from "types/ad";
import { IBoolV, IColorV, IFloatV, IPtListV, IStrV, IVectorV } from "./value";

//#region shape hierarchy interfaces
export interface INamed {
  name: IStrV;
  style: IStrV; // TODO: very temporary; remove this and just use passthrough
  ensureOnCanvas: IBoolV<ad.Num>;
}

export interface IStroke {
  strokeWidth: IFloatV<ad.Num>;
  strokeStyle: IStrV;
  strokeColor: IColorV<ad.Num>;
  strokeDasharray: IStrV;
}

export interface IFill {
  fillColor: IColorV<ad.Num>;
}

export interface ICenter {
  center: IVectorV<ad.Num>; // corresponds to (cx, cy), or other things, in SVG
}

export interface IRect {
  width: IFloatV<ad.Num>;
  height: IFloatV<ad.Num>;
}

export interface IArrow {
  arrowheadSize: IFloatV<ad.Num>;
  arrowheadStyle: IStrV;
  startArrowhead: IBoolV<ad.Num>;
  endArrowhead: IBoolV<ad.Num>;
}

export interface ICorner {
  cornerRadius: IFloatV<ad.Num>; // note: corresponds to rx in SVG
}

// TODO: don't use these
export interface IRotate {
  rotation: IFloatV<ad.Num>; // about the top-left corner
}
export interface IScale {
  scale: IFloatV<ad.Num>; // doesn't work correctly
}

// // TODO: use this
// export interface ITransform {
//   matrix: IMatrixV<ad.Num>;
// }

export interface IPoly {
  points: IPtListV<ad.Num>;
}

export interface IString {
  string: IStrV;
  fontSize: IStrV;
}

export interface IShape {
  shapeType: string;
}

//#endregion

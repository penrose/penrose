import { VarAD } from "types/ad";
import { IBoolV, IColorV, IFloatV, IPtListV, IStrV, IVectorV } from "./value";

//#region shape hierarchy interfaces
export interface INamed {
  name: IStrV;
  style: IStrV; // TODO: very temporary; remove this and just use passthrough
  ensureOnCanvas: IBoolV<VarAD>;
}

export interface IStroke {
  strokeWidth: IFloatV<VarAD>;
  strokeStyle: IStrV;
  strokeColor: IColorV<VarAD>;
  strokeDasharray: IStrV;
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
  points: IPtListV<VarAD>;
}

export interface IString {
  string: IStrV;
  fontSize: IStrV;
}

export interface IShape {
  shapeType: string;
}

//#endregion

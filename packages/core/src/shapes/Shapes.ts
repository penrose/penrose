import * as BBox from "engine/BBox";
import { VarAD } from "types/ad";
import {
  IBoolV,
  IColorV,
  IFloatV,
  IPtListV,
  IStrV,
  IVectorV,
  Value,
} from "types/value";
import { Circle } from "./Circle";
import { Ellipse } from "./Ellipse";
import { Equation } from "./Equation";
import { Image } from "./Image";
import { Line } from "./Line";
import { Path } from "./Path";
import { Polygon } from "./Polygon";
import { Polyline } from "./Polyline";
import { Rectangle } from "./Rectangle";
import { Canvas } from "./Samplers";
import { Text } from "./Text";

//#region shape hierarchy interfaces

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
  points: IPtListV<VarAD>;
}

export interface IString {
  string: IStrV;
}

//#endregion

//#region other shape types/globals

export interface IShape {
  shapeType: string;
}

// TODO: fix this type, it's too restrictive
export interface Properties {
  [k: string]: Value<VarAD>;
}

export type Shape =
  | Circle
  | Ellipse
  | Equation
  | Image
  | Line
  | Path
  | Polygon
  | Polyline
  | Rectangle
  | Text;

export interface ShapeDef {
  sampler: (canvas: Canvas) => Properties;
  constr: (canvas: Canvas, properties: Properties) => Shape;

  // TODO: make these methods
  bbox: (properties: Properties) => BBox.BBox;
  isLinelike: boolean; // TODO: use type predicate instead
  isRectlike: boolean; // TODO: remove this
}

// hack to satisfy the typechecker
export const ShapeDef = (shapedef: {
  sampler: (canvas: Canvas) => unknown;
  constr: (canvas: Canvas, properties: Properties) => Shape;
  bbox: (properties: any) => BBox.BBox;
  isLinelike?: boolean;
  isRectlike?: boolean;
}): ShapeDef => ({
  sampler: (canvas) => <Properties>shapedef.sampler(canvas),
  constr: shapedef.constr,

  bbox: shapedef.bbox,
  isLinelike: shapedef.isLinelike || false,
  isRectlike: shapedef.isRectlike || false,
});

export const shapedefs: { [k in Shape["shapeType"]]: ShapeDef } = {
  Circle,
  Ellipse,
  Equation,
  Image,
  Line,
  Path,
  Polygon,
  Polyline,
  Rectangle,
  Text,
};

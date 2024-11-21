import { Num, Value } from "@penrose/core";
import { Diagram } from "./diagram.js";

export type Substance = Record<string, any>;
export type Type = () => Substance;
export type Predicate = ((...objs: any[]) => void) & {
  test: (...objs: any[]) => boolean;
};

export type Vec2 = [Num, Num];
export type Vec3 = [Num, Num, Num];
export type Vec4 = [Num, Num, Num, Num];
export type Color = Num[];
export type VecN = Num[];

export type DragConstraint = (
  [x, y]: [number, number],
  diagram: Diagram,
) => [number, number];

export interface ShapeCommon {
  name: string;
  ensureOnCanvas: boolean;
  interactiveOnly: boolean;
}

export interface Stroke {
  strokeWidth: Num;
  strokeStyle: string;
  strokeColor: Color;
  strokeDasharray: string;
}

export interface Fill {
  fillColor: Color;
}

export interface Center extends Drag {
  center: Vec2;
}

export interface Rect {
  width: Num;
  height: Num;
}

export interface Arrow {
  startArrowheadSize: Num;
  endArrowheadSize: Num;
  startArrowhead: string;
  endArrowhead: string;
  flipStartArrowhead: boolean;
}

export interface Corner {
  cornerRadius: Num;
}

export interface Rotate {
  rotation: Num;
}

export interface Scale {
  scale: Num;
}

export interface Poly extends Drag {
  points: Vec2[];
}

export interface String {
  string: string;
  fontSize: string;
}

export interface Drag {
  drag: boolean;
  dragConstraint: DragConstraint;
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
  | Text
  | Group;

export enum ShapeType {
  Circle = "Circle",
  Ellipse = "Ellipse",
  Equation = "Equation",
  Image = "Image",
  Line = "Line",
  Path = "Path",
  Polygon = "Polygon",
  Polyline = "Polyline",
  Rectangle = "Rectangle",
  Text = "Text",
  Group = "Group",
}

export type PathData = Value.PathCmd<Num>[];

export interface Circle extends ShapeCommon, Stroke, Fill, Center {
  shapeType: ShapeType.Circle;
  r: Num;
}

export interface Ellipse extends ShapeCommon, Stroke, Fill, Center {
  shapeType: ShapeType.Ellipse;
  rx: Num;
  ry: Num;
}

export interface Equation
  extends ShapeCommon,
    Fill,
    Center,
    Rect,
    Rotate,
    String,
    Drag {
  shapeType: ShapeType.Equation;
  ascent: Num;
  descent: Num;
}

export interface Image extends ShapeCommon, Center, Rect, Rotate {
  shapeType: ShapeType.Image;
  svg: string;
  preserveAspectRatio: string;
}

export interface Line extends ShapeCommon, Stroke, Arrow, Fill, Drag {
  shapeType: ShapeType.Line;
  start: Vec2;
  end: Vec2;
  strokeLinecap: string;
}

export interface Path extends ShapeCommon, Stroke, Fill, Arrow {
  shapeType: ShapeType.Path;
  d: PathData;
  strokeLinecap: string;
}

export interface Polygon extends ShapeCommon, Stroke, Fill, Scale, Poly {
  shapeType: ShapeType.Polygon;
}

export interface Polyline extends ShapeCommon, Stroke, Fill, Scale, Poly {
  shapeType: ShapeType.Polyline;
  strokeLinecap: string;
}

export interface Rectangle
  extends ShapeCommon,
    Stroke,
    Fill,
    Center,
    Rotate,
    Rect,
    Corner {
  shapeType: ShapeType.Rectangle;
}

export interface Text
  extends ShapeCommon,
    Stroke,
    Fill,
    Center,
    Rect,
    Rotate,
    String,
    Drag {
  shapeType: ShapeType.Text;
  visibility: string;
  fontFamily: string;
  fontSizeAdjust: string;
  fontStretch: string;
  fontStyle: string;
  fontVariant: string;
  fontWeight: string;
  textAnchor: string;
  lineHeight: string;
  alignmentBaseline: string;
  dominantBaseline: string;
  ascent: Num;
  descent: Num;
}

export interface Group extends ShapeCommon {
  shapeType: ShapeType.Group;
  shapes: Shape[];
  clipPath?: Exclude<Shape, Group>;
}

export type ShapeProps =
  | CircleProps
  | EllipseProps
  | EquationProps
  | ImageProps
  | LineProps
  | PathProps
  | PolygonProps
  | PolylineProps
  | RectangleProps
  | TextProps
  | GroupProps;

export type CircleProps = Omit<Circle, "shapeType">;
export type EllipseProps = Omit<Ellipse, "shapeType">;
export type EquationProps = Omit<Equation, "shapeType">;
export type ImageProps = Omit<Image, "shapeType">;
export type LineProps = Omit<Line, "shapeType">;
export type PathProps = Omit<Path, "shapeType">;
export type PolygonProps = Omit<Polygon, "shapeType">;
export type PolylineProps = Omit<Polyline, "shapeType">;
export type RectangleProps = Omit<Rectangle, "shapeType">;
export type TextProps = Omit<Text, "shapeType">;
export type GroupProps = Omit<Group, "shapeType">;

const PenroseCommonTypes = {
  name: "StrV",
  ensureOnCanvas: "BoolV",
};

const PenroseStrokeTypes = {
  strokeWidth: "FloatV",
  strokeStyle: "StrV",
  strokeColor: "ColorV",
  strokeDasharray: "StrV",
};

const PenroseFillTypes = {
  fillColor: "ColorV",
};

const PenroseCenterTypes = {
  center: "VectorV",
};

const PenroseRectTypes = {
  width: "FloatV",
  height: "FloatV",
};

const PenroseArrowTypes = {
  startArrowheadSize: "FloatV",
  endArrowheadSize: "FloatV",
  startArrowhead: "StrV",
  endArrowhead: "StrV",
  flipStartArrowhead: "BoolV",
};

const PenroseCornerTypes = {
  cornerRadius: "FloatV",
};

const PenroseRotateTypes = {
  rotation: "FloatV",
};

const PenroseScaleTypes = {
  scale: "FloatV",
};

const PenrosePolyTypes = {
  points: "PtListV",
};

const PenroseStringTypes = {
  string: "StrV",
  fontSize: "StrV",
};

const PenroseCircleTypes = {
  ...PenroseCommonTypes,
  ...PenroseStrokeTypes,
  ...PenroseFillTypes,
  ...PenroseCenterTypes,
  r: "FloatV",
};

const PenroseEllipseTypes = {
  ...PenroseCommonTypes,
  ...PenroseStrokeTypes,
  ...PenroseFillTypes,
  ...PenroseCenterTypes,
  rx: "FloatV",
  ry: "FloatV",
};

const PenroseEquationTypes = {
  ...PenroseCommonTypes,
  ...PenroseFillTypes,
  ...PenroseCenterTypes,
  ...PenroseRectTypes,
  ...PenroseRotateTypes,
  ...PenroseStringTypes,
  ascent: "FloatV",
  descent: "FloatV",
};

const PenroseImageTypes = {
  ...PenroseCommonTypes,
  ...PenroseCenterTypes,
  ...PenroseRectTypes,
  ...PenroseRotateTypes,
  href: "StrV",
  preserveAspectRatio: "StrV",
};

const PenroseLineTypes = {
  ...PenroseCommonTypes,
  ...PenroseStrokeTypes,
  ...PenroseFillTypes,
  ...PenroseArrowTypes,
  start: "VectorV",
  end: "VectorV",
  strokeLinecap: "StrV",
};

const PenrosePathTypes = {
  ...PenroseCommonTypes,
  ...PenroseStrokeTypes,
  ...PenroseFillTypes,
  ...PenroseArrowTypes,
  d: "StrV",
  strokeLinecap: "StrV",
};

const PenrosePolygonTypes = {
  ...PenroseCommonTypes,
  ...PenroseStrokeTypes,
  ...PenroseFillTypes,
  ...PenroseScaleTypes,
  ...PenrosePolyTypes,
};

const PenrosePolylineTypes = {
  ...PenroseCommonTypes,
  ...PenroseStrokeTypes,
  ...PenroseScaleTypes,
  ...PenrosePolyTypes,
  ...PenroseFillTypes,
  strokeLinecap: "StrV",
};

const PenroseRectangleTypes = {
  ...PenroseCommonTypes,
  ...PenroseStrokeTypes,
  ...PenroseFillTypes,
  ...PenroseCenterTypes,
  ...PenroseRotateTypes,
  ...PenroseRectTypes,
  ...PenroseCornerTypes,
};

const PenroseTextTypes = {
  ...PenroseCommonTypes,
  ...PenroseStrokeTypes,
  ...PenroseFillTypes,
  ...PenroseCenterTypes,
  ...PenroseRectTypes,
  ...PenroseRotateTypes,
  ...PenroseStringTypes,
  visibility: "StrV",
  fontFamily: "StrV",
  fontSizeAdjust: "StrV",
  fontStretch: "StrV",
  fontStyle: "StrV",
  fontVariant: "StrV",
  fontWeight: "StrV",
  textAnchor: "StrV",
  lineHeight: "StrV",
  alignmentBaseline: "StrV",
  dominantBaseline: "StrV",
  ascent: "FloatV",
  descent: "FloatV",
};

const PenroseGroupTypes = {
  ...PenroseCommonTypes,
  shapes: "ShapeListV",
  clipPath: "ClipDataV",
};

export const penroseShapeFieldTypes = new Map<ShapeType, any>([
  [ShapeType.Circle, PenroseCircleTypes],
  [ShapeType.Ellipse, PenroseEllipseTypes],
  [ShapeType.Equation, PenroseEquationTypes],
  [ShapeType.Image, PenroseImageTypes],
  [ShapeType.Line, PenroseLineTypes],
  [ShapeType.Path, PenrosePathTypes],
  [ShapeType.Polygon, PenrosePolygonTypes],
  [ShapeType.Polyline, PenrosePolylineTypes],
  [ShapeType.Rectangle, PenroseRectangleTypes],
  [ShapeType.Text, PenroseTextTypes],
  [ShapeType.Group, PenroseGroupTypes],
]);

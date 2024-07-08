import { Num, Value } from "@penrose/core";

export type Vec2 = [Num, Num];
export type Vec3 = [Num, Num, Num];
export type Color = Num[];
export type VecN = Num[];

export interface Named {
  name: string;
  ensureOnCanvas: boolean;
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

export interface Center {
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

export interface Poly {
  points: Vec2[];
}

export interface String {
  string: string;
  fontSize: string;
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
}

export type PathData = Value.PathCmd<Num>[];

export interface Circle extends Named, Stroke, Fill, Center {
  shapeType: ShapeType.Circle;
  r: Num;
}

export interface Ellipse extends Named, Stroke, Fill, Center {
  shapeType: ShapeType.Ellipse;
  rx: Num;
  ry: Num;
}

export interface Equation extends Named, Fill, Center, Rect, Rotate, String {
  shapeType: ShapeType.Equation;
  ascent: Num;
  descent: Num;
}

export interface Image extends Named, Center, Rect, Rotate {
  shapeType: ShapeType.Image;
  href: string;
  preserveAspectRatio: string;
}

export interface Line extends Named, Stroke, Arrow, Fill {
  shapeType: ShapeType.Line;
  start: Vec2;
  end: Vec2;
  strokeLinecap: string;
}

export interface Path extends Named, Stroke, Fill, Arrow {
  shapeType: ShapeType.Path;
  d: PathData;
  strokeLinecap: string;
}

export interface Polygon extends Named, Stroke, Fill, Scale, Poly {
  shapeType: ShapeType.Polygon;
}

export interface Polyline extends Named, Stroke, Scale, Poly {
  shapeType: ShapeType.Polyline;
  strokeLinecap: string;
}

export interface Rectangle
  extends Named,
    Stroke,
    Fill,
    Center,
    Rotate,
    Rect,
    Corner {
  shapeType: ShapeType.Rectangle;
}

export interface Text
  extends Named,
    Stroke,
    Fill,
    Center,
    Rect,
    Rotate,
    String {
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
  | TextProps;

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

const PenroseNamedTypes = {
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
  ...PenroseNamedTypes,
  ...PenroseStrokeTypes,
  ...PenroseFillTypes,
  ...PenroseCenterTypes,
  r: "FloatV",
};

const PenroseEllipseTypes = {
  ...PenroseNamedTypes,
  ...PenroseStrokeTypes,
  ...PenroseFillTypes,
  ...PenroseCenterTypes,
  rx: "FloatV",
  ry: "FloatV",
};

const PenroseEquationTypes = {
  ...PenroseNamedTypes,
  ...PenroseFillTypes,
  ...PenroseCenterTypes,
  ...PenroseRectTypes,
  ...PenroseRotateTypes,
  ...PenroseStringTypes,
  ascent: "FloatV",
  descent: "FloatV",
};

const PenroseImageTypes = {
  ...PenroseNamedTypes,
  ...PenroseCenterTypes,
  ...PenroseRectTypes,
  ...PenroseRotateTypes,
  href: "StrV",
  preserveAspectRatio: "StrV",
};

const PenroseLineTypes = {
  ...PenroseNamedTypes,
  ...PenroseStrokeTypes,
  ...PenroseFillTypes,
  ...PenroseArrowTypes,
  start: "VectorV",
  end: "VectorV",
  strokeLinecap: "StrV",
};

const PenrosePathTypes = {
  ...PenroseNamedTypes,
  ...PenroseStrokeTypes,
  ...PenroseFillTypes,
  ...PenroseArrowTypes,
  d: "StrV",
  strokeLinecap: "StrV",
};

const PenrosePolygonTypes = {
  ...PenroseNamedTypes,
  ...PenroseStrokeTypes,
  ...PenroseFillTypes,
  ...PenroseScaleTypes,
  ...PenrosePolyTypes,
};

const PenrosePolylineTypes = {
  ...PenroseNamedTypes,
  ...PenroseStrokeTypes,
  ...PenroseScaleTypes,
  ...PenrosePolyTypes,
  strokeLinecap: "StrV",
};

const PenroseRectangleTypes = {
  ...PenroseNamedTypes,
  ...PenroseStrokeTypes,
  ...PenroseFillTypes,
  ...PenroseCenterTypes,
  ...PenroseRotateTypes,
  ...PenroseRectTypes,
  ...PenroseCornerTypes,
};

const PenroseTextTypes = {
  ...PenroseNamedTypes,
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

export const PenroseShapeType = new Map<ShapeType, any>([
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
]);

import {
  Num,
  Circle as PenroseCircle,
  Ellipse as PenroseEllipse,
  Equation as PenroseEquation,
  Group as PenroseGroup,
  Image as PenroseImage,
  Line as PenroseLine,
  Path as PenrosePath,
  Polygon as PenrosePolygon,
  Polyline as PenrosePolyline,
  Rectangle as PenroseRectangle,
  Shape as PenroseShape,
  Text as PenroseText,
  SamplingContext,
  Var,
} from "@penrose/core";
import { Color, Vec2 } from "./types.js";

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

export interface Named {
  name: string;
}

export interface Stroke {
  strokeWidth: Num;
  strokeStyle: string;
  strokeColor: Color;
  stokeDasharray: string;
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

export interface Rotate {
  rotation: Num;
}

export interface String {
  string: string;
  fontSize: string;
}

export interface Arrow {
  startArrowheadSize: Num;
  startArrowhead: string;
  flipStartArrowhead: boolean;
  endArrowheadSize: Num;
  endArrowhead: string;
}

export interface Poly {
  points: Vec2[];
}

export interface Corner {
  cornerRadius: Num;
}

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

export interface Line extends Named, Stroke, Fill, Arrow {
  shapeType: ShapeType.Line;
  start: Vec2;
  end: Vec2;
  strokeLinecap: string;
}

export interface Path extends Named, Stroke, Fill, Arrow {
  shapeType: ShapeType.Path;
  d: string;
  strokeLinecap: string;
}

export interface Polygon extends Named, Stroke, Fill, Poly {
  shapeType: ShapeType.Polygon;
}

export interface Polyline extends Named, Stroke, Fill, Poly {
  shapeType: ShapeType.Polyline;
}

export interface Rectangle
  extends Named,
    Stroke,
    Fill,
    Center,
    Rect,
    Corner,
    Rotate {
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

export type ClipData =
  | { tag: "NoClip" }
  | { tag: "Clip"; shape: Exclude<Shape, Group> };

export interface Group extends Named {
  shapeType: ShapeType.Group;
  shapes: Shape[];
  clipPath: ClipData;
}

const floatVFields = new Set([
  "strokeWidth",
  "r",
  "rx",
  "ry",
  "ascent",
  "descent",
  "startArrowheadSize",
  "endArrowheadSize",
  "cornerRadius",
  "rotation",
]);

const vecVFields = new Set(["center", "start", "end", "points"]);
const colorVFields = new Set(["strokeColor", "fillColor"]);
const strVFields = new Set([
  "name",
  "strokeStyle",
  "strokeDasharray",
  "fillColor",
  "string",
  "fontSize",
  "href",
  "preserveAspectRatio",
  "strokeLinecap",
  "fontFamily",
  "fontSizeAdjust",
  "fontStretch",
  "fontStyle",
  "fontVariant",
  "fontWeight",
  "textAnchor",
  "lineHeight",
  "alignmentBaseline",
  "dominantBaseline",
]);

const pathDataVFields = new Set(["d"]);

export const toPenroseShape = (
  shape: Partial<Shape> & Pick<Shape, "shapeType">,
  context: SamplingContext,
): PenroseShape<Num> => {
  switch (shape.shapeType) {
    case ShapeType.Circle:

  }
}

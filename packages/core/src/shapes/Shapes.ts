import { add, div, maxN, minN, sub } from "../engine/AutodiffFunctions.js";
import * as BBox from "../engine/BBox.js";
import * as ad from "../types/ad.js";
import { isVar } from "../types/ad.js";
import { unwrap } from "../utils/Util.js";
import { Circle, CircleProps, sampleCircle } from "./Circle.js";
import { Ellipse, EllipseProps, sampleEllipse } from "./Ellipse.js";
import { Equation, EquationProps, sampleEquation } from "./Equation.js";
import { Group, GroupProps, sampleGroup } from "./Group.js";
import { Image, ImageProps, sampleImage } from "./Image.js";
import { Line, LineProps, sampleLine } from "./Line.js";
import { Path, PathProps, samplePath } from "./Path.js";
import { Polygon, PolygonProps, samplePolygon } from "./Polygon.js";
import { Polyline, PolylineProps, samplePolyline } from "./Polyline.js";
import { Rectangle, RectangleProps, sampleRectangle } from "./Rectangle.js";
import { Canvas, Context } from "./Samplers.js";
import { Text, TextProps, sampleText } from "./Text.js";
//#region other shape types/globals

export type Shape<T> =
  | Circle<T>
  | Ellipse<T>
  | Equation<T>
  | Image<T>
  | Line<T>
  | Path<T>
  | Polygon<T>
  | Polyline<T>
  | Rectangle<T>
  | Text<T>
  | Group<T>;

export type ShapeType = Shape<ad.Num>["shapeType"];
export type ShapeProps<T> =
  | CircleProps<T>
  | EllipseProps<T>
  | EquationProps<T>
  | ImageProps<T>
  | LineProps<T>
  | PathProps<T>
  | PolygonProps<T>
  | PolylineProps<T>
  | RectangleProps<T>
  | TextProps<T>
  | GroupProps<T>;

export const computeShapeBbox = (shape: Shape<ad.Num>): BBox.BBox => {
  switch (shape.shapeType) {
    case "Circle":
      return BBox.bboxFromCircle(shape);
    case "Ellipse":
      return BBox.bboxFromEllipse(shape);
    case "Equation":
    case "Image":
    case "Text":
      return BBox.bboxFromRectlike(shape);
    case "Line":
      return BBox.bboxFromLinelike.rose(
        shape.start.contents,
        shape.end.contents,
        shape.strokeWidth.contents,
      );
    case "Path":
      return BBox.bboxFromPath(shape);
    case "Polygon":
    case "Polyline":
      return BBox.bboxFromPolygon(shape);
    case "Rectangle":
      return BBox.bboxFromRect(shape);
    case "Group":
      return bboxFromGroup(shape);
  }
};

const shapeSampler: Map<
  string,
  (context: Context, canvas: Canvas) => ShapeProps<ad.Num>
> = new Map(
  Object.entries({
    Circle: sampleCircle,
    Ellipse: sampleEllipse,
    Equation: sampleEquation,
    Image: sampleImage,
    Line: sampleLine,
    Path: samplePath,
    Polygon: samplePolygon,
    Polyline: samplePolyline,
    Rectangle: sampleRectangle,
    Text: sampleText,
    Group: sampleGroup,
  }),
);

const bboxFromGroup = ({ shapes, clipPath }: GroupProps<ad.Num>): BBox.BBox => {
  const bboxes = shapes.contents.map((shape) => computeShapeBbox(shape));
  const xRanges = bboxes.map(BBox.xRange);
  const yRanges = bboxes.map(BBox.yRange);
  const minX = minN(xRanges.map((xRange) => xRange[0]));
  const maxX = maxN(xRanges.map((xRange) => xRange[1]));
  const minY = minN(yRanges.map((yRange) => yRange[0]));
  const maxY = maxN(yRanges.map((yRange) => yRange[1]));
  const width = sub(maxX, minX);
  const height = sub(maxY, minY);
  const centerX = div(add(minX, maxX), 2);
  const centerY = div(add(minY, maxY), 2);

  const bboxAllMembers = BBox.bbox(width, height, [centerX, centerY]);

  if (clipPath.contents.tag === "NoClip") {
    return bboxAllMembers;
  }

  const bboxClipPath = computeShapeBbox(clipPath.contents.contents);

  return BBox.intersectBbox.rose(bboxAllMembers, bboxClipPath);
};

export const shapeTypes = [
  "Circle",
  "Ellipse",
  "Equation",
  "Image",
  "Line",
  "Path",
  "Polygon",
  "Polyline",
  "Rectangle",
  "Text",
  "Group",
];

export const sampleShape = (
  shapeType: ShapeType,
  context: Context,
  canvas: Canvas,
): ShapeProps<ad.Num> => {
  const sampler = unwrap(
    shapeSampler.get(shapeType),
    () => `shapeType not in sampler: ${shapeType}`,
  );
  return sampler(context, canvas);
};

// TODO: don't use a type predicate for this
export const isShapeType = (shapeType: string): shapeType is ShapeType =>
  shapeSampler.has(shapeType);

export const isTranslatable = (shape: Shape<ad.Num>): boolean => {
  switch (shape.shapeType) {
    case "Circle":
    case "Ellipse":
    case "Equation":
    case "Image":
    case "Rectangle":
    case "Text":
      return isVar(shape.center.contents[0]) && isVar(shape.center.contents[1]);

    case "Line":
      return (
        isVar(shape.start.contents[0]) &&
        isVar(shape.start.contents[1]) &&
        isVar(shape.end.contents[0]) &&
        isVar(shape.end.contents[1])
      );

    case "Group":
    case "Path":
      return false;

    case "Polygon":
    case "Polyline":
      for (const point of shape.points.contents) {
        if (!isVar(point[0]) || !isVar(point[1])) {
          return false;
        }
      }
      return true;
  }
};

export const isScalable = (shape: Shape<ad.Num>): boolean => {
  switch (shape.shapeType) {
    case "Circle":
      return isVar(shape.r.contents);

    case "Ellipse":
      return isVar(shape.rx.contents) && isVar(shape.ry.contents);

    case "Image":
    case "Rectangle":
      return isVar(shape.width.contents) && isVar(shape.height.contents);

    case "Equation":
    case "Text":
    case "Group":
    case "Path":
    case "Line":
    case "Polygon":
    case "Polyline":
      return false;
  }
};

//#endregion

// Map between "tag" and corresponding component
import Circle from "shapes/Circle";
import Image from "shapes/Image";
import Label from "shapes/Label";
import LabelTransform from "shapes/LabelTransform";
import Rectangle from "shapes/Rectangle";
import Arc from "shapes/Arc";
import RectangleTransform from "shapes/RectangleTransform";
import CircleTransform from "shapes/CircleTransform";
import Ellipse from "shapes/Ellipse";
import EllipseTransform from "shapes/EllipseTransform";
import Square from "shapes/Square";
import SquareTransform from "shapes/SquareTransform";
import Path from "shapes/Path";
import CurveTransform from "shapes/CurveTransform";
import Arrow from "shapes/Arrow";
import Line from "shapes/Line";
import LineTransform from "shapes/LineTransform";
import Polygon from "shapes/Polygon";
import ImageTransform from "shapes/ImageTransform";
import ParallelogramTransform from "shapes/ParallelogramTransform";
import draggable from "./Draggable";

// prettier-ignore
export const staticMap = {
  Circle,
  Rectangle,
  Arc,
  RectangleTransform,
  CircleTransform,
  Ellipse,
  EllipseTransform,
  Polygon,
  Square,
  SquareTransform,
  "Text": Label,
  "TextTransform": LabelTransform,
  Path,
  CurveTransform,
  Arrow,
  Image,
  ImageTransform,
  LineTransform,
  Line,
  ParallelogramTransform
};

export const interactiveMap = { ...staticMap };
Object.keys(interactiveMap).forEach(
  (key: string) => (interactiveMap[key] = draggable(interactiveMap[key]))
);

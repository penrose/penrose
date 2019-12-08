// Map between "tag" and corresponding component
import Circle from "./Circle";
import Image from "./Image";
import Label from "./Label";
import LabelTransform from "./LabelTransform";
import Rectangle from "./Rectangle";
import Arc from "./Arc";
import RectangleTransform from "./RectangleTransform";
import CircleTransform from "./CircleTransform";
import Ellipse from "./Ellipse";
import EllipseTransform from "./EllipseTransform";
import Square from "./Square";
import SquareTransform from "./SquareTransform";
import Curve from "./Curve";
import CurveTransform from "./CurveTransform";
import Arrow from "./Arrow";
import Line from "./Line";
import LineTransform from "./LineTransform";
import Polygon from "./Polygon";
import ImageTransform from "./ImageTransform";
import ParallelogramTransform from "./ParallelogramTransform";

// prettier-ignore
const componentMap = {
  // "Circle": Circle,
  "Rectangle": Rectangle,
  "Arc": Arc,
  "RectangleTransform": RectangleTransform,
  "CircleTransform": CircleTransform,
  "Ellipse": Ellipse,
  "EllipseTransform": EllipseTransform,
  "Polygon": Polygon,
  "Square": Square,
  "SquareTransform" : SquareTransform,
  // "Text": Label,
  "TextTransform": LabelTransform,
  "Curve": Curve,
  "CurveTransform": CurveTransform,
  "Arrow": Arrow,
  "Image": Image,
  "ImageTransform": ImageTransform,
  "LineTransform": LineTransform,
  "Line": Line,
  "ParallelogramTransform": ParallelogramTransform
};

export const staticShapes = {
  Circle: Circle,
  Text: Label
};

export default componentMap;

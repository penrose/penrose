// Map between "tag" and corresponding component
import Circle from "./Circle";
import Image from "./Image";
import Label from "./Label";
import Rectangle from "./Rectangle";
import RectangleTransform from "./RectangleTransform";
import CircleTransform from "./CircleTransform";
import EllipseTransform from "./EllipseTransform";
import Square from "./Square";
import SquareTransform from "./SquareTransform";
import Curve from "./Curve";
import CurveTransform from "./CurveTransform";
import Arrow from "./Arrow";
import Line from "./Line";
import LineTransform from "./LineTransform";
import Polygon from "./Polygon";

// prettier-ignore
const componentMap = {
  "Circle": Circle,
  "Rectangle": Rectangle,
  "RectangleTransform": RectangleTransform,
  "CircleTransform": CircleTransform,
  "EllipseTransform": EllipseTransform,
  "Polygon": Polygon,
  "Square": Square,
  "SquareTransform" : SquareTransform,
  "Text": Label,
  "Curve": Curve,
  "CurveTransform": CurveTransform,
  "Arrow": Arrow,
  "Image": Image,
  "LineTransform": LineTransform,
  "Line": Line
};

export default componentMap;

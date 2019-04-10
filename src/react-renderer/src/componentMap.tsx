// Map between "tag" and corresponding component
import Circle from "./Circle";
import Image from "./Image";
import Label from "./Label";
import Rectangle from "./Rectangle";
import RectangleTransform from "./RectangleTransform";
import CircleTransform from "./CircleTransform";
import Square from "./Square";
import Curve from "./Curve";
import CurveTransform from "./CurveTransform";
import Arrow from "./Arrow";
import Line from "./Line";
import Polygon from "./Polygon";

// prettier-ignore
const componentMap = {
  "Circle": Circle,
  "Rectangle": Rectangle,
  "RectangleTransform": RectangleTransform,
  "CircleTransform": CircleTransform,
  "Polygon": Polygon,
  "Square": Square,
  "Text": Label,
  "Curve": Curve,
  "CurveTransform": CurveTransform,
  "Arrow": Arrow,
  "Image": Image,
  "Line": Line
};

export default componentMap;

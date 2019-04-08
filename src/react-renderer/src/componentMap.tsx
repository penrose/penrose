// Map between "tag" and corresponding component
import Circle from "./Circle";
import Image from "./Image";
import Label from "./Label";
import Rectangle from "./Rectangle";
import RectangleTransform from "./RectangleTransform";
import Square from "./Square";
import Curve from "./Curve";
import Arrow from "./Arrow";
import Line from "./Line";
import Polygon from "./Polygon";

// prettier-ignore
const componentMap = {
  "Circle": Circle,
  "Rectangle": Rectangle,
  "RectangleTransform": RectangleTransform,
  "Polygon": Polygon,
  "Square": Square,
  "Text": Label,
  "Curve": Curve,
  "Arrow": Arrow,
  "Image": Image,
  "Line": Line
};

export default componentMap;

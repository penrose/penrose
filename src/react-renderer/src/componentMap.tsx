// Map between "tag" and corresponding component
import Circle from "./Circle";
import Label from "./Label";
import Rectangle from "./Rectangle";
import Curve from "./Curve";
import Arrow from "./Arrow";
import Line from "./Line";

// prettier-ignore
const componentMap = {
  "Circle": Circle,
  "Rectangle": Rectangle,
  "Text": Label,
  "Curve": Curve,
  "Arrow": Arrow,
  "Line": Line
};

export default componentMap;

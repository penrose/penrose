// Map between "tag" and corresponding component
import Circle from "./Circle";
import Label from "./Label";
import Rectangle from "./Rectangle";
import Curve from "./Curve";

// prettier-ignore
const componentMap = {
  "Circle": Circle,
  "Rectangle": Rectangle,
  "Text": Label,
  "Curve": Curve
};

export default componentMap;

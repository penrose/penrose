// Map between "tag" and corresponding component
import Circle from "./Circle";
import Label from "./Label";
import Rectangle from "./Rectangle";

// prettier-ignore
const componentMap = {
  "Circle": Circle,
  "Rectangle": Rectangle,
  "Text": Label
};

export default componentMap;

import Circle from "./Circle";
import Label from "./Label";
import Rectangle from "./Rectangle";
// Map between "tag" and corresponding component
// TODO: turn off prettifier for this
const componentMap = {
  Circle,
  Rectangle,
  Text: Label
};

export default componentMap;

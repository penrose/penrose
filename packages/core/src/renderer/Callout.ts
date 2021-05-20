import {
  attrFill,
  attrRadiusX,
  attrRadiusY,
  attrStroke,
  attrTitle,
  attrWH,
  attrXY,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Callout = ({ shape, canvasSize }: ShapeProps) => {
  // TODO: Change this from "rect"
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "rect");
  attrXY(shape, canvasSize, elem);
  attrWH(shape, elem);
  attrFill(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);

  attrRadiusX(shape, elem);

  return elem;
};
export default Callout;

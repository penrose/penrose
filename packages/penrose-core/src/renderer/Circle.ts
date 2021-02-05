import {
  attrCenter,
  attrFill,
  attrRadius,
  attrStroke,
  attrTitle,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Circle = ({ shape, canvasSize }: ShapeProps) => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "circle");
  attrFill(shape, elem);
  attrCenter(shape, canvasSize, elem);
  attrRadius(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);

  return elem;
};
export default Circle;

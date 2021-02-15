import { attrFill, attrStroke, attrTitle, attrWH, attrXY } from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Rectangle = ({ shape, canvasSize }: ShapeProps) => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "rect");
  attrXY(shape, canvasSize, elem);
  attrWH(shape, elem);
  attrFill(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);
  return elem;
};
export default Rectangle;

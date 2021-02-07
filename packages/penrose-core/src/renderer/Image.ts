import { attrOpacity, attrWH, attrXY } from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Image = ({ shape, canvasSize }: ShapeProps) => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "image");
  const path = (shape.properties.path as IStrV<string>).contents;
  // TODO: will this work?
  elem.setAttribute("href", path);
  attrOpacity(shape, elem);
  attrXY(shape, canvasSize, elem);
  attrWH(shape, elem);
  return elem;
};
export default Image;

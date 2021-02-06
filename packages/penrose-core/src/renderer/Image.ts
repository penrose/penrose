import { attrOpacity, attrWH, attrXY } from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Image = ({ shape, canvasSize }: ShapeProps) => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "image");
  const path = shape.properties.path.contents;
  // TODO: will this work?
  elem.setAttribute("href", process.env.PUBLIC_URL + path);
  attrOpacity(shape, elem);
  attrXY(shape, canvasSize, elem);
  attrWH(shape, elem);
  return elem;
};
export default Image;

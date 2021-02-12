import { attrOpacity, attrTransformCoords, attrWH } from "./AttrHelper";
import { ShapeProps } from "./Renderer";
import images from "contrib/images.json";

const Image = ({ shape, canvasSize }: ShapeProps): SVGGElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const path = (shape.properties.path as IStrV<string>).contents;
  if (!(path in images)) {
    console.error(`Could not find image path ${path}`);
    return elem;
  }
  elem.innerHTML = images[path];
  attrOpacity(shape, elem);
  attrWH(shape, elem.firstChild as SVGElement);
  attrTransformCoords(shape, canvasSize, elem);
  return elem;
};
export default Image;

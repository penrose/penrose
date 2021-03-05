import {
  attrCenter,
  attrFill,
  attrRadii,
  attrRadiusX,
  attrRadiusY,
  attrStroke,
  attrTitle,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Ellipse = ({ shape, canvasSize }: ShapeProps) => {
  const elem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "ellipse"
  );
  attrFill(shape, elem);
  attrCenter(shape, canvasSize, elem);
  attrRadii(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);


  attrRadiusX(shape, elem);
  attrRadiusY(shape, elem);


  return elem;
};
export default Ellipse;

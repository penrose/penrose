import {
  attrNoFill,
  attrPathLength,
  attrPoints,
  attrStroke,
  attrTitle,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Polyline = ({ shape, canvasSize }: ShapeProps) => {
  const elem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "polyline"
  );

  attrNoFill(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);
  attrPoints(shape, elem);
  attrPathLength(shape,elem);

  return elem;
};
export default Polyline;

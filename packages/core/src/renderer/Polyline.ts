import {
  attrNoFill,
  attrPoints,
  attrScale,
  attrPolyCenter,
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
  attrPolyCenter(shape, canvasSize, elem);
  attrScale(shape, elem);

  return elem;
};
export default Polyline;

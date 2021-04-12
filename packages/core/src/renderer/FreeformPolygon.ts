import { attrFill, attrPoints, attrStroke, attrTitle } from "./AttrHelper";
import { ShapeProps } from "./Renderer";

// polygon without a calculated center
// to be used with ? values
const FreeformPolygon = ({ shape, canvasSize }: ShapeProps) => {
  const elem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "polygon"
  );

  attrFill(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);
  attrPoints(shape, elem);

  return elem;
};
export default FreeformPolygon;

import {
  attrFill,
  attrSide,
  attrSideCoords,
  attrStroke,
  attrTitle,
  attrRadiusX,
  attrRadiusY,
  attrRotation,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

// (old) COMBAK: Apparently our shapes don't deal with stroke opacity? All shapes should deal with stroke opacity

const Square = ({ shape, canvasSize }: ShapeProps) => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "rect");
  attrSide(shape, elem);
  attrFill(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);
  attrRotation(
    shape,
    shape.properties.center,
    shape.properties.side,
    shape.properties.side,
    canvasSize,
    elem
  );
  attrSideCoords(shape, canvasSize, elem);

  attrRadiusX(shape, elem);

  return elem;
};
export default Square;

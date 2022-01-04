import {
  attrFill,
  attrSide,
  attrSideCoords,
  attrStroke,
  attrTitle,
  attrRadiusX,
  attrRotation,
  attrAutoFillSvg,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

// (old) COMBAK: Apparently our shapes don't deal with stroke opacity? All shapes should deal with stroke opacity

const Square = ({ shape, canvasSize }: ShapeProps): SVGRectElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "rect");

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  attrToNotAutoMap.push(...attrSide(shape, elem));
  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));
  attrToNotAutoMap.push(...attrRotation(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrSideCoords(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrRadiusX(shape, elem));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default Square;

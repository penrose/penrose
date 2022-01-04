import {
  attrFill,
  attrSide,
  attrSideCoords,
  attrStroke,
  attrTitle,
  attrRadiusX,
  attrRadiusY,
  attrRotation,
  attrAutoFillSvg,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

// (old) COMBAK: Apparently our shapes don't deal with stroke opacity? All shapes should deal with stroke opacity

const Square = ({ shape, canvasSize }: ShapeProps): SVGRectElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "rect");
  console.debug('Rendering Square');

  // Keep track of which SVG attributes we map below
  const attrToNotAutoMap: string[] = [];

  attrToNotAutoMap.push(...attrSide(shape, elem));
  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));
  attrToNotAutoMap.push(...attrRotation(
    shape,
    shape.properties.center,
    shape.properties.side,
    shape.properties.side,
    canvasSize,
    elem
  ));
  attrToNotAutoMap.push(...attrSideCoords(shape, canvasSize, elem));

  attrToNotAutoMap.push(...attrRadiusX(shape, elem));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);
  console.debug('Rendering Square - Done');

  return elem;
};
export default Square;

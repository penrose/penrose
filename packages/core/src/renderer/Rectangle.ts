import {
  attrAutoFillSvg,
  attrFill,
  attrRadiusX,
  attrRadiusY,
  attrStroke,
  attrTitle,
  attrWH,
  attrXY,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Rectangle = ({ shape, canvasSize }: ShapeProps): SVGRectElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "rect");
  console.debug('Rendering Rectangle');

  // Keep track of which SVG attributes we map below
  const attrToNotAutoMap: string[] = [];

  attrToNotAutoMap.push(...attrXY(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrWH(shape, elem));
  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  attrToNotAutoMap.push(...attrRadiusX(shape, elem));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);
  console.debug('Rendering Rectangle - Done');

  return elem;
};
export default Rectangle;

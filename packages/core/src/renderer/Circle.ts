import {
  attrAutoFillSvg,
  attrCenter,
  attrFill,
  attrStroke,
  attrTitle,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Circle = ({ shape, canvasSize }: ShapeProps): SVGCircleElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "circle");

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Fill the output SVG attributes while keeping track of input properties mapped
  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrCenter(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default Circle;

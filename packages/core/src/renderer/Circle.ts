import {
  attrCenter,
  attrFill,
  attrRadius,
  attrStroke,
  attrTitle,
  attrAutoFillSvg
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Circle = ({ shape, canvasSize }: ShapeProps) : SVGCircleElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "circle");
  console.debug('Rendering Circle');

  // Keep track of which SVG attributes we map below
  const attrToNotAutoMap: string[] = [];

  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrCenter(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrRadius(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);
  console.debug('Rendering Circle - Done');

  return elem;
};
export default Circle;

import { Circle } from "../shapes/Circle.js";
import {
  attrAutoFillSvg,
  attrCenter,
  attrFill,
  attrStroke,
  attrTitle,
} from "./AttrHelper.js";
import { RenderProps } from "./Renderer.js";

const RenderCircle = (
  shape: Circle<number>,
  { canvasSize }: RenderProps
): SVGCircleElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "circle");

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Fill the output SVG attributes while keeping track of input properties mapped
  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrCenter(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  elem.setAttribute("r", shape.r.contents.toString());
  attrToNotAutoMap.push("r");

  // Directly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default RenderCircle;

import { Rectangle } from "../shapes/Rectangle.js";
import {
  attrAutoFillSvg,
  attrCornerRadius,
  attrFill,
  attrRotation,
  attrStroke,
  attrTitle,
  attrWH,
  attrXY,
} from "./AttrHelper.js";
import { RenderProps } from "./Renderer.js";

const RenderRectangle = (
  shape: Rectangle<number>,
  { canvasSize }: RenderProps,
): SVGRectElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "rect");

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  attrToNotAutoMap.push(...attrXY(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrWH(shape, elem));
  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));
  attrToNotAutoMap.push(...attrCornerRadius(shape, elem));
  attrToNotAutoMap.push(...attrRotation(shape, canvasSize, elem));

  // Directly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default RenderRectangle;

import { Polyline } from "../shapes/Polyline.js";
import {
  attrAutoFillSvg,
  attrFill,
  attrPolyPoints,
  attrScale,
  attrStroke,
  attrTitle,
} from "./AttrHelper.js";
import { RenderProps } from "./Renderer.js";

const RenderPolyline = (
  shape: Polyline<number>,
  { canvasSize, titleCache }: RenderProps,
): SVGPolylineElement => {
  const elem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "polyline",
  );

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem, titleCache));
  attrToNotAutoMap.push(...attrScale(shape, elem));
  attrToNotAutoMap.push(...attrPolyPoints(shape, canvasSize, elem));

  // Directly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default RenderPolyline;

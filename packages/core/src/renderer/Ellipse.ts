import { Ellipse } from "../shapes/Ellipse";
import {
  attrAutoFillSvg,
  attrCenter,
  attrFill,
  attrStroke,
  attrTitle,
} from "./AttrHelper";
import { RenderProps } from "./Renderer";

const RenderEllipse = (
  shape: Ellipse<number>,
  { canvasSize }: RenderProps
): SVGEllipseElement => {
  const elem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "ellipse"
  );

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrCenter(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  // Directly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default RenderEllipse;

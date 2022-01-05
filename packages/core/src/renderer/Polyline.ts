import {
  attrPoints,
  attrScale,
  attrPolyCenter,
  attrStroke,
  attrTitle,
  attrAutoFillSvg,
  attrFill,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Polyline = ({ shape, canvasSize }: ShapeProps): SVGPolylineElement => {
  const elem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "polyline"
  );

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));
  attrToNotAutoMap.push(...attrPoints(shape, elem));
  attrToNotAutoMap.push(...attrPolyCenter(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrScale(shape, elem));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default Polyline;

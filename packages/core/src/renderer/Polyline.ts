import {
  attrNoFill,
  attrPoints,
  attrScale,
  attrPolyCenter,
  attrStroke,
  attrTitle,
  attrAutoFillSvg,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Polyline = ({ shape, canvasSize }: ShapeProps): SVGPolylineElement => {
  const elem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "polyline"
  );
  console.debug('Rendering Polyline');

  // Keep track of which SVG attributes we map below
  const attrToNotAutoMap: string[] = [];

  attrToNotAutoMap.push(...attrNoFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));
  attrToNotAutoMap.push(...attrPoints(shape, elem));
  attrToNotAutoMap.push(...attrPolyCenter(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrScale(shape, elem));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);
  console.debug('Rendering Polyline - Done');

  return elem;
};
export default Polyline;

import {
  attrAutoFillSvg,
  attrFill,
  attrPoints,
  attrPolyCenter,
  attrScale,
  attrStroke,
  attrTitle,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Polygon = ({ shape, canvasSize }: ShapeProps): SVGPolygonElement => {
  const elem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "polygon"
  );
  console.debug('Rendering Polygon');

  // Keep track of which SVG attributes we map below
  const attrToNotAutoMap: string[] = [];

  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));
  attrToNotAutoMap.push(...attrPoints(shape, elem));
  attrToNotAutoMap.push(...attrPolyCenter(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrScale(shape, elem));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);
  console.debug('Rendering Polygon - Done');

  return elem;
};
export default Polygon;

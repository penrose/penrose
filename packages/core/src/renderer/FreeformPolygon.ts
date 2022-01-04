import { attrAutoFillSvg, attrFill, attrPoints, attrStroke, attrTitle } from "./AttrHelper";
import { ShapeProps } from "./Renderer";

// polygon without a calculated center
// to be used with ? values
const FreeformPolygon = ({ shape, canvasSize }: ShapeProps): SVGPolygonElement => {
  const elem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "polygon"
  );
  console.debug('Rendering FreeformPolygon');

  // Keep track of which SVG attributes we map below
  const attrToNotAutoMap: string[] = [];

  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));
  attrToNotAutoMap.push(...attrPoints(shape, elem));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);
  console.debug('Rendering FreeformPolygon - Done');
  
  return elem;
};
export default FreeformPolygon;

import {
  attrCenter,
  attrFill,
  attrRadiusX,
  attrRadiusY,
  attrStroke,
  attrTitle,
  attrAutoFillSvg
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Ellipse = ({ shape, canvasSize }: ShapeProps): SVGEllipseElement => {
  const elem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "ellipse"
  );
  console.debug('Rendering Ellipse');

  // Keep track of which SVG attributes we map below
  const attrToNotAutoMap: string[] = [];

  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrCenter(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  attrToNotAutoMap.push(...attrRadiusX(shape, elem));
  attrToNotAutoMap.push(...attrRadiusY(shape, elem));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);
  console.debug('Rendering Ellipse - Done');

  return elem;
};
export default Ellipse;

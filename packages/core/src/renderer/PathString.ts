import { IStrV } from "types/value";
import {
  attrStroke,
  attrTitle,
  attrFill,
  attrPathData,
  attrWH,
  attrXY,
  attrRotation,
  attrAutoFillSvg,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const PathString = ({ shape, canvasSize }: ShapeProps): SVGGElement => {
  console.debug('Rendering PathString');

  const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "path");

  // Keep track of which SVG attributes we map into elem
  const attrToNotAutoMap: string[] = [];

  attrToNotAutoMap.push(...attrRotation(
    shape,
    shape.properties.center,
    shape.properties.w,
    shape.properties.h,
    canvasSize,
    g
  ));

  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));
  attrToNotAutoMap.push(...attrPathData(shape, elem));

  attrToNotAutoMap.push(...attrWH(shape, svg));
  attrToNotAutoMap.push(...attrXY(shape, canvasSize, svg));

  const viewBox = shape.properties.viewBox as IStrV;
  svg.setAttribute("viewBox", viewBox.contents);
  attrToNotAutoMap.push('viewBox');

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  svg.appendChild(elem);
  g.appendChild(svg);

  console.debug('Rendering PathString - Done');

  return g;
};
export default PathString;

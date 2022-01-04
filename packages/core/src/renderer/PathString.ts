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
  const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "path");

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  attrToNotAutoMap.push(...attrRotation(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));
  attrToNotAutoMap.push(...attrPathData(shape, elem));
  attrToNotAutoMap.push(...attrWH(shape, svg));
  attrToNotAutoMap.push(...attrXY(shape, canvasSize, svg));

  const viewBox = shape.properties.viewBox as IStrV;
  svg.setAttribute("viewBox", viewBox.contents);
  attrToNotAutoMap.push("viewBox");

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  svg.appendChild(elem);
  g.appendChild(svg);

  return g;
};
export default PathString;

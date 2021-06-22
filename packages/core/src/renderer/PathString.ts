import { IStrV } from "types/value";
import {
  attrStroke,
  attrTitle,
  attrFill,
  attrPathData,
  attrWH,
  attrXY,
  attrPathLength,
  attrGeneric,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const PathString = ({ shape, canvasSize }: ShapeProps): SVGGElement => {
  const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "path");

  attrFill(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);
  attrPathData(shape, elem);
  attrPathLength(shape,elem);

  const viewBox = shape.properties.viewBox as IStrV;
  svg.setAttribute("viewBox", viewBox.contents);

  svg.appendChild(elem);
  g.appendChild(svg);


  attrGeneric(shape,elem);

  return g;
};
export default PathString;
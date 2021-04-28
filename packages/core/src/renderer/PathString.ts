import { IStrV } from "types/value";
import {
  attrStroke,
  attrTitle,
  attrFill,
  attrPathData,
  attrWH,
  attrXY,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const PathString = ({ shape, canvasSize }: ShapeProps): SVGSVGElement => {
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "path");

  attrFill(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);
  attrPathData(shape, elem);

  attrWH(shape, svg);
  attrXY(shape, canvasSize, svg);

  const viewBox = shape.properties.viewBox as IStrV<string>;
  svg.setAttribute("viewBox", viewBox.contents);

  svg.appendChild(elem);
  return svg;
};
export default PathString;

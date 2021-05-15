import { IStrV } from "types/value";
import {
  attrStroke,
  attrTitle,
  attrFill,
  attrPathData,
  attrWH,
  attrXY,
  attrRotation,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const PathString = ({ shape, canvasSize }: ShapeProps): SVGGElement => {
  const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "path");

  attrRotation(
    shape,
    shape.properties.center,
    shape.properties.w,
    shape.properties.h,
    canvasSize,
    g
  );

  attrFill(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);
  attrPathData(shape, elem);

  attrWH(shape, svg);
  attrXY(shape, canvasSize, svg);

  const viewBox = shape.properties.viewBox as IStrV<string>;
  svg.setAttribute("viewBox", viewBox.contents);

  svg.appendChild(elem);
  g.appendChild(svg);
  return g;
};
export default PathString;

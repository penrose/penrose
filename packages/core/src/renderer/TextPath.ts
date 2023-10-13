import { TextPath } from "../shapes/TextPath.js";
import {
  attrAutoFillSvg,
  attrFill,
  attrString,
  attrStroke,
  attrTitle,
} from "./AttrHelper.js";
import { toPathString } from "./Path.js";
import { RenderProps } from "./Renderer.js";

const RenderTextPath = (
  shape: TextPath<number>,
  { canvasSize }: RenderProps,
): SVGTextPathElement => {
  const elem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "textPath",
  );
  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));
  attrToNotAutoMap.push(...attrString(shape, elem));
  //   attrToNotAutoMap.push(...attrFont(shape, elem));

  // Assuming TextPath has a `path` property for path definition
  elem.setAttribute("startOffset", shape.startOffset.contents.toString());
  elem.setAttribute("method", shape.method.contents);
  elem.setAttribute("spacing", shape.spacing.contents);
  elem.setAttribute("path", toPathString(shape.path.contents, canvasSize));

  attrToNotAutoMap.push("startOffset", "method", "spacing");

  // Directly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};

export default RenderTextPath;

import { Image } from "../shapes/Image.js";
import {
  attrAutoFillSvg,
  attrRotation,
  attrTitle,
  attrTransformCoords,
  attrWH,
} from "./AttrHelper.js";
import * as notFound from "./not_found.js";
import { RenderProps } from "./Renderer.js";
import { makeIdsUnique } from "./util.js";

const RenderImage = async (
  shape: Image<number>,
  { canvasSize, pathResolver }: RenderProps
): Promise<SVGGElement> => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  const path = shape.href.contents;
  let rawSVG = await pathResolver(path);
  if (rawSVG === undefined) {
    console.error(`Could not resolve image path ${path}`);
    rawSVG = notFound.image;
  }
  attrToNotAutoMap.push("href");
  elem.innerHTML = rawSVG;
  // We assume the first svg element in the file is the one to display
  const svg = elem.querySelector("svg")!;
  // make sure the SVG has unique IDs so multiple diagrams can appear on the screen
  makeIdsUnique(elem, false);

  attrToNotAutoMap.push(...attrWH(shape, svg));
  attrToNotAutoMap.push(...attrRotation(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrTransformCoords(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));
  svg.setAttribute("preserveAspectRatio", shape.preserveAspectRatio.contents);
  attrToNotAutoMap.push("preserveAspectRatio");
  // Directly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default RenderImage;

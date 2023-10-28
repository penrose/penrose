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

export const RenderTextPath = (
  shape: TextPath<number>,
  { canvasSize }: RenderProps,
): SVGGElement => {
  // Create the group element
  const groupElem = document.createElementNS("http://www.w3.org/2000/svg", "g");

  // Generate a unique ID for linking textPath and path
  const uniqueID = `path_${Math.random().toString(36).substring(2, 15)}`;

  // Create the path element
  const pathElem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "path",
  );
  pathElem.setAttribute("id", uniqueID);
  pathElem.setAttribute("d", toPathString(shape.path.contents, canvasSize));
  pathElem.setAttribute("style", "visibility: hidden");

  // Append the path to the group
  groupElem.appendChild(pathElem);

  // Create the text element
  const textElem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "text",
  );

  // Create the textPath element
  const textPathElem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "textPath",
  );
  textPathElem.setAttribute("href", `#${uniqueID}`);

  textPathElem.setAttribute(
    "startOffset",
    shape.startOffset.contents.toString(),
  );
  textPathElem.setAttribute("method", shape.method.contents);
  textPathElem.setAttribute("spacing", shape.spacing.contents);

  const attrToNotAutoMap: string[] = [];
  attrToNotAutoMap.push(...attrFill(shape, textPathElem));
  attrToNotAutoMap.push(...attrStroke(shape, textPathElem));
  attrToNotAutoMap.push(...attrTitle(shape, textPathElem));
  attrToNotAutoMap.push(...attrString(shape, textPathElem));
  attrToNotAutoMap.push("startOffset", "method", "spacing");

  attrAutoFillSvg(shape, textPathElem, attrToNotAutoMap);

  textElem.appendChild(textPathElem);
  groupElem.appendChild(textElem);

  return groupElem;
};
export default RenderTextPath;

import { uniqueId } from "lodash";
import { StrV } from "types/value";
import {
  attrAutoFillSvg,
  attrRotation,
  attrTransformCoords,
  attrWH,
} from "./AttrHelper";
import * as notFound from "./not_found";
import { ShapeProps } from "./Renderer";

const Image = async ({
  shape,
  canvasSize,
  variation,
  pathResolver,
}: ShapeProps): Promise<SVGGElement> => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  const path = (shape.properties.href as StrV).contents;
  let rawSVG = await pathResolver(path);
  if (rawSVG === undefined) {
    console.error(`Could not resolve image path ${path}`);
    rawSVG = notFound.image;
  }
  attrToNotAutoMap.push("href");
  elem.innerHTML = rawSVG;
  // We assume the first svg element in the file is the one to display
  const svg = elem.querySelector("svg")!;
  const defs = svg.getElementsByTagName("defs");
  /**
   * HACK:
   * We generate Unique IDs because of potential collisions when multiple images
   * are integrated in one diagram.
   */
  if (defs.length > 0) {
    defs[0].querySelectorAll("*").forEach((node) => {
      if (node.id !== "") {
        const users = svg.querySelectorAll(
          `[*|href="#${node.id}"]:not([href])`
        );
        const uuid = uniqueId();
        users.forEach((user) => {
          const unique = `${uuid}-${
            (shape.properties.name as StrV).contents
          }-ns-${node.id}`;
          user.setAttributeNS(
            "http://www.w3.org/1999/xlink",
            "href",
            "#" + unique
          );
          node.setAttribute("id", unique);
        });
        // special case: fill urls
        const urlUsers = svg.querySelectorAll(`[fill="url(#${node.id})"]`);
        urlUsers.forEach((user, n) => {
          const unique = `${uuid}-${
            (shape.properties.name as StrV).contents
          }-${n}-ns-${node.id}`;
          node.setAttribute("id", unique);
          user.setAttribute("fill", `url(#${unique})`);
        });
      }
    });
  }
  attrToNotAutoMap.push(...attrWH(shape, svg));
  attrToNotAutoMap.push(...attrRotation(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrTransformCoords(shape, canvasSize, elem));

  // Directly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default Image;

import { IStrV } from "types/value";
import {
  attrAutoFillSvg,
  attrRotation,
  attrTransformCoords,
  attrWH,
} from "./AttrHelper";
import notFound from "./not_found.json";
import { ShapeProps } from "./Renderer";

const Image = async ({
  shape,
  canvasSize,
  pathResolver,
}: ShapeProps): Promise<SVGGElement> => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  const path = (shape.properties.href as IStrV).contents;
  let rawSVG = await pathResolver(path);
  if (rawSVG === undefined) {
    console.error(`Could not resolve image path ${path}`);
    rawSVG = notFound["image"] as string;
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
        // BUG: not matching on fill="url(#...)", only hrefs
        const users = svg.querySelectorAll(
          `[*|href="#${node.id}"]:not([href])`
        );
        users.forEach((user) => {
          const unique = `${(shape.properties.name as IStrV).contents}-ns-${
            node.id
          }`;
          user.setAttributeNS(
            "http://www.w3.org/1999/xlink",
            "href",
            "#" + unique
          );
          node.setAttribute("id", unique);
        });
      }
    });
  }
  attrToNotAutoMap.push(...attrWH(shape, svg));
  attrToNotAutoMap.push(...attrRotation(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrTransformCoords(shape, canvasSize, elem));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default Image;

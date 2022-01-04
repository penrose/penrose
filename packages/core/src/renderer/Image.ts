import {
  attrAutoFillSvg,
  attrOpacity,
  attrRotation,
  attrTransformCoords,
  attrWH,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";
import images from "contrib/images.json";
import { IStrV } from "types/value";

const Image = ({ shape, canvasSize }: ShapeProps): SVGGElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  console.debug('Rendering Image');

  // Keep track of which SVG attributes we map below
  const attrToNotAutoMap: string[] = [];

  const path = (shape.properties.path as IStrV).contents;
  if (!(path in images)) {
    console.error(`Could not find image path ${path}`);
    return elem;
  }
  attrToNotAutoMap.push('path');
  elem.innerHTML = images[path];
  const svg = elem.firstChild as SVGSVGElement;
  const defs = svg.getElementsByTagName("defs");
  /**
   * HACK:
   * We generate Unique IDs because of potential collisions when multiple images
   * are integrated in one diagram.
   */
  if (defs.length > 0) {
    defs[0].querySelectorAll("*").forEach((node: any) => {
      if (node.id !== "") {
        // BUG: not matching on fill="url(#...)", only hrefs
        const users = svg.querySelectorAll(
          `[*|href="#${node.id}"]:not([href])`
        );
        users.forEach((user: any) => {
          const unique = `${(shape.properties.name as IStrV).contents}-ns-${
            node.id
          }`;
          user.setAttributeNS(
            "http://www.w3.org/1999/xlink",
            "href",
            "#" + unique
          );
          node.setAttribute("id", unique);
          attrToNotAutoMap.push('id');
        });
      }
    });
  }
  attrToNotAutoMap.push(...attrOpacity(shape, svg));
  attrToNotAutoMap.push(...attrWH(shape, svg));
  attrToNotAutoMap.push(...attrTransformCoords(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrRotation(
    shape,
    shape.properties.center,
    shape.properties.w,
    shape.properties.h,
    canvasSize,
    elem
  ));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);
  console.debug('Rendering Image - Done');

  return elem;
};
export default Image;

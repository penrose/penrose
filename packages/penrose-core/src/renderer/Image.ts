import { attrOpacity, attrTransformCoords, attrWH } from "./AttrHelper";
import { ShapeProps } from "./Renderer";
import images from "contrib/images.json";

const Image = ({ shape, canvasSize }: ShapeProps): SVGGElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const path = (shape.properties.path as IStrV<string>).contents;
  if (!(path in images)) {
    console.error(`Could not find image path ${path}`);
    return elem;
  }
  elem.innerHTML = images[path];
  // HACK: generate unique ids
  const svg = elem.firstChild as SVGSVGElement;
  const defs = svg.getElementsByTagName("defs");
  if (defs.length > 0) {
    defs[0].querySelectorAll("*").forEach((node: any) => {
      if (node.id !== "") {
        // BUG: not matching on fill="url(#...)", only hrefs
        const users = svg.querySelectorAll(
          `[*|href="#${node.id}"]:not([href])`
        );
        users.forEach((user: any) => {
          const unique = `${
            (shape.properties.name as IStrV<string>).contents
          }-ns-${node.id}`;
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
  attrOpacity(shape, svg);
  attrWH(shape, svg);
  attrTransformCoords(shape, canvasSize, elem);
  return elem;
};
export default Image;

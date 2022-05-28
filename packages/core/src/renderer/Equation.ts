import { StrV } from "types/value";
import {
  attrAutoFillSvg,
  attrFill,
  attrRotation,
  attrTitle,
  attrTransformCoords,
  attrWH,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Equation = ({ shape, canvasSize, labels }: ShapeProps): SVGGElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  attrToNotAutoMap.push(...attrRotation(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrTransformCoords(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  const name = shape.properties.name as StrV;
  const retrievedLabel = labels.get(name.contents);
  attrToNotAutoMap.push("name");

  if (
    retrievedLabel &&
    retrievedLabel.tag === "EquationData" &&
    retrievedLabel.rendered
  ) {
    const renderedLabel = retrievedLabel.rendered;

    attrToNotAutoMap.push(
      ...attrFill(shape, renderedLabel.getElementsByTagName("g")[0])
    );
    attrToNotAutoMap.push(...attrWH(shape, renderedLabel as any));

    renderedLabel.getElementsByTagName("g")[0].setAttribute("stroke", "none");
    renderedLabel
      .getElementsByTagName("g")[0]
      .setAttribute("stroke-width", "0");
    const fontSize = shape.properties.fontSize as StrV;
    renderedLabel.setAttribute(
      "style",
      `font-size: ${fontSize.contents.toString()}`
    );
    elem.appendChild(renderedLabel);
    attrToNotAutoMap.push("fontSize");
  } else {
    const txt = document.createElementNS("http://www.w3.org/2000/svg", "text");
    txt.textContent = shape.properties.string.contents as string;
    elem.appendChild(txt);
    attrToNotAutoMap.push("string");
  }

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default Equation;

import { IStrV } from "types/value";
import { retrieveLabel } from "utils/CollectLabels";
import {
  attrAutoFillSvg,
  attrFill,
  attrRotation,
  attrTitle,
  attrTransformCoords,
  attrWH,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Label = ({ shape, canvasSize, labels }: ShapeProps): SVGGElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  console.debug('Rendering Label');

  // Keep track of which SVG attributes we map below
  const attrToNotAutoMap: string[] = [];

  attrToNotAutoMap.push(...attrRotation(
    shape,
    shape.properties.center,
    shape.properties.w,
    shape.properties.h,
    canvasSize,
    elem
  ));
  attrToNotAutoMap.push(...attrTransformCoords(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  const name = shape.properties.name as IStrV;
  const retrievedLabel = retrieveLabel(name.contents, labels);

  if (retrievedLabel && retrievedLabel.rendered) {
    const renderedLabel = retrievedLabel.rendered;

    attrToNotAutoMap.push(...attrFill(shape, renderedLabel.getElementsByTagName("g")[0]));
    attrToNotAutoMap.push(...attrWH(shape, renderedLabel as any));

    renderedLabel.getElementsByTagName("g")[0].setAttribute("stroke", "none");
    renderedLabel
      .getElementsByTagName("g")[0]
      .setAttribute("stroke-width", "0");
    const fontSize = shape.properties.fontSize as IStrV;
    renderedLabel.setAttribute(
      "style",
      `font-size: ${fontSize.contents.toString()}`
    );
    elem.appendChild(renderedLabel);
    attrToNotAutoMap.push('stroke-width','font-size','fontSize');
  } else {
    const txt = document.createElementNS("http://www.w3.org/2000/svg", "text");
    txt.textContent = shape.properties.string.contents as string;
    elem.appendChild(txt);
  }
  
  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);
  console.debug('Rendering Label - Done');

  return elem;
};
export default Label;

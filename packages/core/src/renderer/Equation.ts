import { getAdValueAsString } from "utils/Util";
import {
  attrAutoFillSvg,
  attrFill,
  attrFont,
  attrRotation,
  attrStroke,
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

  // Indicator: pre-rendered label was found
  let labelFound = false;

  const retrievedLabel = labels.get(getAdValueAsString(shape.properties.name));

  if (retrievedLabel && retrievedLabel.tag === "EquationData") {
    const renderedLabel = retrievedLabel.rendered.cloneNode(
      true
    ) as HTMLElement;
    const paths = renderedLabel.getElementsByTagName("path");

    // Map Width/Height, clear style
    attrToNotAutoMap.push(...attrWH(shape, renderedLabel));
    renderedLabel.removeAttribute("style");

    // Map Stroke and fill
    for (const path in paths) {
      const thisPath = paths[path];
      if (typeof thisPath === "object") {
        attrToNotAutoMap.push(...attrFill(shape, thisPath));
        attrToNotAutoMap.push(...attrStroke(shape, thisPath));
      }
    }

    // Font Size was handled in pre-rendering & has no effect here.
    attrToNotAutoMap.push("fontSize");

    // Append the element & indicate the rendered label was found
    elem.appendChild(renderedLabel);
    labelFound = true;
  }

  if (!labelFound) {
    // Fallback case: generate plain-text (non-rendered) label from string
    const txt = document.createElementNS("http://www.w3.org/2000/svg", "text");
    txt.textContent = getAdValueAsString(shape.properties.string);
    attrToNotAutoMap.push("string");
    elem.appendChild(txt);

    // Map the attributes we have
    attrToNotAutoMap.push(...attrFill(shape, elem));
    attrToNotAutoMap.push(...attrWH(shape, elem));
    attrToNotAutoMap.push(...attrStroke(shape, elem));
    attrToNotAutoMap.push(...attrFont(shape, elem));
  }

  // Directly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default Equation;

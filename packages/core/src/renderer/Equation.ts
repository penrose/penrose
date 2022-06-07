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

  // Indicator: label was rendered and found
  let labelFound = false;

  if (shape.properties.name.tag === "StrV") {
    const name = shape.properties.name;
    const retrievedLabel = labels.get(name.contents);

    if (retrievedLabel && retrievedLabel.tag === "EquationData") {
      const renderedLabel = retrievedLabel.rendered;
      const g = renderedLabel.getElementsByTagName("g")[0];
      const paths = g.getElementsByTagName("path");

      // Map Fill and Width/Height
      attrToNotAutoMap.push(...attrFill(shape, g));
      attrToNotAutoMap.push(...attrWH(shape, renderedLabel));

      // Map Stroke
      for (const path in paths) {
        const thisPath = paths[path];
        if (typeof thisPath === "object") {
          attrToNotAutoMap.push(...attrStroke(shape, thisPath));
        }
      }

      // Map Font Size
      if (shape.properties.fontSize.tag === "StrV") {
        renderedLabel.setAttribute(
          "style",
          `font-size: ${shape.properties.fontSize.contents}`
        );
        attrToNotAutoMap.push("fontSize");
      }
      elem.appendChild(renderedLabel);

      // Indicate the rendered label was found
      labelFound = true;
    }
  }

  if (!labelFound && shape.properties.string.tag === "StrV") {
    // Fallback case: generate plain-text (non-rendered) label from string
    const txt = document.createElementNS("http://www.w3.org/2000/svg", "text");
    txt.textContent = shape.properties.string.contents;
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

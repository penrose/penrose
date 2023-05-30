import { Equation } from "../shapes/Equation.js";
import { getAdValueAsString } from "../utils/Util.js";
import {
  attrAutoFillSvg,
  attrFill,
  attrRotation,
  attrTitle,
  attrTransformCoords,
  attrWH,
} from "./AttrHelper.js";
import { RenderProps } from "./Renderer.js";

const placeholderString = (
  label: string,
  canvasSize: [number, number],
  shape: Equation<number>
): SVGGElement => {
  const txt = document.createElementNS("http://www.w3.org/2000/svg", "text");
  txt.textContent = label;
  attrFill(shape, txt);
  attrWH(shape, txt);
  attrTransformCoords(shape, canvasSize, txt);
  return txt;
};

const RenderEquation = (
  shape: Equation<number>,
  renderOptions: RenderProps
): SVGGElement => {
  const { canvasSize, labels, texLabels } = renderOptions;

  if (texLabels) {
    // if equations are rendered as plain TeX strings, forward relevant props to a <text> element and surround the TeX string with $$
    const txt = placeholderString(
      `$${getAdValueAsString(shape.string)}$`,
      canvasSize,
      shape
    );
    return txt;
  }

  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  attrToNotAutoMap.push(...attrRotation(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrTransformCoords(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  const retrievedLabel = labels.get(getAdValueAsString(shape.name));

  // If pre-rendered label was found, render the label in a group
  if (retrievedLabel && retrievedLabel.tag === "EquationData") {
    // Clone the retrieved node first to avoid mutating existing labels
    const renderedLabel = retrievedLabel.rendered.cloneNode(
      true
    ) as HTMLElement;
    const g = renderedLabel.getElementsByTagName("g")[0];

    attrToNotAutoMap.push(...attrFill(shape, g));
    // Map Width/Height
    attrToNotAutoMap.push(...attrWH(shape, renderedLabel));

    g.setAttribute("stroke", "none");
    g.setAttribute("stroke-width", "0");
    const fontSize = shape.fontSize;
    renderedLabel.setAttribute("style", `font-size: ${fontSize.contents}`);

    // Append the element & indicate the rendered label was found
    elem.appendChild(renderedLabel);

    // Directly Map across any "unknown" SVG properties
    attrAutoFillSvg(shape, elem, attrToNotAutoMap);

    return elem;
  } else {
    // Fallback case: generate plain-text (non-rendered) label from string
    return placeholderString(
      getAdValueAsString(shape.string),
      canvasSize,
      shape
    );
  }
};
export default RenderEquation;

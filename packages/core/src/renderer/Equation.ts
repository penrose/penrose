import { Equation } from "../shapes/Equation.js";
import { getAdValueAsString, toScreen } from "../utils/Util.js";
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
  [x, y]: [number, number],
  shape: Equation<number>
): SVGGElement => {
  const txt = document.createElementNS("http://www.w3.org/2000/svg", "text");
  // fake font
  txt.textContent = label;
  attrFill(shape, txt);
  attrWH(shape, txt);
  txt.setAttribute("x", `${x}`);
  txt.setAttribute("y", `${y}`);
  txt.setAttribute("alignment-baseline", "alphabetic");
  txt.setAttribute("dominant-baseline", "alphabetic");
  txt.setAttribute("text-anchor", "middle");
  return txt;
};

const RenderEquation = (
  shape: Equation<number>,
  renderOptions: RenderProps
): SVGGElement => {
  const { canvasSize, labels, texLabels } = renderOptions;
  const { center } = shape;
  const [x, y] = toScreen([center.contents[0], center.contents[1]], canvasSize);

  if (texLabels) {
    // If equations are rendered as plain TeX strings, forward relevant props to a <text> element and surround the TeX string with $$
    // Since the `svg` TeX package render text with the center on the baseline, we shift the labels down by height/2 + descent
    const baselineY = y + shape.height.contents / 2 - shape.descent.contents;
    const txt = placeholderString(
      `$${getAdValueAsString(shape.string)}$`,
      [x, baselineY],
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
    return placeholderString(getAdValueAsString(shape.string), [x, y], shape);
  }
};
export default RenderEquation;

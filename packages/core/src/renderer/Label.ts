import { IStrV } from "types/value";
import { retrieveLabel } from "utils/CollectLabels";
import {
  attrFill,
  attrRotation,
  attrTitle,
  attrTransformCoords,
  attrWH,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Label = ({ shape, canvasSize, labels }: ShapeProps) => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  attrRotation(
    shape,
    shape.properties.center,
    shape.properties.w,
    shape.properties.h,
    canvasSize,
    elem
  );
  attrTransformCoords(shape, canvasSize, elem);
  attrTitle(shape, elem);
  const name = shape.properties.name as IStrV<string>;
  const retrievedLabel = retrieveLabel(name.contents, labels);
  if (retrievedLabel && retrievedLabel.rendered) {
    const renderedLabel = retrievedLabel.rendered;
    attrFill(shape, renderedLabel.getElementsByTagName("g")[0]);
    attrWH(shape, renderedLabel as any);
    renderedLabel.getElementsByTagName("g")[0].setAttribute("stroke", "none");
    renderedLabel
      .getElementsByTagName("g")[0]
      .setAttribute("stroke-width", "0");
    const fontSize = shape.properties.fontSize as IStrV<string>;
    renderedLabel.setAttribute(
      "style",
      `font-size: ${fontSize.contents.toString()}`
    );
    elem.appendChild(renderedLabel);
  } else {
    const txt = document.createElementNS("http://www.w3.org/2000/svg", "text");
    txt.textContent = shape.properties.string.contents as string;
    elem.appendChild(txt);
  }
  return elem;
};
export default Label;

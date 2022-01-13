import { IStrV, IVectorV } from "types/value";
import { retrieveLabel } from "utils/CollectLabels";
import { toScreen } from "utils/Util";
import {
  attrAutoFillSvg,
  attrFill,
  attrFont,
  attrRotation,
  attrString,
  attrStroke,
  attrTitle,
  attrWH,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Text = ({ shape, canvasSize, labels }: ShapeProps): SVGTextElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "text");

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  const center = shape.properties.center as IVectorV<number>;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);

  // Map/Fill the shape attributes while keeping track of input properties mapped
  // Directly render the text with [x, y] in screen coordinates without transforming them using `width` and `height`
  elem.setAttribute("x", x.toString());
  elem.setAttribute("y", y.toString());
  attrToNotAutoMap.push("x", "y");
  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));
  attrToNotAutoMap.push(...attrString(shape, elem));
  attrToNotAutoMap.push(...attrRotation(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrFont(shape, elem));

  // Get width/height of the text if available
  const name = shape.properties.name as IStrV;
  const retrievedLabel = retrieveLabel(name.contents, labels);
  if (retrievedLabel && retrievedLabel.tag === "TextData") {
    attrToNotAutoMap.push(...attrWH(shape, elem));
  }

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default Text;

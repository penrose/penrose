import { Text } from "../shapes/Text";
import { toScreen } from "../utils/Util";
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
import { RenderProps } from "./Renderer";

const RenderText = (
  shape: Text<number>,
  { canvasSize, labels }: RenderProps
): SVGTextElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "text");

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  attrToNotAutoMap.push("x", "y");
  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));
  attrToNotAutoMap.push(...attrString(shape, elem));
  attrToNotAutoMap.push(...attrRotation(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrFont(shape, elem));

  // Get width/height of the text if available
  const name = shape.name;
  const retrievedLabel = labels.get(name.contents);
  // Directly render the text with [x, y] in screen coordinates without transforming them using `width` and `height`
  const center = shape.center;
  const [x, y] = toScreen([center.contents[0], center.contents[1]], canvasSize);
  if (retrievedLabel && retrievedLabel.tag === "TextData") {
    // adjust the y-coordinate of the text center s.t. it's the center of the bbox
    // see https://user-images.githubusercontent.com/11740102/149545843-84406be2-b3dc-4294-b01f-26ef8a2098ee.png for an illustration
    const descent = retrievedLabel.descent.contents;
    const height = retrievedLabel.height.contents;
    const centerY = y + (height / 2 - descent);
    elem.setAttribute("x", x.toString());
    elem.setAttribute("y", centerY.toString());
    attrToNotAutoMap.push(...attrWH(shape, elem));
  } else {
    elem.setAttribute("x", x.toString());
    elem.setAttribute("y", y.toString());
  }

  // Directly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default RenderText;

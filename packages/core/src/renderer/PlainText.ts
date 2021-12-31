import {
  attrStyle,
  attrFill,
  attrStroke,
  attrString,
  attrTitle,
  attrFontFamily,
  attrFontSize,
  attrFontSizeAdjust,
  attrFontStretch,
  attrFontStyle,
  attrFontVariant,
  attrFontWeight,
  attrWH,
  attrXY,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const PlainText = ({ shape, canvasSize }: ShapeProps) => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "text");
  attrXY(shape, canvasSize, elem);
  attrStyle(shape, elem);
  attrFill(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);
  attrFontFamily(shape, elem);
  attrFontSize(shape, elem);
  attrFontSizeAdjust(shape, elem);
  attrFontStretch(shape, elem);
  attrFontStyle(shape, elem);
  attrFontVariant(shape, elem);
  attrFontWeight(shape, elem);
  attrString(shape, elem);

  return elem;
};
export default PlainText;

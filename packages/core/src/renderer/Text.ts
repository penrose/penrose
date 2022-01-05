import {
  attrXY,
  attrVisibility,
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
  attrTextAnchor,
  attrAlignmentBaseline,
  attrRotation,
  attrAutoFillSvg,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Text = ({ shape, canvasSize }: ShapeProps): SVGTextElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "text");

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  attrToNotAutoMap.push(...attrXY(shape, canvasSize, elem));
  attrToNotAutoMap.push(...attrVisibility(shape, elem));
  attrToNotAutoMap.push(...attrStyle(shape, elem));
  attrToNotAutoMap.push(...attrFill(shape, elem));
  attrToNotAutoMap.push(...attrStroke(shape, elem));
  attrToNotAutoMap.push(...attrTitle(shape, elem));
  attrToNotAutoMap.push(...attrFontFamily(shape, elem));
  attrToNotAutoMap.push(...attrFontSize(shape, elem));
  attrToNotAutoMap.push(...attrFontSizeAdjust(shape, elem));
  attrToNotAutoMap.push(...attrFontStretch(shape, elem));
  attrToNotAutoMap.push(...attrFontStyle(shape, elem));
  attrToNotAutoMap.push(...attrFontVariant(shape, elem));
  attrToNotAutoMap.push(...attrFontWeight(shape, elem));
  attrToNotAutoMap.push(...attrString(shape, elem));
  attrToNotAutoMap.push(...attrTextAnchor(shape, elem));
  attrToNotAutoMap.push(...attrAlignmentBaseline(shape, elem));
  attrToNotAutoMap.push(...attrRotation(shape, canvasSize, elem));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  /**
    * TODO This snippet correctly gets the bounding box, but
    * TODO doesn't work here since `shape` isn't passed by reference.
    * TODO Hence, we can't set the width/height of the GPI (for
    * TODO optimization).  Will have to put this somewhere else...
    *
    * Since the SVG hasn't been rendered yet, we temporarily
    * create an invisible SVG containing just this text
    * in order to determine its bounding box.
  var tempDiv = document.createElement("div");
  tempDiv.setAttribute("style", "position:absolute; visibility:hidden; width:0; height:0");
  var tempSvg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  tempSvg.appendChild(elem.cloneNode(true));
  tempDiv.appendChild(tempSvg);
  document.body.appendChild(tempDiv);
  var bbox = tempSvg.getBBox();
  document.body.removeChild(tempDiv);
  shape.w = bbox.width;
  shape.h = bbox.height;
  console.log( "text bbox width  : " + String(bbox.width) );
  console.log( "text bbox height : " + String(bbox.height) );
  */

  return elem;
};
export default Text;

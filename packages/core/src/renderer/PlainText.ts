import {
  attrWH,
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
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const PlainText = ({ shape, canvasSize }: ShapeProps) => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "text");
  attrXY(shape, canvasSize, elem);
  attrVisibility(shape, elem);
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
  attrTextAnchor(shape, elem);
  attrAlignmentBaseline(shape, elem);

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
export default PlainText;





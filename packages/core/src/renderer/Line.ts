import { IFloatV, IColorV, IStrV } from "types/value";
import { toSvgPaintProperty, toSvgOpacityProperty, toScreen } from "utils/Util";
import { arrowHead, makeRoomForArrows } from "./Arrow";
import { attrTitle, DASH_ARRAY } from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Line = ({ shape, canvasSize }: ShapeProps) => {
  const style = shape.properties.style.contents;
  const [[arrowSX, arrowSY], [arrowEX, arrowEY]] = makeRoomForArrows(shape);
  const [sx, sy] = toScreen([arrowSX, arrowSY], canvasSize);
  const [ex, ey] = toScreen([arrowEX, arrowEY], canvasSize);
  const path = `M ${sx} ${sy} L ${ex} ${ey}`;
  const color = toSvgPaintProperty((shape.properties.color as IColorV<number>).contents);
  const thickness = (shape.properties.thickness as IFloatV<number>).contents;
  const strokeDasharray = style === "dashed" ? "7, 5" : "";
  const opacity = toSvgOpacityProperty((shape.properties.color as IColorV<number>).contents);
  const leftArrowId = shape.properties.name.contents + "-leftArrowhead";
  const rightArrowId = shape.properties.name.contents + "-rightArrowhead";
  const arrowheadStyle = (shape.properties.arrowheadStyle as IStrV).contents;
  const arrowheadSize = (shape.properties.arrowheadSize as IFloatV<number>)
    .contents;

  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  elem.appendChild(
    arrowHead(leftArrowId, color, opacity, arrowheadStyle, arrowheadSize)
  );
  elem.appendChild(
    arrowHead(rightArrowId, color, opacity, arrowheadStyle, arrowheadSize)
  );
  const pathElem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "path"
  );
  pathElem.setAttribute("d", path);
  
  // Opacity and width only relevant if stroke is present
  if((shape.properties.color as IColorV<number>).contents.tag !== "NONE") {
    pathElem.setAttribute("stroke-opacity", opacity.toString());
    pathElem.setAttribute("stroke-width", thickness.toString());
  }
  pathElem.setAttribute("stroke", color);

  // factor out an AttrHelper
  if (
    "strokeDashArray" in shape.properties &&
    shape.properties.strokeDashArray.contents !== ""
  ) {
    pathElem.setAttribute(
      "stroke-dasharray",
      (shape.properties.strokeDashArray as IStrV).contents
    );
  } else if (shape.properties.style.contents === "dashed") {
    pathElem.setAttribute("stroke-dasharray", DASH_ARRAY.toString());
  }
  // TODO: dedup in AttrHelper (problem: thickness vs strokeWidth)
  if (shape.properties.leftArrowhead.contents === true) {
    pathElem.setAttribute("marker-start", `url(#${leftArrowId})`);
  }
  if (shape.properties.rightArrowhead.contents === true) {
    pathElem.setAttribute("marker-end", `url(#${rightArrowId})`);
  }
  elem.appendChild(pathElem);
  attrTitle(shape, elem);
  return elem;
};
export default Line;

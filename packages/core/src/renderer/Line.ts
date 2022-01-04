import { IFloatV, IColorV, IStrV } from "types/value";
import { toSvgPaintProperty, toSvgOpacityProperty, toScreen } from "utils/Util";
import { arrowHead, makeRoomForArrows } from "./Arrow";
import { attrAutoFillSvg, attrTitle, DASH_ARRAY } from "./AttrHelper";
import { ShapeProps } from "./Renderer";

const Line = ({ shape, canvasSize }: ShapeProps): SVGGElement => {
  const [[arrowSX, arrowSY], [arrowEX, arrowEY]] = makeRoomForArrows(shape);
  const [sx, sy] = toScreen([arrowSX, arrowSY], canvasSize);
  const [ex, ey] = toScreen([arrowEX, arrowEY], canvasSize);
  const path = `M ${sx} ${sy} L ${ex} ${ey}`;
  const color = toSvgPaintProperty((shape.properties.color as IColorV<number>).contents);
  const thickness = (shape.properties.thickness as IFloatV<number>).contents;
  const opacity = toSvgOpacityProperty((shape.properties.color as IColorV<number>).contents);
  const leftArrowId = shape.properties.name.contents + "-leftArrowhead";
  const rightArrowId = shape.properties.name.contents + "-rightArrowhead";
  const arrowheadStyle = (shape.properties.arrowheadStyle as IStrV).contents;
  const arrowheadSize = (shape.properties.arrowheadSize as IFloatV<number>)
    .contents;

  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  console.debug('Rendering Line');

  // Keep track of which SVG attributes we map below
  const attrToNotAutoMap: string[] = [];

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
  attrToNotAutoMap.push('d');
  
  // Opacity and width only relevant if stroke is present
  if((shape.properties.color as IColorV<number>).contents.tag !== "NONE") {
    pathElem.setAttribute("stroke-opacity", opacity.toString());
    pathElem.setAttribute("stroke-width", thickness.toString());
    attrToNotAutoMap.push('stroke-opacity','stroke-width','color','thickness');
  }
  pathElem.setAttribute("stroke", color);
  attrToNotAutoMap.push('stroke');

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
  attrToNotAutoMap.push('strokeDashArray','stroke-dasharray');

  if (
    "strokeLineCap" in shape.properties &&
    shape.properties.strokeLineCap.contents !== ""
  ) {
    pathElem.setAttribute(
      "stroke-linecap",
      (shape.properties.strokeLineCap as IStrV).contents
    );
  } else {
    pathElem.setAttribute("stroke-linecap", "butt"); // same default as SVG
  }
  attrToNotAutoMap.push('strokeLineCap','stroke-linecap');

  // TODO: dedup in AttrHelper (problem: thickness vs strokeWidth)
  if (shape.properties.leftArrowhead.contents === true) {
    pathElem.setAttribute("marker-start", `url(#${leftArrowId})`);
    attrToNotAutoMap.push('leftArrow','marker-start');
  }
  if (shape.properties.rightArrowhead.contents === true) {
    pathElem.setAttribute("marker-end", `url(#${rightArrowId})`);
    attrToNotAutoMap.push('rightArrow','marker-end');
  }
  elem.appendChild(pathElem);
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);
  console.debug('Rendering Line - Done');

  return elem;
};
export default Line;

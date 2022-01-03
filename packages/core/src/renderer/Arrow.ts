import { Shape } from "types/shape";
import { IFloatV, IStrV, IColorV, IVectorV } from "types/value";
import { arrowheads, round2, toSvgPaintProperty, toSvgOpacityProperty, toScreen } from "utils/Util";
import { attrFill, attrTitle, DASH_ARRAY } from "./AttrHelper";
import { ShapeProps } from "./Renderer";

export const arrowHead = (
  id: string,
  color: string,
  opacity: number,
  style: string,
  size: number
) => {
  const arrow = arrowheads[style];
  const marker = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "marker"
  );
  marker.setAttribute("id", id);
  marker.setAttribute("markerUnits", "strokeWidth");
  marker.setAttribute("markerWidth", round2(arrow.width * size).toString());
  marker.setAttribute("markerHeight", round2(arrow.height * size).toString());
  marker.setAttribute("viewBox", arrow.viewbox);
  marker.setAttribute("refX", arrow.refX.toString());
  marker.setAttribute("refY", arrow.refY.toString());
  marker.setAttribute("orient", "auto-start-reverse");
  const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
  path.setAttribute("d", arrow.path);
  path.setAttribute("fill", color);
  path.setAttribute("fill-opacity", opacity.toString());
  marker.appendChild(path);
  return marker;
};

export const makeRoomForArrows = (shape: Shape) => {
  const [lineSX, lineSY] = (shape.properties.start as IVectorV<number>)
    .contents as [number, number];
  const [lineEX, lineEY] = (shape.properties.end as IVectorV<number>)
    .contents as [number, number];

  const arrowheadStyle = (shape.properties.arrowheadStyle as IStrV).contents;
  const arrowheadSize = (shape.properties.arrowheadSize as IFloatV<number>)
    .contents;
  const thickness = (shape.properties.thickness as IFloatV<number>).contents;

  // height * size = Penrose computed arrow size
  // multiplied by thickness since the arrow size uses markerUnits, which is strokeWidth by default:
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerUnits
  const arrowHeight =
    arrowheads[arrowheadStyle].height * arrowheadSize * thickness;
  const length = Math.sqrt((lineSX - lineEX) ** 2 + (lineSY - lineEY) ** 2);

  // Subtract off the arrowHeight from each side.
  // See https://math.stackexchange.com/a/2045181 for a derivation.
  let arrowSX, arrowSY;
  if (shape.shapeType === "Line" && shape.properties.leftArrowhead.contents) {
    [arrowSX, arrowSY] = [
      lineSX - (arrowHeight / length) * (lineSX - lineEX),
      lineSY - (arrowHeight / length) * (lineSY - lineEY),
    ];
  } else {
    [arrowSX, arrowSY] = [lineSX, lineSY];
  }

  let arrowEX, arrowEY;
  if (
    shape.shapeType === "Arrow" ||
    (shape.shapeType === "Line" && shape.properties.rightArrowhead.contents)
  ) {
    [arrowEX, arrowEY] = [
      lineEX - (arrowHeight / length) * (lineEX - lineSX),
      lineEY - (arrowHeight / length) * (lineEY - lineSY),
    ];
  } else {
    [arrowEX, arrowEY] = [lineEX, lineEY];
  }

  return [
    [arrowSX, arrowSY],
    [arrowEX, arrowEY],
  ];
};

const Arrow = ({ shape, canvasSize }: ShapeProps) => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  elem.setAttribute("pointer-events", "bounding-box");
  const id = `arrowhead_${shape.properties.name.contents}`;
  const color = toSvgPaintProperty((shape.properties.color as IColorV<number>).contents);
  const arrowheadSize = (shape.properties.arrowheadSize as IFloatV<number>)
    .contents;
  const arrowheadStyle = (shape.properties.arrowheadStyle as IStrV).contents;
  const alpha = toSvgOpacityProperty((shape.properties.color as IColorV<number>).contents);
  elem.appendChild(arrowHead(id, color, alpha, arrowheadStyle, arrowheadSize));
  const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
  const [[arrowSX, arrowSY], [arrowEX, arrowEY]] = makeRoomForArrows(shape);
  const [sx, sy] = toScreen([arrowSX, arrowSY], canvasSize);
  const [ex, ey] = toScreen([arrowEX, arrowEY], canvasSize);

  path.setAttribute("d", `M ${sx} ${sy} L ${ex} ${ey}`);
  path.setAttribute("marker-end", `url(#${id})`);
  path.setAttribute("stroke-opacity",alpha.toString())
  path.setAttribute("stroke", color);
  // factor out an AttrHelper
  if (
    "strokeDashArray" in shape.properties &&
    shape.properties.strokeDashArray.contents !== ""
  ) {
    elem.setAttribute(
      "stroke-dasharray",
      (shape.properties.strokeDashArray as IStrV).contents
    );
  } else if (shape.properties.style.contents === "dashed") {
    elem.setAttribute("stroke-dasharray", DASH_ARRAY.toString());
  }

  if (
    "strokeLineCap" in shape.properties &&
    shape.properties.strokeLineCap.contents !== ""
  ) {
    elem.setAttribute(
      "stroke-linecap",
      (shape.properties.strokeLineCap as IStrV).contents
    );
  } else {
    elem.setAttribute("stroke-linecap", "butt");
  }

  path.setAttribute(
    "stroke-width",
    shape.properties.thickness.contents.toString()
  );
  elem.appendChild(path);
  attrTitle(shape, elem);
  return elem;
};
export default Arrow;

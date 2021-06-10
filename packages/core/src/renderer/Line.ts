import { IVectorV, IFloatV, IColorV, IStrV } from "types/value";
import { arrowheads, toHex, toScreen } from "utils/Util";
import { arrowHead } from "./Arrow";
import { attrTitle, DASH_ARRAY } from "./AttrHelper";
import { ShapeProps } from "./Renderer";
import { Shape } from "types/shape";

const makeRoomForArrows = (shape: Shape) => {
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

  //   TODO: these should only happen if there is actually an arrow on that side
  // subtract off a the arrowHeight from each side

  const [arrowSX, arrowSY] = shape.properties.leftArrowhead.contents
    ? [
        lineSX - (arrowHeight / length) * (lineSX - lineEX),
        lineSY - (arrowHeight / length) * (lineSY - lineEY),
      ]
    : [lineSX, lineSY];
  const [arrowEX, arrowEY] = shape.properties.rightArrowhead.contents
    ? [
        lineEX - (arrowHeight / length) * (lineEX - lineSX),
        lineEY - (arrowHeight / length) * (lineEY - lineSY),
      ]
    : [lineEX, lineEY];

  return [
    [arrowSX, arrowSY],
    [arrowEX, arrowEY],
  ];
};

const Line = ({ shape, canvasSize }: ShapeProps) => {
  const style = shape.properties.style.contents;
  const [[arrowSX, arrowSY], [arrowEX, arrowEY]] = makeRoomForArrows(shape);
  const [sx, sy] = toScreen([arrowSX, arrowSY], canvasSize);
  const [ex, ey] = toScreen([arrowEX, arrowEY], canvasSize);
  const path = `M ${sx} ${sy} L ${ex} ${ey}`;
  const color = toHex(shape.properties.color.contents);
  const thickness = (shape.properties.thickness as IFloatV<number>).contents;
  const strokeDasharray = style === "dashed" ? "7, 5" : "";
  const opacity = (shape.properties.color as IColorV<number>).contents
    .contents[3];
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
  pathElem.setAttribute("fill-opacity", opacity.toString());
  pathElem.setAttribute("stroke-opacity", opacity.toString());
  pathElem.setAttribute("stroke", color);
  pathElem.setAttribute("stroke-width", thickness.toString());
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

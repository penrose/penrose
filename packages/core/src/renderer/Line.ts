import { Shape } from "types/shape";
import { ColorV, FloatV, StrV, VectorV } from "types/value";
import {
  arrowheads,
  round2,
  toScreen,
  toSvgOpacityProperty,
  toSvgPaintProperty,
} from "utils/Util";
import { attrAutoFillSvg, attrTitle, DASH_ARRAY } from "./AttrHelper";
import { ShapeProps } from "./Renderer";

export const arrowHead = (
  id: string,
  color: string,
  opacity: number,
  style: string,
  size: number
): SVGMarkerElement => {
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

const makeRoomForArrows = (shape: Shape): [number[][], string[]] => {
  // Keep a list of which input properties we programatically mapped
  const attrMapped: string[] = [];

  const [lineSX, lineSY] = (shape.properties.start as VectorV<number>)
    .contents as [number, number];
  const [lineEX, lineEY] = (shape.properties.end as VectorV<number>)
    .contents as [number, number];

  const arrowheadStyle = (shape.properties.arrowheadStyle as StrV).contents;
  const arrowheadSize = (shape.properties.arrowheadSize as FloatV<number>)
    .contents;
  const thickness = (shape.properties.strokeWidth as FloatV<number>).contents;
  attrMapped.push(
    "start",
    "end",
    "arrowheadStyle",
    "arrowheadSize",
    "strokeWidth"
  );

  // height * size = Penrose computed arrow size
  // multiplied by thickness since the arrow size uses markerUnits, which is strokeWidth by default:
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerUnits
  const arrowHeight =
    arrowheads[arrowheadStyle].height * arrowheadSize * thickness;
  const length = Math.sqrt((lineSX - lineEX) ** 2 + (lineSY - lineEY) ** 2);

  // Subtract off the arrowHeight from each side.
  // See https://math.stackexchange.com/a/2045181 for a derivation.
  let arrowSX, arrowSY;
  if (shape.properties.startArrowhead.contents) {
    [arrowSX, arrowSY] = [
      lineSX - (arrowHeight / length) * (lineSX - lineEX),
      lineSY - (arrowHeight / length) * (lineSY - lineEY),
    ];
  } else {
    [arrowSX, arrowSY] = [lineSX, lineSY];
  }
  attrMapped.push("startArrowhead");

  let arrowEX, arrowEY;
  if (shape.properties.endArrowhead.contents) {
    [arrowEX, arrowEY] = [
      lineEX - (arrowHeight / length) * (lineEX - lineSX),
      lineEY - (arrowHeight / length) * (lineEY - lineSY),
    ];
  } else {
    [arrowEX, arrowEY] = [lineEX, lineEY];
  }
  attrMapped.push("endArrowhead");

  return [
    [
      [arrowSX, arrowSY],
      [arrowEX, arrowEY],
    ],
    attrMapped,
  ];
};

const Line = ({ shape, canvasSize }: ShapeProps): SVGGElement => {
  const [[[arrowSX, arrowSY], [arrowEX, arrowEY]], attrToNotAutoMap] =
    makeRoomForArrows(shape);
  const [sx, sy] = toScreen([arrowSX, arrowSY], canvasSize);
  const [ex, ey] = toScreen([arrowEX, arrowEY], canvasSize);
  const path = `M ${sx} ${sy} L ${ex} ${ey}`;
  const color = toSvgPaintProperty(
    (shape.properties.strokeColor as ColorV<number>).contents
  );
  const thickness = (shape.properties.strokeWidth as FloatV<number>).contents;
  const opacity = toSvgOpacityProperty(
    (shape.properties.strokeColor as ColorV<number>).contents
  );
  const leftArrowId = shape.properties.name.contents + "-leftArrowhead";
  const rightArrowId = shape.properties.name.contents + "-rightArrowhead";
  const arrowheadStyle = (shape.properties.arrowheadStyle as StrV).contents;
  const arrowheadSize = (shape.properties.arrowheadSize as FloatV<number>)
    .contents;

  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");

  // Map/Fill the shape attributes while keeping track of input properties mapped
  elem.appendChild(
    arrowHead(leftArrowId, color, opacity, arrowheadStyle, arrowheadSize)
  );
  elem.appendChild(
    arrowHead(rightArrowId, color, opacity, arrowheadStyle, arrowheadSize)
  );
  attrToNotAutoMap.push(
    "strokeColor",
    "strokeWidth",
    "arrowheadStyle",
    "arrowheadSize"
  );
  const pathElem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "path"
  );
  pathElem.setAttribute("d", path);

  // Opacity and width only relevant if stroke is present
  if (
    (shape.properties.strokeColor as ColorV<number>).contents.tag !== "NONE"
  ) {
    pathElem.setAttribute("stroke-opacity", opacity.toString());
    pathElem.setAttribute("stroke-width", thickness.toString());
  }
  pathElem.setAttribute("stroke", color);

  // factor out an AttrHelper
  if (
    "strokeDasharray" in shape.properties &&
    shape.properties.strokeDasharray.contents !== ""
  ) {
    pathElem.setAttribute(
      "stroke-dasharray",
      (shape.properties.strokeDasharray as StrV).contents
    );
  } else if (shape.properties.strokeStyle.contents === "dashed") {
    pathElem.setAttribute("stroke-dasharray", DASH_ARRAY.toString());
  }
  attrToNotAutoMap.push("strokeDasharray", "strokeStyle");

  if (
    "strokeLinecap" in shape.properties &&
    shape.properties.strokeLinecap.contents !== ""
  ) {
    pathElem.setAttribute(
      "stroke-linecap",
      (shape.properties.strokeLinecap as StrV).contents
    );
  } else {
    pathElem.setAttribute("stroke-linecap", "butt"); // same default as SVG
  }
  attrToNotAutoMap.push("strokeLinecap");

  // TODO: dedup in AttrHelper
  if (shape.properties.startArrowhead.contents === true) {
    pathElem.setAttribute("marker-start", `url(#${leftArrowId})`);
    attrToNotAutoMap.push("startArrowhead");
  }
  if (shape.properties.endArrowhead.contents === true) {
    pathElem.setAttribute("marker-end", `url(#${rightArrowId})`);
    attrToNotAutoMap.push("endArrowhead");
  }
  elem.appendChild(pathElem);
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  // Directly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default Line;

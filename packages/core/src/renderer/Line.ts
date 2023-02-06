import * as uuid from "uuid";
import { Shape } from "../types/shape";
import { BoolV, ColorV, FloatV, StrV, VectorV } from "../types/value";
import {
  ArrowheadSpec,
  getArrowhead,
  round2,
  toScreen,
  toSvgOpacityProperty,
  toSvgPaintProperty,
} from "../utils/Util";
import { attrAutoFillSvg, attrTitle, DASH_ARRAY } from "./AttrHelper";
import { ShapeProps } from "./Renderer";

export const arrowHead = (
  id: string,
  color: string,
  opacity: number,
  arrow: ArrowheadSpec,
  size: number,
  flip: boolean
): SVGMarkerElement => {
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
  if (flip) {
    marker.setAttribute("orient", "auto");
  } else {
    marker.setAttribute("orient", "auto-start-reverse");
  }

  const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
  path.setAttribute("d", arrow.path);
  if (arrow.fillKind === "stroke") {
    path.setAttribute("fill", "none");
    marker.setAttribute("stroke", color);
    marker.setAttribute("stroke-opacity", opacity.toString());
  } /* if (arrow.fillKind === "fill") */ else {
    path.setAttribute("fill", color);
    path.setAttribute("fill-opacity", opacity.toString());
  }
  if (arrow.style) {
    Object.entries(arrow.style).forEach(([key, value]: [string, string]) => {
      path.setAttribute(key, value);
    });
  }
  marker.appendChild(path);
  return marker;
};

const makeRoomForArrows = (
  shape: Shape,
  startArrowhead?: ArrowheadSpec,
  endArrowhead?: ArrowheadSpec
): [number[][], string[]] => {
  // Keep a list of which input properties we programatically mapped
  const attrMapped: string[] = [];

  const [lineSX, lineSY] = (shape.properties.start as VectorV<number>)
    .contents as [number, number];
  const [lineEX, lineEY] = (shape.properties.end as VectorV<number>)
    .contents as [number, number];

  const startArrowheadSize = (shape.properties
    .startArrowheadSize as FloatV<number>).contents;
  const endArrowheadSize = (shape.properties.endArrowheadSize as FloatV<number>)
    .contents;
  const thickness = (shape.properties.strokeWidth as FloatV<number>).contents;
  attrMapped.push(
    "start",
    "end",
    "startArrowhead",
    "endArrowhead",
    "startArrowheadSize",
    "endArrowheadSize",
    "strokeWidth"
  );

  // height * size = Penrose computed arrow size
  // multiplied by thickness since the arrow size uses markerUnits, which is strokeWidth by default:
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerUnits
  const length = Math.sqrt((lineSX - lineEX) ** 2 + (lineSY - lineEY) ** 2);

  // Subtract off the arrowHeight from each side.
  // See https://math.stackexchange.com/a/2045181 for a derivation.
  let arrowSX, arrowSY;
  if (startArrowhead) {
    const startFlip = shape.properties.flipStartArrowhead.contents;
    const startArrowWidth =
      (startFlip
        ? startArrowhead.refX
        : startArrowhead.width - startArrowhead.refX) *
      startArrowheadSize *
      thickness;
    const dx = (startArrowWidth / length) * (lineSX - lineEX);
    const dy = (startArrowWidth / length) * (lineSY - lineEY);
    [arrowSX, arrowSY] = [lineSX - dx, lineSY - dy];
  } else {
    [arrowSX, arrowSY] = [lineSX, lineSY];
  }

  let arrowEX, arrowEY;
  if (endArrowhead) {
    const endArrowWidth =
      (endArrowhead.width - endArrowhead.refX) * endArrowheadSize * thickness;
    [arrowEX, arrowEY] = [
      lineEX - (endArrowWidth / length) * (lineEX - lineSX),
      lineEY - (endArrowWidth / length) * (lineEY - lineSY),
    ];
  } else {
    [arrowEX, arrowEY] = [lineEX, lineEY];
  }

  return [
    [
      [arrowSX, arrowSY],
      [arrowEX, arrowEY],
    ],
    attrMapped,
  ];
};

const Line = ({ shape, canvasSize, variation }: ShapeProps): SVGGElement => {
  const startArrowhead = getArrowhead(
    (shape.properties.startArrowhead as StrV).contents
  );
  const endArrowhead = getArrowhead(
    (shape.properties.endArrowhead as StrV).contents
  );
  const [
    [[arrowSX, arrowSY], [arrowEX, arrowEY]],
    attrToNotAutoMap,
  ] = makeRoomForArrows(shape, startArrowhead, endArrowhead);
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
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");

  const unique = uuid.v4();
  const startArrowId = unique + "-startArrowId";
  const endArrowId = unique + "-endArrowId";
  if (startArrowhead) {
    const startArrowheadSize = (shape.properties
      .startArrowheadSize as FloatV<number>).contents;
    const flip = (shape.properties.flipStartArrowhead as BoolV).contents;
    elem.appendChild(
      arrowHead(
        startArrowId,
        color,
        opacity,
        startArrowhead,
        startArrowheadSize,
        flip
      )
    );
  }
  if (endArrowhead) {
    const endArrowheadSize = (shape.properties
      .endArrowheadSize as FloatV<number>).contents;
    elem.appendChild(
      arrowHead(
        endArrowId,
        color,
        opacity,
        endArrowhead,
        endArrowheadSize,
        false
      )
    );
  }

  // Map/Fill the shape attributes while keeping track of input properties mapped
  attrToNotAutoMap.push(
    "strokeColor",
    "strokeWidth",
    "startArrowhead",
    "flipStartArrowhead",
    "endArrowhead",
    "startArrowheadSize",
    "endArrowheadSize"
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
  if (startArrowhead) {
    pathElem.setAttribute("marker-start", `url(#${startArrowId})`);
    attrToNotAutoMap.push("startArrowhead");
  }
  if (endArrowhead) {
    pathElem.setAttribute("marker-end", `url(#${endArrowId})`);
    attrToNotAutoMap.push("endArrowhead");
  }
  elem.appendChild(pathElem);
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  // Directly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default Line;

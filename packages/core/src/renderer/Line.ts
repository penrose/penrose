import { Line } from "../shapes/Line";
import {
  ArrowheadSpec,
  getArrowhead,
  round2,
  toScreen,
  toSvgOpacityProperty,
  toSvgPaintProperty,
} from "../utils/Util";
import { attrAutoFillSvg, attrTitle, DASH_ARRAY } from "./AttrHelper";
import { RenderProps } from "./Renderer";

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
  shape: Line<number>,
  startArrowhead?: ArrowheadSpec,
  endArrowhead?: ArrowheadSpec
): [number[][], string[]] => {
  // Keep a list of which input properties we programatically mapped
  const attrMapped: string[] = [];

  const [lineSX, lineSY] = [shape.start.contents[0], shape.start.contents[1]];
  const [lineEX, lineEY] = [shape.end.contents[0], shape.end.contents[1]];

  const startArrowheadSize = shape.startArrowheadSize.contents;
  const endArrowheadSize = shape.endArrowheadSize.contents;
  const thickness = shape.strokeWidth.contents;
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
    const startFlip = shape.flipStartArrowhead.contents;
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

const RenderLine = (
  shape: Line<number>,
  { canvasSize, namespace, variation }: RenderProps
): SVGGElement => {
  const startArrowhead = getArrowhead(shape.startArrowhead.contents);
  const endArrowhead = getArrowhead(shape.endArrowhead.contents);
  const [
    [[arrowSX, arrowSY], [arrowEX, arrowEY]],
    attrToNotAutoMap,
  ] = makeRoomForArrows(shape, startArrowhead, endArrowhead);
  const [sx, sy] = toScreen([arrowSX, arrowSY], canvasSize);
  const [ex, ey] = toScreen([arrowEX, arrowEY], canvasSize);

  const path = `M ${sx} ${sy} L ${ex} ${ey}`;
  const color = toSvgPaintProperty(shape.strokeColor.contents);
  const thickness = shape.strokeWidth.contents;
  const opacity = toSvgOpacityProperty(shape.strokeColor.contents);
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  
  // an unique id for this instance is determined by the variation and namespace
  const unique = `${namespace}-${variation}-${shape.name.contents}`;
  const startArrowId = unique + "-startArrowId";
  const endArrowId = unique + "-endArrowId";
  if (startArrowhead) {
    const startArrowheadSize = shape.startArrowheadSize.contents;
    const flip = shape.flipStartArrowhead.contents;
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
    const endArrowheadSize = shape.endArrowheadSize.contents;
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
  if (shape.strokeColor.contents.tag !== "NONE") {
    pathElem.setAttribute("stroke-opacity", opacity.toString());
    pathElem.setAttribute("stroke-width", thickness.toString());
  }
  pathElem.setAttribute("stroke", color);

  // factor out an AttrHelper
  if (shape.strokeDasharray.contents !== "") {
    pathElem.setAttribute("stroke-dasharray", shape.strokeDasharray.contents);
  } else if (shape.strokeStyle.contents === "dashed") {
    pathElem.setAttribute("stroke-dasharray", DASH_ARRAY.toString());
  }
  attrToNotAutoMap.push("strokeDasharray", "strokeStyle");

  if (shape.strokeLinecap.contents !== "") {
    pathElem.setAttribute("stroke-linecap", shape.strokeLinecap.contents);
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
export default RenderLine;

import { Shape } from "types/shapeTypes";
import { toHex, toScreen } from "utils/Util";

export const attrFill = ({ properties }: Shape, elem: SVGElement) => {
  const color = properties.color as IColorV<number>;
  const alpha = color.contents.contents[3];
  elem.setAttribute("fill", toHex(color.contents));
  elem.setAttribute("fill-opacity", alpha.toString());
};

export const attrOpacity = ({ properties }: Shape, elem: SVGElement) => {
  const opacity = (properties.opacity as IFloatV<number>).contents;
  elem.setAttribute("opacity", opacity.toString());
};

export const attrCenter = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
) => {
  const center = properties.center as IVectorV<number>;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  elem.setAttribute("cx", x.toString());
  elem.setAttribute("cy", y.toString());
};

export const attrTransformCoords = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
) => {
  const center = properties.center as IVectorV<number>;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  const w = properties.w as IFloatV<number>;
  const h = properties.h as IFloatV<number>;
  elem.setAttribute(
    "transform",
    `translate(${x - w.contents / 2}, ${y - h.contents / 2})`
  );
};

export const attrXY = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
) => {
  const center = properties.center as IVectorV<number>;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  const w = properties.w as IFloatV<number>;
  const h = properties.h as IFloatV<number>;
  elem.setAttribute("x", (x - w.contents / 2).toString());
  elem.setAttribute("y", (y - h.contents / 2).toString());
};

export const attrSideCoords = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
) => {
  const center = properties.center as IVectorV<number>;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  const side = properties.side as IFloatV<number>;
  elem.setAttribute(
    "transform",
    `translate(${x - side.contents / 2}, ${y - side.contents / 2})`
  );
};

export const attrRadius = ({ properties }: Shape, elem: SVGElement) => {
  const r = properties.r as IFloatV<number>;
  elem.setAttribute("r", r.contents.toString());
};

export const attrRadii = ({ properties }: Shape, elem: SVGElement) => {
  const rx = properties.rx as IFloatV<number>;
  const ry = properties.ry as IFloatV<number>;
  elem.setAttribute("rx", rx.contents.toString());
  elem.setAttribute("ry", rx.contents.toString());
};

export const attrWH = ({ properties }: Shape, elem: SVGElement) => {
  const w = properties.w as IFloatV<number>;
  const h = properties.h as IFloatV<number>;
  elem.setAttribute("width", w.contents.toString());
  elem.setAttribute("height", h.contents.toString());
};

export const attrSide = ({ properties }: Shape, elem: SVGElement) => {
  const side = properties.side as IFloatV<number>;
  elem.setAttribute("width", side.contents.toString());
  elem.setAttribute("height", side.contents.toString());
};

export const DASH_ARRAY = "7,5";

export const attrStroke = ({ properties }: Shape, elem: SVGElement) => {
  const strokeColor = properties.strokeColor as IColorV<number>;
  const strokeAlpha = strokeColor.contents.contents[3];
  const thickness = properties.strokeWidth.contents;
  elem.setAttribute("stroke", toHex(strokeColor.contents));
  elem.setAttribute("stroke-opacity", strokeAlpha.toString());
  elem.setAttribute("stroke-width", thickness.toString());
  let dashArray = DASH_ARRAY;
  if ("strokeDashArray" in properties) {
    dashArray = (properties.strokeDashArray as IStrV<string>).contents;
  }
  if (properties.strokeStyle.contents === "dashed") {
    elem.setAttribute("stroke-dasharray", dashArray.toString());
  }
};

export const attrTitle = ({ properties }: Shape, elem: SVGElement) => {
  const name = properties.name as IStrV<string>;
  const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
  title.textContent = name.contents;
  elem.appendChild(title);
};

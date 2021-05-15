import {
  IColorV,
  IFloatV,
  IVectorV,
  IStrV,
  IPtListV,
  ILListV,
  Value,
} from "types/value";
import { Shape } from "types/shape";
import { toHex, toScreen } from "utils/Util";

export const attrFill = ({ properties }: Shape, elem: SVGElement) => {
  const color = properties.color as IColorV<number>;
  const alpha = color.contents.contents[3];
  elem.setAttribute("fill", toHex(color.contents));
  elem.setAttribute("fill-opacity", alpha.toString());
};

export const attrNoFill = ({ properties }: Shape, elem: SVGElement) => {
  elem.setAttribute("fill", "none");
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

export const attrPolyCenter = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
) => {
  if (properties.center) {
    const [x, y] = toScreen(
      properties.center.contents as [number, number],
      canvasSize
    );
    elem.setAttribute("cx", x.toString());
    elem.setAttribute("cy", y.toString());
  } else {
    const points = properties.points as IPtListV<number>;
    const xs = points.contents.map((xy) => xy[0]);
    const ys = points.contents.map((xy) => xy[1]);

    const minX = Math.min(...xs),
      minY = Math.min(...ys),
      maxX = Math.max(...xs),
      maxY = Math.max(...ys);

    const cx = (minX + maxX) / 2,
      cy = (minY + maxY) / 2;

    const [x, y] = toScreen([cx, cy] as [number, number], canvasSize);
    elem.setAttribute("cx", x.toString());
    elem.setAttribute("cy", y.toString());
  }
};

export const attrScale = ({ properties }: Shape, elem: SVGElement) => {
  let scale = properties?.scale?.contents;
  scale ||= 1;
  let transform = elem.getAttribute("transform");
  transform =
    transform == null ? `scale(${scale})` : transform + `scale{${scale}}`;
  elem.setAttribute("transform", transform);
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
  let transform = elem.getAttribute("transform");
  transform =
    transform == null
      ? `translate(${x - w.contents / 2}, ${y - h.contents / 2})`
      : transform + `translate(${x - w.contents / 2}, ${y - h.contents / 2})`;
  elem.setAttribute("transform", transform);
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

/**
 * Rotates a GPI by n degrees about a center
 * Note: elem must be `transform`able
 * NOTE: must be called before transform translate coords (matrix rules)
 * https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
 */
export const attrRotation = (
  { properties }: Shape,
  center: Value<number>,
  w: Value<number>,
  h: Value<number>,
  canvasSize: [number, number],
  elem: SVGElement
): void => {
  const rotation = (properties.rotation as IFloatV<number>).contents;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  let transform = elem.getAttribute("transform");
  transform =
    transform == null
      ? `rotate(${rotation}, ${x - (w.contents as number) / 2}, ${
          y - (h.contents as number) / 2
        })`
      : transform +
        `rotate(${rotation}, ${x - (w.contents as number) / 2}, ${
          y - (h.contents as number) / 2
        })`;
  elem.setAttribute("transform", transform);
};

export const attrSideCoords = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
) => {
  const center = properties.center as IVectorV<number>;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  const side = properties.side as IFloatV<number>;
  let transform = elem.getAttribute("transform");
  transform =
    transform == null
      ? `translate(${x - side.contents / 2}, ${y - side.contents / 2})`
      : transform +
        `translate(${x - side.contents / 2}, ${y - side.contents / 2})`;
  elem.setAttribute("transform", transform);
};

export const attrRadius = ({ properties }: Shape, elem: SVGElement) => {
  const r = properties.r as IFloatV<number>;
  elem.setAttribute("r", r.contents.toString());
};

export const attrPathLength = ({ properties }: Shape, elem: SVGElement) => {
  const pathLength = properties.pathLength as IFloatV<number>;
  elem.setAttribute("pathLength", pathLength.contents.toString());
};

export const attrRadiusX = ({ properties }: Shape, elem: SVGElement) => {
  const rx = properties.rx as IFloatV<number>;
  elem.setAttribute("rx", rx.contents.toString());
};

export const attrRadiusY = ({ properties }: Shape, elem: SVGElement) => {
  const ry = properties.ry as IFloatV<number>;
  elem.setAttribute("ry", ry.contents.toString());
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

export const attrPoints = ({ properties }: Shape, elem: SVGElement) => {
  const points = properties.points as IPtListV<number>;
  elem.setAttribute("points", points.contents.toString());
};

export const attrSide = ({ properties }: Shape, elem: SVGElement) => {
  const side = properties.side as IFloatV<number>;
  elem.setAttribute("width", side.contents.toString());
  elem.setAttribute("height", side.contents.toString());
};

export const attrPathData = ({ properties }: Shape, elem: SVGElement) => {
  const d = properties.data as IStrV<string>;
  elem.setAttribute("d", d.contents.toString());
};

export const DASH_ARRAY = "7,5";

export const attrStroke = ({ properties }: Shape, elem: SVGElement) => {
  const strokeColor = properties.strokeColor as IColorV<number>;
  const strokeAlpha = strokeColor.contents.contents[3];
  const thickness = properties.strokeWidth.contents;
  elem.setAttribute("stroke", toHex(strokeColor.contents));
  elem.setAttribute("stroke-opacity", strokeAlpha.toString());
  elem.setAttribute("stroke-width", thickness.toString());
  if (
    "strokeDashArray" in properties &&
    properties.strokeDashArray.contents !== ""
  ) {
    elem.setAttribute(
      "stroke-dasharray",
      (properties.strokeDashArray as IStrV<string>).contents
    );
  } else if (properties.strokeStyle.contents === "dashed") {
    elem.setAttribute("stroke-dasharray", DASH_ARRAY.toString());
  }
};

export const attrTitle = ({ properties }: Shape, elem: SVGElement) => {
  const name = properties.name as IStrV<string>;
  const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
  title.textContent = name.contents;
  elem.appendChild(title);
};

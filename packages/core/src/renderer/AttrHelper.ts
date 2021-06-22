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
import { toHex, getCoords, toScreen } from "utils/Util";

/**
 * @param  {Shape} {properties}
 * @param  {SVGElement} elem
 * @requires shape defines attribute 'fill'
 * @description sets 'fill' from attribute 'fill', sets 'fill-opacity' from 'fill' (or from 'fill-opacity' if it's defined)
 */
export const attrFill = ({ properties }: Shape, elem: SVGElement) => {
  const fill = properties.fill as IColorV<number>;
  const alpha = fill.contents.contents[3];
  elem.setAttribute("fill", toHex(fill.contents));
  elem.setAttribute("fill-opacity", alpha.toString());
};
/**
 * @param  {Shape} {properties}
 * @param  {SVGElement} elem
 * @description sets 'fill' to 'none'
 */
export const attrNoFill = ({ properties }: Shape, elem: SVGElement) => {
  elem.setAttribute("fill", "none");
};
/**
 * @param  {Shape} {properties}
 * @param  {SVGElement} elem
 * @description sets 'opacity'
 */
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
  const [x, y] = getCoords(properties);
  elem.setAttribute("cx", x.toString());
  elem.setAttribute("cy", y.toString());
};

/**
 * @param  {Shape} {properties}
 * @param  {SVGElement} elem
 * @description adds a scaling function to the shape's 'transform' attribute
 */
export const attrScale = ({ properties }: Shape, elem: SVGElement) => {
  let scale = properties?.scale?.contents;
  scale = scale || 1;
  let transform = elem.getAttribute("transform");
  transform =
    transform == null ? `scale(${scale})` : transform + `scale{${scale}}`;
  elem.setAttribute("transform", transform);
};
/**
 * @param  {Shape} {properties}
 * @param  {[number} canvasSize
 * @param  {} number]
 * @param  {SVGElement} elem
 * @description adds 'translate' function to the shape's 'transform' attribute
 */
export const attrTransformCoords = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
) => {
  const center = properties.center as IVectorV<number>;
  const [x, y] = getCoords(properties);
  const w = properties.width as IFloatV<number>;
  const h = properties.hidth as IFloatV<number>;
  let transform = elem.getAttribute("transform");
  transform =
    transform == null
      ? `translate(${x - w.contents / 2}, ${y - h.contents / 2})`
      : transform + `translate(${x - w.contents / 2}, ${y - h.contents / 2})`;
  elem.setAttribute("transform", transform);
};
/**
 * @param  {Shape} {properties}
 * @param  {[number} canvasSize
 * @param  {} number]
 * @param  {SVGElement} elem
 * @requires shape defines attributes 'x' and 'y'
 * @description sets attributes 'x' and 'y'
 */
export const attrXY = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
) => {
  const [x, y] = getCoords(properties);
  // console.log({ x: x, y: y });
  // const w = properties.w as IFloatV<number>;
  // const h = properties.h as IFloatV<number>;
  elem.setAttribute("x", x.toString());
  elem.setAttribute("y", y.toString());
};
/**
 * @param  {Shape} {properties}
 * @param  {[number} canvasSize
 * @param  {} number]
 * @param  {SVGElement} elem
 * @requires shape defines attributes 'cx' and 'cy'
 * @description set attributes 'cx', 'cy'
 */
export const attrcXcY = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
) => {
  const [x, y] = getCoords(properties);

  if (elem.nodeName === "circle") {
    elem.setAttribute("cx", x.toString());
    elem.setAttribute("cy", y.toString());
  } else {
    elem.setAttribute("cx", x.toString());
    elem.setAttribute("cy", y.toString());
  }
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
  const [x, y] = getCoords(properties);
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
  const [x, y] = getCoords(properties);
  const side = properties.side as IFloatV<number>;
  let transform = elem.getAttribute("transform");
  transform =
    transform == null
      ? `translate(${x - side.contents / 2}, ${y - side.contents / 2})`
      : transform +
        `translate(${x - side.contents / 2}, ${y - side.contents / 2})`;
  elem.setAttribute("transform", transform);
};
/**
 * @param  {Shape} {properties}
 * @param  {SVGElement} elem
 * @requires shape has defines 'pathlength'
 * @define sets attribue 'pathLength'
 */
export const attrPathLength = ({ properties }: Shape, elem: SVGElement) => {
  const pathLength = properties.pathLength as IFloatV<number>;
  elem.setAttribute("pathLength", pathLength.contents.toString());
};
/**
 * @param  {Shape} {properties}
 * @param  {SVGElement} elem
 * @requires shape defines attribue 'r'
 * @define sets attribue 'r'
 */
export const attrRadius = ({ properties }: Shape, elem: SVGElement) => {
  const r = properties.r as IFloatV<number>;
  elem.setAttribute("r", r.contents.toString());
};
/**
 * @param  {Shape} {properties}
 * @param  {SVGElement} elem
 * @requires shape defines attribue 'rx'
 * @define sets attribue 'rx'
 */
export const attrRadiusX = ({ properties }: Shape, elem: SVGElement) => {
  const rx = properties.rx as IFloatV<number>;
  elem.setAttribute("rx", rx.contents.toString());
};
/**
 * @param  {Shape} {properties}
 * @param  {SVGElement} elem
 * @requires shape defines attribue 'ry'
 * @define sets attribue 'ry'
 */
export const attrRadiusY = ({ properties }: Shape, elem: SVGElement) => {
  const ry = properties.ry as IFloatV<number>;
  elem.setAttribute("ry", ry.contents.toString());
};
/**
 * @param  {Shape} {properties}
 * @param  {SVGElement} elem
 * @requires shape has attributes 'rx' and 'ry'
 * @description sets attributes 'rx' and 'ry'
 */
export const attrRadii = ({ properties }: Shape, elem: SVGElement) => {
  const rx = properties.rx as IFloatV<number>;
  const ry = properties.ry as IFloatV<number>;
  elem.setAttribute("rx", rx.contents.toString());
  elem.setAttribute("ry", ry.contents.toString());
};
/**
 * @param  {Shape} {properties}
 * @param  {SVGElement} elem
 * @requires shape has attributes 'w' and 'h'
 * @description sets attributes 'width' and 'height'
 */
export const attrWH = ({ properties }: Shape, elem: SVGElement) => {
  const w = properties.w as IFloatV<number>;
  const h = properties.h as IFloatV<number>;
  elem.setAttribute("width", w.contents.toString());
  elem.setAttribute("height", h.contents.toString());
};
/**
 * @param  {Shape} {properties}
 * @param  {SVGElement} elem
 * @requires shape defines attribute 'points'
 * @description sets attribute 'points'
 */
export const attrPoints = ({ properties }: Shape, elem: SVGElement) => {
  const points = properties.points as IPtListV<number>;
  elem.setAttribute("points", points.contents.toString());
};

export const attrSide = ({ properties }: Shape, elem: SVGElement) => {
  const side = properties.side as IFloatV<number>;
  elem.setAttribute("width", side.contents.toString());
  elem.setAttribute("height", side.contents.toString());
};
/**
 * @param  {Shape} {properties}
 * @param  {SVGElement} elem
 * @requires shape defines attribute 'd'
 * @description sets attribute 'd'
 */
export const attrPathData = ({ properties }: Shape, elem: SVGElement) => {
  const d = properties.data as IStrV;
  elem.setAttribute("d", d.contents.toString());
};

export const DASH_ARRAY = "7,5";
/**
 * @param  {Shape} {properties}
 * @param  {SVGElement} elem
 * @description sets attributes 'stroke', 'stroke-opacity', and 'stroke-width', sets 'stroke-dasharray' if applicable
 */
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
      (properties.strokeDashArray as IStrV).contents
    );
  } else if (properties.strokeStyle.contents === "dashed") {
    elem.setAttribute("stroke-dasharray", DASH_ARRAY.toString());
  }
};
/**
 * @param  {Shape} {properties}
 * @param  {SVGElement} elem
 * @description creates svg elem attribute 'title', defines it's 'textContent', and appends it to the shape
 */
export const attrTitle = ({ properties }: Shape, elem: SVGElement) => {
  const name = properties.name as IStrV;
  const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
  title.textContent = name.contents;
  elem.appendChild(title);
};

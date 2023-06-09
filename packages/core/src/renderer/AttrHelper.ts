/**
 * Provides an assortment of utility functions shared across shapes that computes
 * output SVG properties using the optimized shape properties as input.
 */

import { Line } from "../shapes/Line.js";
import { Shape } from "../shapes/Shapes.js";
import { Text } from "../shapes/Text.js";
import {
  Center,
  Corner,
  Fill,
  Named,
  Poly,
  Rect,
  Rotate,
  Scale,
  String as StringProps,
  Stroke,
} from "../types/shapes.js";
import { toFontRule } from "../utils/CollectLabels.js";
import {
  isKeyOf,
  toScreen,
  toSvgOpacityProperty,
  toSvgPaintProperty,
} from "../utils/Util.js";
import { attrMapSvg } from "./AttrMapSvg.js";

/**
 * Auto-map to SVG any input properties for which we lack specific logic.
 *
 * Apply a map, AttrMapSvg, to perform any target-specific property name translation,
 * i.e., map from Penrose camel case formal to SVG mixed-case/kebab format.  Property names
 * not found in the map are mapped straight across.
 *
 * Note: Right now we are neither validating the SVG property names nor its contents.  The
 * thinking is to add an optional validator to the end of the pipeline at some point rather
 * than implement validation for all passthrough SVG properties inside Penrose.
 *
 * Note: This is an "escape hatch" for "passthrough" SVG properties we don't currently support.
 *
 * Note: SVG property names are case sensitive.
 */
export const attrAutoFillSvg = (
  shape: Shape<number>,
  elem: SVGElement,
  attrAlreadyMapped: string[]
): void => {
  // Internal properties to never auto-map to SVG
  const attrToNeverAutoMap: string[] = [
    "strokeStyle",
    "name",
    "ensureOnCanvas",
  ];

  // Merge the mapped and never-map properties.  Convert to Set
  const attrToNotAutoMap = new Set<string>(
    attrAlreadyMapped.concat(attrToNeverAutoMap)
  );

  // Map unknown/unseen attributes with values to SVG output.
  // This is the "escape hatch" for properties we don't support.
  //
  // NOTE: `style` is handled as a special case, because some of
  // the built-in properties will write to it __and__ the user
  // should be able to append to it. Therefore, we check if there's
  // an existing value in `style` and append to it if true.

  for (const [propKey, propVal] of shape.passthrough) {
    if (
      (propVal.tag === "StrV" && propVal.contents === "") ||
      attrToNotAutoMap.has(propKey)
    )
      continue;

    if (isKeyOf(propKey, attrMapSvg)) {
      const mappedPropKey: string = attrMapSvg[propKey];
      if (!elem.hasAttribute(mappedPropKey)) {
        elem.setAttribute(mappedPropKey, propVal.contents.toString());
      }
    } else if (propKey === "style" && propVal.contents !== "") {
      const style = elem.getAttribute(propKey);
      if (style === null) {
        elem.setAttribute(propKey, propVal.contents.toString());
      } else {
        elem.setAttribute(propKey, `${style}${propVal.contents.toString()}`);
      }
    } else {
      if (!elem.hasAttribute(propKey)) {
        elem.setAttribute(propKey, propVal.contents.toString());
      }
    }
  }
};

/**
 * Maps fillColor --> fill, fill-opacity
 */
export const attrFill = (
  properties: Fill<number>,
  elem: SVGElement
): string[] => {
  const color = properties.fillColor;
  const alpha = toSvgOpacityProperty(color.contents);

  elem.setAttribute("fill", toSvgPaintProperty(color.contents));

  // Fill opacity only relevant if fill is present
  if (color.contents.tag !== "NONE") {
    elem.setAttribute("fill-opacity", alpha.toString());
  }

  return ["fillColor"]; // Return array of input properties programatically mapped
};

/**
 * Maps center --> cx, cy
 */
export const attrCenter = (
  properties: Center<number>,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
  const center = properties.center;
  const [x, y] = toScreen([center.contents[0], center.contents[1]], canvasSize);
  elem.setAttribute("cx", x.toString());
  elem.setAttribute("cy", y.toString());
  return ["center"]; // Return array of input properties programatically mapped
};

/**
 * Maps scale --> transform
 */
export const attrScale = (
  properties: Scale<number>,
  elem: SVGElement
): string[] => {
  let scale = properties.scale.contents;
  scale = scale || 1;
  let transform = elem.getAttribute("transform");
  transform =
    transform === null ? `scale(${scale})` : transform + `scale{${scale}}`;
  elem.setAttribute("transform", transform);

  return ["scale"]; // Return array of input properties programatically mapped
};

/**
 * Maps center, width, height --> transform
 */
export const attrTransformCoords = (
  properties: Center<number> & Rect<number>,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
  const center = properties.center;
  const [x, y] = toScreen([center.contents[0], center.contents[1]], canvasSize);
  const w = properties.width;
  const h = properties.height;
  let transform = elem.getAttribute("transform");
  transform =
    transform === null
      ? `translate(${x - w.contents / 2}, ${y - h.contents / 2})`
      : transform + `translate(${x - w.contents / 2}, ${y - h.contents / 2})`;
  elem.setAttribute("transform", transform);

  return ["center", "width", "height"]; // Return array of input properties programatically mapped
};

/**
 * Maps center, width, height --> x, y
 */
export const attrXY = (
  properties: Center<number> & Rect<number>,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
  const center = properties.center;
  const [x, y] = toScreen([center.contents[0], center.contents[1]], canvasSize);
  const w = properties.width;
  const h = properties.height;
  elem.setAttribute("x", (x - w.contents / 2).toString());
  elem.setAttribute("y", (y - h.contents / 2).toString());

  return ["center", "width", "height"]; // Return array of input properties programatically mapped
};

/**
 * Maps center, width, height, rotation --> transform
 *
 * Rotates a GPI by n degrees about a center
 * Note: elem must be `transform`able
 * NOTE: must be called before transform translate coords (matrix rules)
 * https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
 */
export const attrRotation = (
  properties: Rotate<number> & Center<number> & Rect<number>,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
  const w = properties.width;
  const h = properties.height;
  const center = properties.center;
  const rotation = properties.rotation.contents;
  const [x, y] = toScreen([center.contents[0], center.contents[1]], canvasSize);
  let transform = elem.getAttribute("transform");
  transform =
    transform === null
      ? `rotate(${rotation}, ${x}, ${y})`
      : transform + `rotate(${rotation}, ${x}, ${y})`;
  elem.setAttribute("transform", transform);

  return ["rotation", "center", "width", "height"]; // Return array of input properties programatically mapped
};

/**
 * Maps width, height --> width, height
 */
export const attrWH = (
  properties: Rect<number>,
  elem: SVGElement | HTMLElement
): string[] => {
  const w = properties.width;
  const h = properties.height;
  elem.setAttribute("width", w.contents.toString());
  elem.setAttribute("height", h.contents.toString());

  return ["width", "height"]; // Return array of input properties programatically mapped
};

/**
 * Maps cornerRadius --> rx
 */
export const attrCornerRadius = (
  properties: Corner<number>,
  elem: SVGElement
): string[] => {
  const rx = properties.cornerRadius;
  elem.setAttribute("rx", rx.contents.toString());

  return ["cornerRadius"]; // Return array of input properties programatically mapped
};

/**
 * Maps string --> new TextNode
 */
export const attrString = (
  properties: StringProps<number>,
  elem: SVGElement
): string[] => {
  const str = properties.string;
  const text = document.createTextNode(str.contents.toString());
  elem.appendChild(text);

  return ["string"]; // Return array of input properties programatically mapped
};

export const DASH_ARRAY = "7,5";

/**
 * Maps strokeColor --> stroke, stroke-opacity
 *      strokeWidth --> stroke-width
 *      strokeDasharray, strokeStyle --> stroke-dasharray
 *      strokeLinecap --> stroke-linecap
 */
export const attrStroke = (
  properties: Stroke<number> | Line<number>,
  elem: SVGElement
): string[] => {
  // Keep a list of which input properties we programatically mapped
  const attrMapped: string[] = [];

  const strokeColor = properties.strokeColor;
  const strokeAlpha = toSvgOpacityProperty(strokeColor.contents);
  const thickness = properties.strokeWidth.contents;
  elem.setAttribute("stroke", toSvgPaintProperty(strokeColor.contents));
  attrMapped.push("strokeColor", "strokeWidth");

  // Stroke opacity, width, and dashiness only relevant if stroke is present
  if (strokeColor.contents.tag !== "NONE") {
    elem.setAttribute("stroke-opacity", strokeAlpha.toString());
    elem.setAttribute("stroke-width", thickness.toString());

    if (
      "strokeDasharray" in properties &&
      properties.strokeDasharray.contents !== ""
    ) {
      elem.setAttribute(
        "stroke-dasharray",
        properties.strokeDasharray.contents
      );
      attrMapped.push("strokeDasharray");
    } else if (
      "strokeStyle" in properties &&
      properties.strokeStyle.contents === "dashed"
    ) {
      elem.setAttribute("stroke-dasharray", DASH_ARRAY.toString());
      attrMapped.push("strokeDasharray", "strokeStyle");
    }

    // NOTE: some stroked properties might not contain `strokeLinecap`
    if (
      "strokeLinecap" in properties &&
      properties.strokeLinecap.contents !== ""
    ) {
      elem.setAttribute("stroke-linecap", properties.strokeLinecap.contents);
      attrMapped.push("strokeLinecap");
    }
  }

  return attrMapped; // Return array of input properties programatically mapped
};

/**
 * Maps name --> new Title
 */
export const attrTitle = (
  properties: Named<number>,
  elem: SVGElement
): string[] => {
  const name = properties.name;
  const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
  title.textContent = name.contents;
  elem.appendChild(title);

  return ["name"]; // Return array of input properties programatically mapped
};

/**
 * Maps fontFamily, fontSize, fontStretch, fontStyle, fontVariant, fontWeight, lineHeight -> font
 */
export const attrFont = (shape: Text<number>, elem: SVGElement): string[] => {
  const fontString: string = toFontRule(shape);
  const existingStyle: string | null = elem.getAttribute("style");

  // TODO: check if `lineHeight` is valid
  elem.setAttribute(
    "style",
    existingStyle
      ? `${existingStyle}; font: ${fontString};`
      : `font: ${fontString};`
  );
  return [
    "fontFamily",
    "fontSize",
    "fontStretch",
    "fontStyle",
    "fontVariant",
    "fontWeight",
    "lineHeigh",
  ]; // Return array of input properties programatically mapped
};

/**
 * Maps points -> points
 */
export const attrPolyPoints = (
  shape: Poly<number>,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
  const points = shape.points;
  const pointsTransformed = points.contents.map((p: number[]) =>
    toScreen([p[0], p[1]], canvasSize)
  );
  elem.setAttribute("points", pointsTransformed.toString());
  return ["points"];
};

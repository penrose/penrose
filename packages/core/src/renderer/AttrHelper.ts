/**
 * Provides an assortment of utility functions shared across shapes that computes
 * output SVG properties using the optimized shape properties as input.
 */

import { Shape } from "types/shape";
import { ColorV, FloatV, PtListV, StrV, VectorV } from "types/value";
import { toFontRule } from "utils/CollectLabels";
import { toScreen, toSvgOpacityProperty, toSvgPaintProperty } from "utils/Util";
import { attrMapSvg } from "./AttrMapSvg";

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
  { properties }: Shape,
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
  for (const propName in properties) {
    const propValue: string = properties[propName].contents.toString();

    // Only map properties with values and that we have not previously mapped
    if (propValue !== "" && !attrToNotAutoMap.has(propName)) {
      // If a mapping rule exists, apply it; otherwise, map straight across
      if (propName in attrMapSvg) {
        const mappedPropName: string = attrMapSvg[propName];
        if (!elem.hasAttribute(mappedPropName)) {
          elem.setAttribute(mappedPropName, propValue);
        }
      } else if (propName === "style" && propValue !== "") {
        const style = elem.getAttribute(propName);
        if (style === null) {
          elem.setAttribute(propName, propValue);
        } else {
          elem.setAttribute(propName, `${style}${propValue}`);
        }
      } else {
        if (!elem.hasAttribute(propName)) {
          elem.setAttribute(propName, propValue);
        }
      }
    }
  }
};

/**
 * Maps fillColor --> fill, fill-opacity
 */
export const attrFill = ({ properties }: Shape, elem: SVGElement): string[] => {
  const color = properties.fillColor as ColorV<number>;
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
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
  const center = properties.center as VectorV<number>;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  elem.setAttribute("cx", x.toString());
  elem.setAttribute("cy", y.toString());
  return ["center"]; // Return array of input properties programatically mapped
};

/**
 * Maps scale --> transform
 */
export const attrScale = (
  { properties }: Shape,
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
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
  const center = properties.center as VectorV<number>;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  const w = properties.width as FloatV<number>;
  const h = properties.height as FloatV<number>;
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
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
  const center = properties.center as VectorV<number>;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  const w = properties.width as FloatV<number>;
  const h = properties.height as FloatV<number>;
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
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
  const w = properties.width as FloatV<number>;
  const h = properties.height as FloatV<number>;
  const center = properties.center;
  const rotation = (properties.rotation as FloatV<number>).contents;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  let transform = elem.getAttribute("transform");
  transform =
    transform === null
      ? `rotate(${rotation}, ${x - w.contents / 2}, ${y - h.contents / 2})`
      : transform +
        `rotate(${rotation}, ${x - w.contents / 2}, ${y - h.contents / 2})`;
  elem.setAttribute("transform", transform);

  return ["rotation", "center", "width", "height"]; // Return array of input properties programatically mapped
};

/**
 * Maps width, height --> width, height
 */
export const attrWH = (
  { properties }: Shape,
  elem: SVGElement | HTMLElement
): string[] => {
  const w = properties.width as FloatV<number>;
  const h = properties.height as FloatV<number>;
  elem.setAttribute("width", w.contents.toString());
  elem.setAttribute("height", h.contents.toString());

  return ["width", "height"]; // Return array of input properties programatically mapped
};

/**
 * Maps cornerRadius --> rx
 */
export const attrCornerRadius = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const rx = properties.cornerRadius as FloatV<number>;
  elem.setAttribute("rx", rx.contents.toString());

  return ["cornerRadius"]; // Return array of input properties programatically mapped
};

/**
 * Maps data --> d
 */
export const attrPathData = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const d = properties.data as StrV;
  elem.setAttribute("d", d.contents.toString());

  return ["data"]; // Return array of input properties programatically mapped
};

/**
 * Maps string --> new TextNode
 */
export const attrString = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const str = properties.string as StrV;
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
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  // Keep a list of which input properties we programatically mapped
  const attrMapped: string[] = [];

  const strokeColor = properties.strokeColor as ColorV<number>;
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
        (properties.strokeDasharray as StrV).contents
      );
      attrMapped.push("strokeDasharray");
    } else if (
      "strokeStyle" in properties &&
      properties.strokeStyle.contents === "dashed"
    ) {
      elem.setAttribute("stroke-dasharray", DASH_ARRAY.toString());
      attrMapped.push("strokeDasharray", "strokeStyle");
    }

    if (
      "strokeLinecap" in properties &&
      properties.strokeLinecap.contents !== ""
    ) {
      elem.setAttribute(
        "stroke-linecap",
        (properties.strokeLinecap as StrV).contents
      );
    } else {
      elem.setAttribute("stroke-linecap", "butt");
    }
    attrMapped.push("strokeLinecap");
  }

  return attrMapped; // Return array of input properties programatically mapped
};

/**
 * Maps name --> new Title
 */
export const attrTitle = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const name = properties.name as StrV;
  const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
  title.textContent = name.contents;
  elem.appendChild(title);

  return ["name"]; // Return array of input properties programatically mapped
};

/**
 * Maps fontFamily, fontSize, fontStretch, fontStyle, fontVariant, fontWeight, lineHeight -> font
 */
export const attrFont = (shape: Shape, elem: SVGElement): string[] => {
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
  shape: Shape,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
  const points = shape.properties.points as PtListV<number>;
  const pointsTransformed = points.contents.map((p: number[]) =>
    toScreen(p as [number, number], canvasSize)
  );
  elem.setAttribute("points", pointsTransformed.toString());
  return ["points"];
};

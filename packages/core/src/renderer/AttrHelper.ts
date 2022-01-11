/**
 * Provides an assortment of utility functions shared across shapes that computes
 * output SVG properties using the optimized shape properties as input.
 */

import { IColorV, IFloatV, IVectorV, IStrV } from "types/value";
import { Shape } from "types/shape";
import { toSvgPaintProperty, toScreen, toSvgOpacityProperty } from "utils/Util";

/**
 * Auto-maps any input properties for which we lack specific logic over to the SVG output.
 *
 * Note: Right now we are neither validating the SVG attributes or their contents.  The initial
 * thinking is to add a validator to the end of the pipeline rather than implement validation
 * for all passthrough SVG properties inside Penrose.
 */
export const attrAutoFillSvg = (
  { properties }: Shape,
  elem: SVGElement,
  attrAlreadyMapped: string[]
): void => {
  // Internal properties to never auto-map to SVG
  const attrToNeverAutoMap: string[] = ["strokeStyle"];

  // Merge the mapped and never-map properties, lowercase them, convert to Set
  const attrToNotAutoMap = new Set<string>(
    attrAlreadyMapped
      .concat(attrToNeverAutoMap)
      .map((name) => name.toLowerCase())
  );

  // Map unknown/unseen attributes with values to SVG output.
  for (const propName in properties) {
    const propValue: string = properties[propName].contents.toString();
    const propNameLC: string = propName.toLowerCase();
    if (!attrToNotAutoMap.has(propNameLC)) {
      if (!elem.hasAttribute(propNameLC)) {
        if (propValue !== "") {
          elem.setAttribute(propNameLC, propValue);
        }
      }
    }
  }
};

/**
 * Maps fillColor --> fill, fill-opacity
 */
export const attrFill = ({ properties }: Shape, elem: SVGElement): string[] => {
  const color = properties.fillColor as IColorV<number>;
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
  const center = properties.center as IVectorV<number>;
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
    transform == null ? `scale(${scale})` : transform + `scale{${scale}}`;
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
  const center = properties.center as IVectorV<number>;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  const w = properties.width as IFloatV<number>;
  const h = properties.height as IFloatV<number>;
  let transform = elem.getAttribute("transform");
  transform =
    transform == null
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
  const center = properties.center as IVectorV<number>;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  const w = properties.width as IFloatV<number>;
  const h = properties.height as IFloatV<number>;
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
  const w = properties.width as IFloatV<number>;
  const h = properties.height as IFloatV<number>;
  const center = properties.center;
  const rotation = (properties.rotation as IFloatV<number>).contents;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  let transform = elem.getAttribute("transform");
  transform =
    transform == null
      ? `rotate(${rotation}, ${x - w.contents / 2}, ${y - h.contents / 2})`
      : transform +
        `rotate(${rotation}, ${x - w.contents / 2}, ${y - h.contents / 2})`;
  elem.setAttribute("transform", transform);

  return ["rotation", "center", "width", "height"]; // Return array of input properties programatically mapped
};

/**
 * Maps width, height --> width, height
 */
export const attrWH = ({ properties }: Shape, elem: SVGElement): string[] => {
  const w = properties.width as IFloatV<number>;
  const h = properties.height as IFloatV<number>;
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
  const rx = properties.cornerRadius as IFloatV<number>;
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
  const d = properties.data as IStrV;
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
  const str = properties.string as IStrV;
  const text = document.createTextNode(str.contents.toString());
  elem.appendChild(text);

  return ["string"]; // Return array of input properties programatically mapped
};

export const DASH_ARRAY = "7,5";

/**
 * Maps strokeColor --> stroke, stroke-opacity
 *      strokeWidth --> stroke-width
 *      strokeDashArray, strokeStyle --> stroke-dasharray
 *      strokeLineCap --> stroke-linecap
 */
export const attrStroke = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  // Keep a list of which input properties we programatically mapped
  const attrMapped: string[] = [];

  const strokeColor = properties.strokeColor as IColorV<number>;
  const strokeAlpha = toSvgOpacityProperty(strokeColor.contents);
  const thickness = properties.strokeWidth.contents;
  elem.setAttribute("stroke", toSvgPaintProperty(strokeColor.contents));
  attrMapped.push("strokeColor", "strokeWidth");

  // Stroke opacity, width, and dashiness only relevant if stroke is present
  if (strokeColor.contents.tag !== "NONE") {
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
    } else if (
      "strokeStyle" in properties &&
      properties.strokeStyle.contents === "dashed"
    ) {
      elem.setAttribute("stroke-dasharray", DASH_ARRAY.toString());
      attrMapped.push("strokeDashArray", "strokeStyle");
    }

    if (
      "strokeLineCap" in properties &&
      properties.strokeLineCap.contents !== ""
    ) {
      elem.setAttribute(
        "stroke-linecap",
        (properties.strokeLineCap as IStrV).contents
      );
    } else {
      elem.setAttribute("stroke-linecap", "butt");
    }
    attrMapped.push("strokeLineCap");
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
  const name = properties.name as IStrV;
  const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
  title.textContent = name.contents;
  elem.appendChild(title);

  return ["name"]; // Return array of input properties programatically mapped
};

// Text attributes =============================================================

/**
 * Map fontFamily --> font-family
 *
 * The attributes below cover all of the attributes allowed
 * in an SVG <text> element.  Note, however, that many of
 * these attributes may not be properly rendered by a given
 * program (e.g., browser or vector graphics editor).  For
 * instance, Chrome currently does not support attributes
 * like font-stretch, even though Adobe Illustrator does.
 *
 */
export const attrFontFamily = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const fontFamily = properties.fontFamily as IStrV;
  if (fontFamily.contents !== "") {
    elem.setAttribute("font-family", fontFamily.contents.toString());
  }

  return ["fontFamily"]; // Return array of input properties programatically mapped
};

/**
 * Maps fontSize --> font-size
 */
export const attrFontSize = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const fontSize = properties.fontSize as IStrV;
  if (fontSize.contents !== "") {
    elem.setAttribute("font-size", fontSize.contents.toString());
  }
  return ["fontSize"]; // Return array of input properties programatically mapped
};

/**
 * Maps fontSizeAdjust --> font-size-adjust
 */
export const attrFontSizeAdjust = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const fontSizeAdjust = properties.fontSizeAdjust as IStrV;
  if (fontSizeAdjust.contents !== "") {
    elem.setAttribute("font-size-adjust", fontSizeAdjust.contents.toString());
  }
  return ["fontSizeAdjust"]; // Return array of input properties programatically mapped
};

/**
 * Maps fontStretch --> font-stretch
 */
export const attrFontStretch = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const fontStretch = properties.fontStretch as IStrV;
  if (fontStretch.contents !== "") {
    elem.setAttribute("font-stretch", fontStretch.contents.toString());
  }
  return ["fontStretch"]; // Return array of input properties programatically mapped
};

/**
 * Maps fontStyle --> font-style
 */
export const attrFontStyle = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const fontStyle = properties.fontStyle as IStrV;
  if (fontStyle.contents !== "") {
    elem.setAttribute("font-style", fontStyle.contents.toString());
  }
  return ["fontStyle"]; // Return array of input properties programatically mapped
};

/**
 * Maps fontVariant --> font-variant
 */
export const attrFontVariant = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const fontVariant = properties.fontVariant as IStrV;
  if (fontVariant.contents !== "") {
    elem.setAttribute("font-variant", fontVariant.contents.toString());
  }
  return ["fontVariant"]; // Return array of input properties programatically mapped
};

/**
 * Maps fontWeight --> font-weight
 */
export const attrFontWeight = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const fontWeight = properties.fontWeight as IStrV;
  if (fontWeight.contents !== "") {
    elem.setAttribute("font-weight", fontWeight.contents.toString());
  }
  return ["fontWeight"]; // Return array of input properties programatically mapped
};

/**
 * Maps textAnchor --> text-anchor
 */
export const attrTextAnchor = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const textAnchor = properties.textAnchor as IStrV;
  if (textAnchor.contents !== "") {
    elem.setAttribute("text-anchor", textAnchor.contents.toString());
  }
  return ["textAnchor"]; // Return array of input properties programatically mapped
};

/**
 * Maps alignmentBaseline --> alignment-baseline
 */
export const attrAlignmentBaseline = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const alignmentBaseline = properties.alignmentBaseline as IStrV;
  if (alignmentBaseline.contents !== "") {
    elem.setAttribute(
      "alignment-baseline",
      alignmentBaseline.contents.toString()
    );
    elem.setAttribute(
      "dominant-baseline",
      alignmentBaseline.contents.toString()
    );
  }
  return ["alignmentBaseline"]; // Return array of input properties programatically mapped
};

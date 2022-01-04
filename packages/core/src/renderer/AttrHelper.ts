import { IColorV, IFloatV, IVectorV, IStrV, IPtListV } from "types/value";
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
    if (!attrToNotAutoMap.has(propName.toLowerCase())) {
      if (!elem.hasAttribute(propName)) {
        if (propValue !== "") {
          elem.setAttribute(propName, propValue);
        }
      }
    }
  }
};

/**
 * Maps color --> fill, fill-opacity
 */
export const attrFill = ({ properties }: Shape, elem: SVGElement): string[] => {
  const color = properties.color as IColorV<number>;
  const alpha = toSvgOpacityProperty(color.contents);

  elem.setAttribute("fill", toSvgPaintProperty(color.contents));

  // Fill opacity only relevant if fill is present
  if (color.contents.tag !== "NONE") {
    elem.setAttribute("fill-opacity", alpha.toString());
  }

  return ["color"]; // Return array of input properties programatically mapped
};

/**
 * Maps opacity --> opacity
 */
export const attrOpacity = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const opacity = (properties.opacity as IFloatV<number>).contents;
  elem.setAttribute("opacity", opacity.toString());
  return ["opacity"]; // Return array of input properties programatically mapped
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
 * If center is present:
 *  - Maps center --> cx, cy
 * Else
 *  - Maps points --> cx, cy
 */
export const attrPolyCenter = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
  // Keep a list of which input properties we programatically mapped
  const attrMapped: string[] = [];

  if (properties.center) {
    const [x, y] = toScreen(
      properties.center.contents as [number, number],
      canvasSize
    );
    elem.setAttribute("cx", x.toString());
    elem.setAttribute("cy", y.toString());
    attrMapped.push("center");
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
    attrMapped.push("points");
  }
  return attrMapped; // Return array of input properties programatically mapped
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
 * Maps center, w, h --> transform
 */
export const attrTransformCoords = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
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

  return ["center", "w", "h"]; // Return array of input properties programatically mapped
};

/**
 * Maps center, w, h --> x, y
 */
export const attrXY = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
  const center = properties.center as IVectorV<number>;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  const w = properties.w as IFloatV<number>;
  const h = properties.h as IFloatV<number>;
  elem.setAttribute("x", (x - w.contents / 2).toString());
  elem.setAttribute("y", (y - h.contents / 2).toString());

  return ["center", "w", "h"]; // Return array of input properties programatically mapped
};

/**
 * Maps center, w (or side), h (or side), rotation --> transform !!!
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
  const w = ("side" in properties
    ? properties.side
    : properties.w) as IFloatV<number>; // Handle squares
  const h = ("side" in properties
    ? properties.side
    : properties.h) as IFloatV<number>; // Handle squares
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

  if ("side" in properties) {
    // Handle squares
    return ["rotation", "center", "side"]; // Return array of input properties programatically mapped
  } else {
    return ["rotation", "center", "w", "h"]; // Return array of input properties programatically mapped
  }
};

/**
 * Maps center, side --> transform
 */
export const attrSideCoords = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
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

  return ["center", "side"]; // Return array of input properties programatically mapped
};

/**
 * Maps r --> r
 */
export const attrRadius = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const r = properties.r as IFloatV<number>;
  elem.setAttribute("r", r.contents.toString());

  return ["r"]; // Return array of input properties programatically mapped
};

/**
 * Maps rx --> rx
 */
export const attrRadiusX = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const rx = properties.rx as IFloatV<number>;
  elem.setAttribute("rx", rx.contents.toString());

  return ["rx"]; // Return array of input properties programatically mapped
};

/**
 * Maps ry --> ry
 */
export const attrRadiusY = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const ry = properties.ry as IFloatV<number>;
  elem.setAttribute("ry", ry.contents.toString());

  return ["ry"]; // Return array of input properties programatically mapped
};

/**
 * Maps rx, ry --> rx, ry
 */
export const attrRadii = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const rx = properties.rx as IFloatV<number>;
  const ry = properties.ry as IFloatV<number>;
  elem.setAttribute("rx", rx.contents.toString());
  elem.setAttribute("ry", ry.contents.toString());

  return ["rx", "ry"]; // Return array of input properties programatically mapped
};

/**
 * Maps w, h --> width, height
 */
export const attrWH = ({ properties }: Shape, elem: SVGElement): string[] => {
  const w = properties.w as IFloatV<number>;
  const h = properties.h as IFloatV<number>;
  elem.setAttribute("width", w.contents.toString());
  elem.setAttribute("height", h.contents.toString());

  return ["w", "h"]; // Return array of input properties programatically mapped
};

/**
 * Maps points --> points
 */
export const attrPoints = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const points = properties.points as IPtListV<number>;
  elem.setAttribute("points", points.contents.toString());

  return ["points"]; // Return array of input properties programatically mapped
};

/**
 * Maps side --> width, height
 */
export const attrSide = ({ properties }: Shape, elem: SVGElement): string[] => {
  const side = properties.side as IFloatV<number>;
  elem.setAttribute("width", side.contents.toString());
  elem.setAttribute("height", side.contents.toString());

  return ["side"]; // Return array of input properties programatically mapped
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

/**
 * Map visibility --> visibility
 
* The SVG attribute "visibility" can be set to
 * "visible" or "hidden" to show/hide elements.
 */
export const attrVisibility = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const visibility = properties.visibility as IStrV;
  if (visibility.contents !== "") {
    elem.setAttribute("visibility", visibility.contents.toString());
  }

  return ["visibility"]; // Return array of input properties programatically mapped
};

/**
 * Map style --> style
 *
 * In SVG, the attribute "style" is a catch-all that allows
 * a tag to be styled using an arbitrary CSS string.  This
 * attribute is often a better way to get certain attributes
 * to appear correctly in the browser than one-off attributes
 * associated with particular tags.  For instance, the SVG
 * attribute stroke-width="4" appears not to work on text in
 * many browsers, whereas style="stroke-width:4;" appears to
 * work just fine.
 */
export const attrStyle = (
  { properties }: Shape,
  elem: SVGElement
): string[] => {
  const style = properties.style as IStrV;
  if (style.contents !== "") {
    elem.setAttribute("style", style.contents.toString());
  }

  return ["style"]; // Return array of input properties programatically mapped
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
  }
  return ["alignmentBaseline"]; // Return array of input properties programatically mapped
};

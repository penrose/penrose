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
import { toSvgPaintProperty, toScreen, toSvgOpacityProperty } from "utils/Util";

export const attrFill = ({ properties }: Shape, elem: SVGElement) => {
  const color = properties.color as IColorV<number>;
  const alpha = toSvgOpacityProperty(color.contents);
  elem.setAttribute("fill", toSvgPaintProperty(color.contents));
  // Fill opacity only relevant if fill is present
  if(color.contents.tag !== "NONE") {
    elem.setAttribute("fill-opacity", alpha.toString());
  }
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
  let scale = properties.scale.contents;
  scale = scale || 1;
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
  const d = properties.data as IStrV;
  elem.setAttribute("d", d.contents.toString());
};

export const attrString = ({ properties }: Shape, elem: SVGElement) => {
  const str = properties.string as IStrV;
  const text = document.createTextNode(str.contents.toString());
  elem.appendChild(text);
};

//export const attrTitle = ({ properties }: Shape, elem: SVGElement) => {
//  const name = properties.name as IStrV;
//  const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
//  title.textContent = name.contents;
//  elem.appendChild(title);
//};



export const DASH_ARRAY = "7,5";

export const attrStroke = ({ properties }: Shape, elem: SVGElement) => {
  const strokeColor = properties.strokeColor as IColorV<number>;
  const strokeAlpha = toSvgOpacityProperty(strokeColor.contents);
  const thickness = properties.strokeWidth.contents;
  elem.setAttribute("stroke", toSvgPaintProperty(strokeColor.contents));
  // Stroke opacity, width, and dashiness only relevant if stroke is present
  if(strokeColor.contents.tag !== "NONE") {
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
  }
};

export const attrTitle = ({ properties }: Shape, elem: SVGElement) => {
  const name = properties.name as IStrV;
  const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
  title.textContent = name.contents;
  elem.appendChild(title);
};


/* In SVG, the attribute "style" is a catch-all that allows
 * a tag to be styled using an arbitrary CSS string.  This
 * attribute is often a better way to get certain attributes
 * to appear correctly in the browser than one-off attributes
 * associated with particular tags.  For instance, the SVG
 * attribute stroke-width="4" appears not to work on text in
 * many browsers, whereas style="stroke-width:4;" appears to
 * work just fine.
 */
export const attrStyle = ({ properties }: Shape, elem: SVGElement) => {
  const style = properties.style as IStrV;
  if( style.contents !== "" ) {
     elem.setAttribute("style", style.contents.toString());
  }
};

// Text attributes =============================================================
/*
 * The attributes below cover all of the attributes allowed
 * in an SVG <text> element.  Note, however, that many of
 * these attributes may not be properly rendered by a given
 * program (e.g., browser or vector graphics editor).  For
 * instance, Chrome currently does not support attributes
 * like font-stretch, even though Adobe Illustrator does.
 *
 */

export const attrFontFamily = ({ properties }: Shape, elem: SVGElement) => {
  const fontFamily = properties.fontFamily as IStrV;
  if( fontFamily.contents !== "" ) {
     elem.setAttribute("font-family", fontFamily.contents.toString());
  }
};

export const attrFontSize = ({ properties }: Shape, elem: SVGElement) => {
  const fontSize = properties.fontSize as IStrV;
  if( fontSize.contents !== "" ) {
     elem.setAttribute("font-size", fontSize.contents.toString());
  }
};

export const attrFontSizeAdjust = ({ properties }: Shape, elem: SVGElement) => {
  const fontSizeAdjust = properties.fontSizeAdjust as IStrV;
  if( fontSizeAdjust.contents !== "" ) {
     elem.setAttribute("font-size-adjust", fontSizeAdjust.contents.toString());
  }
};

export const attrFontStretch = ({ properties }: Shape, elem: SVGElement) => {
  const fontStretch = properties.fontStretch as IStrV;
  if( fontStretch.contents !== "" ) {
     elem.setAttribute("font-stretch", fontStretch.contents.toString());
  }
};

export const attrFontStyle = ({ properties }: Shape, elem: SVGElement) => {
  const fontStyle = properties.fontStyle as IStrV;
  if( fontStyle.contents !== "" ) {
     elem.setAttribute("font-style", fontStyle.contents.toString());
  }
};

export const attrFontVariant = ({ properties }: Shape, elem: SVGElement) => {
  const fontVariant = properties.fontVariant as IStrV;
  if( fontVariant.contents !== "" ) {
     elem.setAttribute("font-variant", fontVariant.contents.toString());
  }
};

export const attrFontWeight = ({ properties }: Shape, elem: SVGElement) => {
  const fontWeight = properties.fontWeight as IStrV;
  if( fontWeight.contents !== "" ) {
     elem.setAttribute("font-weight", fontWeight.contents.toString());
  }
};



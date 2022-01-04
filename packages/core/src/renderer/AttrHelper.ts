import {
  IColorV,
  IFloatV,
  IVectorV,
  IStrV,
  IPtListV,
  Value,
} from "types/value";
import { Shape } from "types/shape";
import { toSvgPaintProperty, toScreen, toSvgOpacityProperty } from "utils/Util";

export const attrFill = ({ properties }: Shape, elem: SVGElement): string[] => {
  const color = properties.color as IColorV<number>;
  const alpha = toSvgOpacityProperty(color.contents);

  // Keep a list of which properties we programatically mapped
  const attrMapped: string[] = [];

  elem.setAttribute("fill", toSvgPaintProperty(color.contents));
  attrMapped.concat(['color','fill']);

  // Fill opacity only relevant if fill is present
  if(color.contents.tag !== "NONE") {
    elem.setAttribute("fill-opacity", alpha.toString());
    attrMapped.concat(['fill-opacity']);
  }

  return attrMapped; // Return array of attributes programatically mapped
};

export const attrAutoFillSvg = ({ properties }: Shape, elem: SVGElement, attrAlreadyMapped: string[]): void => {
  // Lowercase the array entries & load into a Set
  console.debug(`Input array of already mapped attributes: ${attrAlreadyMapped.toString()}`)
  const attrAlreadyMappedLower = attrAlreadyMapped.map(name => name.toLowerCase());
  const attrAlreadyMappedSet = new Set<string>(attrAlreadyMappedLower);
  
  let attrTemp: string[] = [];
  for (const attrVal of attrAlreadyMappedSet) {attrTemp = attrTemp.concat(attrVal);}
  console.debug(`Final set of already mapped attributes: ${attrTemp.toString()}`)

  // Map unknown/unseen attributes to SVG output.
  for(const propName in properties) {
    const propValue: string = properties[propName].contents.toString();
    // Skip any attributes previously mapped (both input and output sides).
    if(!attrAlreadyMappedSet.has(propName.toLowerCase())) {
      // Skip any attributes that already exist in outpout
      if(!elem.hasAttribute(propName)) {
        // Skip blank values
        if(propValue!=='') {
          elem.setAttribute(propName, propValue);
          console.debug(`automapping unknown property [${propName}]=[${propValue}] to SVG`);
        } else {
          //console.debug(`skipping empty property      [${propName}]=[${propValue}]`);
        }
      } else {
        //console.debug(`skipping now-extant property [${propName}]=[${propValue}]`);
      }
    } else {
      //console.debug(`skipping pre-mapped property [${propName}]=[${propValue}]`);
    }
  }
};

export const attrNoFill = ({ properties }: Shape, elem: SVGElement) : string[] => {
  elem.setAttribute("fill", "none");
  return ['fill']; // Return array of attributes programatically mapped
};

export const attrOpacity = ({ properties }: Shape, elem: SVGElement): string[] => {
  const opacity = (properties.opacity as IFloatV<number>).contents;
  elem.setAttribute("opacity", opacity.toString());
  return ['opacity']; // Return array of attributes programatically mapped
};

export const attrCenter  = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
  const center = properties.center as IVectorV<number>;
  const [x, y] = toScreen(center.contents as [number, number], canvasSize);
  elem.setAttribute("cx", x.toString());
  elem.setAttribute("cy", y.toString());
  return ['center','cx','cy']; // Return array of attributes programatically mapped
};

export const attrPolyCenter = (
  { properties }: Shape,
  canvasSize: [number, number],
  elem: SVGElement
): string[] => {
  // Keep a list of which properties we programatically mapped
  const attrMapped: string[] = [];
  
  if (properties.center) {
    const [x, y] = toScreen(
      properties.center.contents as [number, number],
      canvasSize
    );
    elem.setAttribute("cx", x.toString());
    elem.setAttribute("cy", y.toString());
    attrMapped.concat('center','cx','cy');
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
    attrMapped.concat('points','cx','cy');
  }
  return attrMapped; // Return array of attributes programatically mapped
};

export const attrScale = ({ properties }: Shape, elem: SVGElement): string[] => {
  let scale = properties.scale.contents;
  scale = scale || 1;
  let transform = elem.getAttribute("transform");
  transform =
    transform == null ? `scale(${scale})` : transform + `scale{${scale}}`;
  elem.setAttribute("transform", transform);

  return ['scale', 'transform']; // Return array of attributes programatically mapped
};

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

  return ['center','w','h','transform']; // Return array of attributes programatically mapped
};

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

  return ['center','w','h','x','y']; // Return array of attributes programatically mapped
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
): string[] => {
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

  return ['rotation','center','transform']; // Return array of attributes programatically mapped
};

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

  return ['center','side','transform']; // Return array of attributes programatically mapped
};

export const attrRadius = ({ properties }: Shape, elem: SVGElement): string[] => {
  const r = properties.r as IFloatV<number>;
  elem.setAttribute("r", r.contents.toString());

  return ['r']; // Return array of attributes programatically mapped
};

export const attrPathLength = ({ properties }: Shape, elem: SVGElement): string[] => {
  const pathLength = properties.pathLength as IFloatV<number>;
  elem.setAttribute("pathLength", pathLength.contents.toString());

  return ['pathLength']; // Return array of attributes programatically mapped
};

export const attrRadiusX = ({ properties }: Shape, elem: SVGElement): string[] => {
  const rx = properties.rx as IFloatV<number>;
  elem.setAttribute("rx", rx.contents.toString());

  return ['rx']; // Return array of attributes programatically mapped
};

export const attrRadiusY = ({ properties }: Shape, elem: SVGElement): string[] => {
  const ry = properties.ry as IFloatV<number>;
  elem.setAttribute("ry", ry.contents.toString());

  return ['ry']; // Return array of attributes programatically mapped
};

export const attrRadii = ({ properties }: Shape, elem: SVGElement): string[] => {
  const rx = properties.rx as IFloatV<number>;
  const ry = properties.ry as IFloatV<number>;
  elem.setAttribute("rx", rx.contents.toString());
  elem.setAttribute("ry", ry.contents.toString());

  return ['rx','ry']; // Return array of attributes programatically mapped
};

export const attrWH = ({ properties }: Shape, elem: SVGElement): string[] => {
  const w = properties.w as IFloatV<number>;
  const h = properties.h as IFloatV<number>;
  elem.setAttribute("width", w.contents.toString());
  elem.setAttribute("height", h.contents.toString());

  return ['w','h','width','height']; // Return array of attributes programatically mapped
};

export const attrPoints = ({ properties }: Shape, elem: SVGElement): string[] => {
  const points = properties.points as IPtListV<number>;
  elem.setAttribute("points", points.contents.toString());

  return ['points']; // Return array of attributes programatically mapped
};

export const attrSide = ({ properties }: Shape, elem: SVGElement): string[] => {
  const side = properties.side as IFloatV<number>;
  elem.setAttribute("width", side.contents.toString());
  elem.setAttribute("height", side.contents.toString());

  return ['side','width','height']; // Return array of attributes programatically mapped
};

export const attrPathData = ({ properties }: Shape, elem: SVGElement): string[] => {
  const d = properties.data as IStrV;
  elem.setAttribute("d", d.contents.toString());

  return ['d','data']; // Return array of attributes programatically mapped
};

export const attrString = ({ properties }: Shape, elem: SVGElement): string[] => {
  const str = properties.string as IStrV;
  const text = document.createTextNode(str.contents.toString());
  elem.appendChild(text);

  return ['string']; // Return array of attributes programatically mapped
};

export const DASH_ARRAY = "7,5";

export const attrStroke = ({ properties }: Shape, elem: SVGElement): string[] => {
  // Keep a list of which properties we programatically mapped
  const attrMapped: string[] = ['stroke','strokeColor','strokeWidth'];

  const strokeColor = properties.strokeColor as IColorV<number>;
  const strokeAlpha = toSvgOpacityProperty(strokeColor.contents);
  const thickness = properties.strokeWidth.contents;
  elem.setAttribute("stroke", toSvgPaintProperty(strokeColor.contents));

  // Stroke opacity, width, and dashiness only relevant if stroke is present
  if(strokeColor.contents.tag !== "NONE") {
   elem.setAttribute("stroke-opacity", strokeAlpha.toString());
    elem.setAttribute("stroke-width", thickness.toString());
    attrMapped.concat(['stroke-opacity','stroke-width']);

    if (
      "strokeDashArray" in properties &&
      properties.strokeDashArray.contents !== ""
    ) {
      elem.setAttribute(
        "stroke-dasharray",
        (properties.strokeDashArray as IStrV).contents
      );
      attrMapped.concat(['stroke-dasharray','strokeDashArray']);
    } else if (
       "strokeStyle" in properties &&
       properties.strokeStyle.contents === "dashed"
    ) {
      elem.setAttribute("stroke-dasharray", DASH_ARRAY.toString());
      attrMapped.concat(['stroke-dasharray','strokeDashArray','strokeStyle']);
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
    attrMapped.concat(['stroke-linecap','strokeLineCap']);
  }

  return attrMapped; // Return array of attributes programatically mapped
};

export const attrTitle = ({ properties }: Shape, elem: SVGElement): string[] => {
  const name = properties.name as IStrV;
  const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
  title.textContent = name.contents;
  elem.appendChild(title);

  return ['name']; // Return array of attributes programatically mapped
};

/* The SVG attribute "visibility" can be set to
 * "visible" or "hidden" to show/hide elements.
 */
export const attrVisibility = ({ properties }: Shape, elem: SVGElement): string[] => {
  const visibility = properties.visibility as IStrV;
  if( visibility.contents !== "" ) {
     elem.setAttribute("visibility", visibility.contents.toString());
  }

  return ['visibility']; // Return array of attributes programatically mapped
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
export const attrStyle = ({ properties }: Shape, elem: SVGElement): string[] => {
  const style = properties.style as IStrV;
  if( style.contents !== "" ) {
     elem.setAttribute("style", style.contents.toString());
  }

  return ['style']; // Return array of attributes programatically mapped
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

export const attrFontFamily = ({ properties }: Shape, elem: SVGElement): string[] => {
  const fontFamily = properties.fontFamily as IStrV;
  if( fontFamily.contents !== "" ) {
     elem.setAttribute("font-family", fontFamily.contents.toString());
  }

  return ['fontFamily','font-family']; // Return array of attributes programatically mapped
};

export const attrFontSize = ({ properties }: Shape, elem: SVGElement): string[] => {
  const fontSize = properties.fontSize as IStrV;
  if( fontSize.contents !== "" ) {
     elem.setAttribute("font-size", fontSize.contents.toString());
  }

  return ['font-size','fontSize']; // Return array of attributes programatically mapped
};

export const attrFontSizeAdjust = ({ properties }: Shape, elem: SVGElement): string[] => {
  const fontSizeAdjust = properties.fontSizeAdjust as IStrV;
  if( fontSizeAdjust.contents !== "" ) {
     elem.setAttribute("font-size-adjust", fontSizeAdjust.contents.toString());
  }
  return ['font-size-adjust','fontSizeAdjust']; // Return array of attributes programatically mapped
};

export const attrFontStretch = ({ properties }: Shape, elem: SVGElement): string[] => {
  const fontStretch = properties.fontStretch as IStrV;
  if( fontStretch.contents !== "" ) {
     elem.setAttribute("font-stretch", fontStretch.contents.toString());
  }
  return ['font-stretch','fontStretch']; // Return array of attributes programatically mapped
};

export const attrFontStyle = ({ properties }: Shape, elem: SVGElement): string[] => {
  const fontStyle = properties.fontStyle as IStrV;
  if( fontStyle.contents !== "" ) {
     elem.setAttribute("font-style", fontStyle.contents.toString());
  }
  return ['font-style','fontStyle']; // Return array of attributes programatically mapped
};

export const attrFontVariant = ({ properties }: Shape, elem: SVGElement): string[] => {
  const fontVariant = properties.fontVariant as IStrV;
  if( fontVariant.contents !== "" ) {
     elem.setAttribute("font-variant", fontVariant.contents.toString());
  }
  return ['font-variant','fontVariant']; // Return array of attributes programatically mapped
};

export const attrFontWeight = ({ properties }: Shape, elem: SVGElement): string[] => {
  const fontWeight = properties.fontWeight as IStrV;
  if( fontWeight.contents !== "" ) {
     elem.setAttribute("font-weight", fontWeight.contents.toString());
  }
  return ['font-weight','fontWeight']; // Return array of attributes programatically mapped
};

export const attrTextAnchor = ({ properties }: Shape, elem: SVGElement): string[] => {
  const textAnchor = properties.textAnchor as IStrV;
  if( textAnchor.contents !== "" ) {
     elem.setAttribute("text-anchor", textAnchor.contents.toString());
  }
  return ['text-anchor','textAnchor']; // Return array of attributes programatically mapped
};

export const attrAlignmentBaseline = ({ properties }: Shape, elem: SVGElement): string[] => {
  const alignmentBaseline = properties.alignmentBaseline as IStrV;
  if( alignmentBaseline.contents !== "" ) {
     elem.setAttribute("alignment-baseline", alignmentBaseline.contents.toString());
  }
  return ['alignment-baseline','alignmentBaseline']; // Return array of attributes programatically mapped
};



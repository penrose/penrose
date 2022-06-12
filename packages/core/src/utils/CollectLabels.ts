import { browserAdaptor } from "mathjax-full/js/adaptors/browserAdaptor.js";
import { RegisterHTMLHandler } from "mathjax-full/js/handlers/html.js";
import { TeX } from "mathjax-full/js/input/tex.js";
import { AllPackages } from "mathjax-full/js/input/tex/AllPackages.js";
import { mathjax } from "mathjax-full/js/mathjax.js";
import { SVG } from "mathjax-full/js/output/svg.js";
import { InputMeta } from "shapes/Samplers";
import { ShapeDef, shapedefs } from "shapes/Shapes";
import svgpath from "svgpath";
import * as ad from "types/ad";
import { PenroseError } from "types/errors";
import { Properties, ShapeAD } from "types/shape";
import { EquationData, LabelCache, State, TextData } from "types/state";
import { FloatV } from "types/value";
import { err, ok, Result } from "./Error";
import {
  getAdValueAsColor,
  getAdValueAsNumber,
  getAdValueAsString,
} from "./Util";
/* eslint eslint-comments/no-use: off */
// eslint-disable-next-line @typescript-eslint/no-var-requires
const svgflatten = require("svg-flatten");

// https://github.com/mathjax/MathJax-demos-node/blob/master/direct/tex2svg
// const adaptor = chooseAdaptor();
const adaptor = browserAdaptor();
RegisterHTMLHandler(adaptor);
const tex = new TeX({
  packages: AllPackages,
  macros: {
    textsc: ["\\style{font-variant-caps: small-caps}{\\text{#1}}", 1],
  },
  inlineMath: [
    ["$", "$"],
    ["\\(", "\\)"],
  ],
  processEscapes: true,
  // https://github.com/mathjax/MathJax-demos-node/issues/25#issuecomment-711247252
  formatError: (jax: unknown, err: Error) => {
    throw Error(err.message);
  },
});
const svg = new SVG({ fontCache: "none" });
const html = mathjax.document("", { InputJax: tex, OutputJax: svg });

// to re-scale baseline
const EX_CONSTANT = 10;

const convert = (input: string): Result<HTMLElement, string> => {
  // HACK: workaround for newlines
  // https://github.com/mathjax/MathJax/issues/2312#issuecomment-538185951
  const newline_escaped = `\\displaylines{${input}}`;
  // https://github.com/mathjax/MathJax-src/blob/master/ts/core/MathDocument.ts#L689
  // https://github.com/mathjax/MathJax-demos-node/issues/3#issuecomment-497524041
  try {
    const node = html.convert(newline_escaped, { ex: EX_CONSTANT });
    return ok(node.firstChild);
  } catch (error: any) {
    return err(error.message);
  }
};

type Output = {
  body: HTMLElement;
  width: number;
  height: number;
};

/**
 * Call MathJax to render __non-empty__ labels.
 * Re-scale the label so it is 1:1 with the canvas to accomodate
 * properties such as stroke
 */
const tex2svg = async (
  properties: Properties<ad.Num>
): Promise<Result<Output, string>> =>
  new Promise((resolve) => {
    const labelInput = getAdValueAsString(properties.string);
    const fontSize = getAdValueAsString(properties.fontSize);
    const strokeColor = getAdValueAsColor(properties.strokeColor);
    const strokeWidth =
      strokeColor.tag === "NONE"
        ? 0
        : getAdValueAsNumber(properties.strokeWidth);

    // Render the label
    const output = convert(labelInput);
    if (output.isErr()) {
      resolve(err(`MathJax could not render $${labelInput}$: ${output.error}`));
      return;
    }
    let labelSvg = output.value;
    const labelSvgViewBox = labelSvg.getAttribute("viewBox");
    if (labelSvgViewBox === null) {
      resolve(err(`No ViewBox found for MathJax output $${labelInput}$`));
      return;
    }

    // Get the rendered viewBox dimensions
    const labelSvgViewBoxArr = labelSvgViewBox.split(" ");
    const origWidth = parseFloat(labelSvgViewBoxArr[2]);
    const origHeight = parseFloat(labelSvgViewBoxArr[3]);

    // Get re-scaled dimensions of label according to
    // https://github.com/mathjax/MathJax-src/blob/32213009962a887e262d9930adcfb468da4967ce/ts/output/svg.ts#L248
    const vAlignFloat = parseFloat(labelSvg.style.verticalAlign) * EX_CONSTANT;
    const scaledHeight = parseFloat(fontSize) - vAlignFloat;
    const scaledRatio = scaledHeight / origHeight;
    const scaledWidth = scaledRatio * origWidth;

    // Flatten the label & merge paths into a single path
    labelSvg = new DOMParser().parseFromString(
      new svgflatten(labelSvg.outerHTML)
        .pathify() // convert all gpis to paths
        .transform() // push all transforms down to paths
        .flatten() // merge all paths into a single path
        .transform() // push all transforms down to paths (yes, again)
        .value(), // get the string rep of the SVG
      "image/svg+xml"
    ).documentElement;

    // Re-scale the label path, use absolute coords, convert arcs to bezier curves..
    const paths = labelSvg.getElementsByTagName("path");
    for (const path in paths) {
      const thisPath = paths[path];
      if (typeof thisPath === "object") {
        const thisD = thisPath.getAttribute("d") || "";
        thisPath.setAttribute(
          "d",
          svgpath(thisD)
            .scale(scaledRatio) // re-scale the label
            .abs() // use absolute coordinates (for BBox)
            .unarc() // replace arcs with bezier curves (for BBox)
            .toString() // get the string rep of the SVG
        );
      }
    }

    // Set the label's dimensions & accomodate stroke width
    const finalWidth = scaledWidth + strokeWidth * 2;
    const finalHeight = scaledHeight + strokeWidth * 2;
    labelSvg.setAttribute("width", finalWidth.toString());
    labelSvg.setAttribute("height", finalHeight.toString());

    // Set the viewBox to the rescaled dimensions, including stroke width
    const newViewBox = getPathBBox(labelSvg.getElementsByTagName("path")[0]);
    labelSvg.setAttribute(
      "viewBox",
      `${newViewBox.x - strokeWidth} ${newViewBox.y - strokeWidth} ${
        newViewBox.width + strokeWidth * 2
      } ${newViewBox.height + strokeWidth * 2}`
    );

    resolve(ok({ body: labelSvg, width: finalWidth, height: finalHeight }));
  });

const floatV = (contents: number): FloatV<number> => ({
  tag: "FloatV",
  contents,
});

const textData = (
  width: number,
  height: number,
  descent: number,
  ascent: number
): TextData => ({
  tag: "TextData",
  width: floatV(width),
  height: floatV(height),
  descent: floatV(descent),
  ascent: floatV(ascent),
});

const equationData = (
  width: number,
  height: number,
  rendered: HTMLElement
): EquationData => ({
  tag: "EquationData",
  width: floatV(width),
  height: floatV(height),
  rendered,
});

/**
 * Get the CSS string for the font setting of a `Text` GPI.
 * @param shape A text GPI
 *
 * NOTE: the `font` CSS rule -> https://developer.mozilla.org/en-US/docs/Web/CSS/font
 *
 * @returns a CSS rule string of its font settings
 */
export const toFontRule = ({ properties }: ShapeAD): string => {
  const fontFamily = getAdValueAsString(properties.fontFamily);
  const fontSize = getAdValueAsString(properties.fontSize);
  const fontStretch = getAdValueAsString(properties.fontStretch);
  const fontStyle = getAdValueAsString(properties.fontStyle);
  const fontVariant = getAdValueAsString(properties.fontVariant);
  const fontWeight = getAdValueAsString(properties.fontWeight);
  const lineHeight = getAdValueAsString(properties.lineHeight);
  /**
   * assemble according to the rules in https://developer.mozilla.org/en-US/docs/Web/CSS/font
   * it must include values for: <font-size> <font-family>
   * it may optionally include values for: <font-style> <font-variant> <font-weight> <font-stretch> <line-height>
   * font-style, font-variant and font-weight must precede font-size
   * font-variant may only specify the values defined in CSS 2.1, that is normal and small-caps
   * font-stretch may only be a single keyword value.
   * line-height must immediately follow font-size, preceded by "/", like this: "16px/3"
   * font-family must be the last value specified.
   */
  const fontSpec = `${fontStretch} ${fontStyle} ${fontVariant} ${fontWeight} ${fontSize} ${fontFamily}`;
  const fontString =
    lineHeight !== "" ? fontSpec.concat(`/${lineHeight}`) : fontSpec;
  return fontString;
};

// https://stackoverflow.com/a/44564236
export const collectLabels = async (
  allShapes: ShapeAD[]
): Promise<Result<LabelCache, PenroseError>> => {
  const labels: LabelCache = new Map();
  for (const s of allShapes) {
    const { shapeType, properties } = s;
    if (shapeType === "Equation" || shapeType === "EquationTransform") {
      const shapeName = getAdValueAsString(properties.name);
      const svg = await tex2svg(properties);

      if (svg.isErr()) {
        return err({
          errorType: "SubstanceError",
          tag: "Fatal",
          message: svg.error,
        });
      }

      const { body, width, height } = svg.value;

      // Instead of directly overwriting the properties, cache them temporarily
      // NOTE: in the case of empty strings, `tex2svg` returns infinity sometimes. Convert to 0 to avoid NaNs in such cases.
      const label: EquationData = equationData(
        width === Infinity ? 0 : width,
        height === Infinity ? 0 : height,
        body
      );
      labels.set(shapeName, label);
    } else if (shapeType === "Text") {
      const shapeName: string = getAdValueAsString(properties.name);
      let label: TextData;
      // Use canvas to measure text data
      const measure: TextMeasurement = measureText(
        getAdValueAsString(properties.string),
        toFontRule(s)
      );

      // If the width and height are defined, the renderer will render the text. `actualDescent` is currently not used in rendering.
      if (measure.width && measure.height) {
        label = textData(
          measure.width,
          measure.height,
          measure.actualDescent,
          measure.actualAscent
        );
      } else {
        label = textData(0, 0, 0, 0);
      }
      labels.set(shapeName, label);
    }
  }
  return ok(labels);
};

//#region Text measurement
export type TextMeasurement = {
  width: number;
  height: number;
  actualDescent: number;
  actualAscent: number;
};

const measureTextElement = document.createElement("canvas");
const measureTextContext = measureTextElement.getContext("2d")!;

/**
 *
 * @param text the content of the text
 * @param font the CSS font rule for the text
 *
 * NOTE: the `font` CSS rule -> https://developer.mozilla.org/en-US/docs/Web/CSS/font
 * @returns `TextMeasurement` object and includes data such as `width` and `height` of the text.
 */
export function measureText(text: string, font: string): TextMeasurement {
  measureTextContext.textBaseline = "alphabetic";
  measureTextContext.font = font;
  const measurements = measureTextContext.measureText(text);
  return {
    width:
      Math.abs(measurements.actualBoundingBoxLeft) +
      Math.abs(measurements.actualBoundingBoxRight),
    height:
      Math.abs(measurements.actualBoundingBoxAscent) +
      Math.abs(measurements.actualBoundingBoxDescent),
    actualDescent: Math.abs(measurements.actualBoundingBoxDescent),
    actualAscent: Math.abs(measurements.actualBoundingBoxAscent),
  };
}

//#endregion

const setPendingProperty = (
  properties: Properties<ad.Num>,
  propertyID: string,
  newValue: FloatV<number>,
  xs: number[],
  meta: InputMeta[]
) => {
  const value = properties[propertyID];
  if (value.tag === "FloatV") {
    const x = value.contents;
    if (
      typeof x !== "number" &&
      x.tag === "Input" &&
      "pending" in meta[x.key]
    ) {
      xs[x.key] = newValue.contents;
    }
  }
};

export const insertPending = (state: State): State => {
  const varyingValues = [...state.varyingValues];
  for (const { shapeType, properties } of state.shapes) {
    const shapedef: ShapeDef = shapedefs[shapeType];
    if (properties.name.tag === "StrV") {
      const labelData = state.labelCache.get(properties.name.contents);
      if (labelData !== undefined) {
        for (const propertyID of shapedef.pendingProps) {
          setPendingProperty(
            properties,
            propertyID,
            labelData[propertyID],
            varyingValues,
            state.inputs
          );
        }
      }
    }
  }
  return { ...state, varyingValues };
};

/**
 * Provides a structure similar to SVGRect where x, y = canvas origin
 * and width, height = canvas size
 */
type PseudoSVGRect = {
  x: number;
  y: number;
  width: number;
  height: number;
};

/**
 * Gets the approximate bounding box of an SVG path element.
 * We do this here as jsDOM lacks BBox support for SVG.  There is
 * significant opportunity to use more-precise methods, but browsers
 * approximate the bounding box of curves using the control points
 * rather than calculating based on the actual path.
 *
 * Note: This routine requires only absolute coordinate paths and
 *       requires arcs be converted to curves by a prior routine.
 *       Also, stroke-widths are not considered here.
 *
 * @param path The SVGPathElement to get the bounding box of
 * @returns The approximate bounding box of the path
 */
const getPathBBox = (path: SVGPathElement): PseudoSVGRect => {
  const moveX = (x: number) => {
    xmax = Math.max(x, xmax);
    xmin = Math.min(x, xmin);
    cursor[0] = x;
  };
  const moveY = (y: number) => {
    ymax = Math.max(y, ymax);
    ymin = Math.min(y, ymin);
    cursor[1] = y;
  };

  const paths = parsePath(path.getAttribute("d") || "");

  let xmin = Infinity;
  let xmax = -Infinity;
  let ymin = Infinity;
  let ymax = -Infinity;

  let cursor = [xmin, ymin];
  let start = [xmin, ymin];
  let isX = true;

  for (const i in paths) {
    const thisPath = paths[i];
    switch (thisPath.type) {
      case "H": // Horizontal line
        thisPath.args.forEach((e) => {
          moveX(e);
        });
        break;
      case "V": // Vertical line
        thisPath.args.forEach((e) => {
          moveY(e);
        });
        break;
      case "Z": // Close path (return to beginning)
        cursor = start;
        start = [xmin, ymin];
        break;
      case "M": // Move
      case "L": // Line
      case "C": // Cubic Bezier curve (cointrol point approximation!)
      case "S": // Strung-together cubic Bezier curve (cointrol point approximation!)
      case "Q": // Quadratic Bezier curve (cointrol point approximation!)
      case "T": // Strung-together quadratic Bezier curve (cointrol point approximation!)
        isX = true;
        thisPath.args.forEach((e) => {
          if (isX) moveX(e);
          else moveY(e);
          isX = !isX;
        });
        break;
      default:
        // Unsupported path type (might be an arc or a relative path)
        throw new Error(`Unsupported path type: ${thisPath.type}`);
    }

    // If we lack a path starting point and did not just process a Z path
    // close, then set the path start to the current cursor position.
    if (start[0] === -Infinity && thisPath.type !== "Z") start = cursor;
  }

  return {
    x: xmin,
    y: ymin,
    width: Math.abs(xmax - xmin),
    height: Math.abs(ymax - ymin),
  };
};

/**
 * A single command in a Path statement.  E.g., "M 100 100", "Z", etc.
 * where type is the command type and args is the array of arguments.
 */
type PathCommand = {
  type: string;
  args: number[];
};

/**
 * Parses a path string into an array of path commands.
 *
 * @param pathStr The path string to parse
 * @returns Array of PathCommands
 */
const parsePath = (pathStr: string): PathCommand[] => {
  const outStmts: PathCommand[] = [];
  const pathStmts: string[] = pathStr
    .replace(/[MmLlHhVvZzCcSsQqTtAa]/g, "@$&") // find all path commands
    .split("@") // split path commands into array
    .map(
      (e) =>
        e
          .replaceAll("-", "@-") // split at negative numbers
          .replaceAll("  ", " ") // normalize spaces
          .trim() // normalize spaces
          .replaceAll(" ", "@") // split at non-negative numbers
    )
    .filter((e) => e.length > 0);

  for (const pathStmt in pathStmts) {
    const thisStmt = pathStmts[pathStmt];
    outStmts.push({
      type: thisStmt[0],
      args: thisStmt
        .slice(1) // remove command type
        .split("@") // split arguments
        .filter((e) => e) // exclude null arguments)
        .map((e) => parseFloat(e)), // convert to number
    });
  }

  return outStmts;
};

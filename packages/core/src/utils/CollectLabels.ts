import { browserAdaptor } from "mathjax-full/js/adaptors/browserAdaptor.js";
import { RegisterHTMLHandler } from "mathjax-full/js/handlers/html.js";
import { TeX } from "mathjax-full/js/input/tex.js";
import { AllPackages } from "mathjax-full/js/input/tex/AllPackages.js";
import { mathjax } from "mathjax-full/js/mathjax.js";
import { SVG } from "mathjax-full/js/output/svg.js";
import { InputMeta } from "shapes/Samplers";
import { ShapeDef, shapedefs } from "shapes/Shapes";
import * as ad from "types/ad";
import { PenroseError } from "types/errors";
import { Properties, ShapeAD } from "types/shape";
import { EquationData, LabelCache, State, TextData } from "types/state";
import { FloatV } from "types/value";
import { err, ok, Result } from "./Error";
import { getAdValueAsString } from "./Util";

// to re-scale baseline
const EX_CONSTANT = 10;

const convert = (
  input: string,
  fontSize: string
): Result<HTMLElement, string> => {
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
  // HACK: workaround for newlines
  // https://github.com/mathjax/MathJax/issues/2312#issuecomment-538185951
  const newline_escaped = `\\displaylines{${input}}`;
  // https://github.com/mathjax/MathJax-src/blob/master/ts/core/MathDocument.ts#L689
  // https://github.com/mathjax/MathJax-demos-node/issues/3#issuecomment-497524041
  try {
    const node = html.convert(newline_escaped, { ex: EX_CONSTANT });
    // Not sure if this call does anything:
    // https://github.com/mathjax/MathJax-src/blob/master/ts/adaptors/liteAdaptor.ts#L523
    adaptor.setStyle(node, "font-size", fontSize);
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
 */
const tex2svg = async (
  properties: Properties<ad.Num>
): Promise<Result<Output, string>> =>
  new Promise((resolve) => {
    const contents = getAdValueAsString(properties.string, "");
    const fontSize = getAdValueAsString(properties.fontSize, "");

    // Raise error if string or fontSize are empty or optimized
    if (fontSize === "" || contents === "") {
      resolve(
        err(
          `Label 'string' and 'fontSize' must be non-empty and non-optimized for ${properties.name.contents}`
        )
      );
      return;
    }

    // Render the label
    const output = convert(contents, fontSize);
    if (output.isErr()) {
      resolve(err(`MathJax could not render $${contents}$: ${output.error}`));
      return;
    }
    const body = output.value;
    const viewBox = body.getAttribute("viewBox");
    if (viewBox === null) {
      resolve(err(`No ViewBox found for MathJax output $${contents}$`));
      return;
    }

    // Get the rendered viewBox dimensions
    const viewBoxArr = viewBox.split(" ");
    const width = parseFloat(viewBoxArr[2]);
    const height = parseFloat(viewBoxArr[3]);

    // Get re-scaled dimensions of label according to
    // https://github.com/mathjax/MathJax-src/blob/32213009962a887e262d9930adcfb468da4967ce/ts/output/svg.ts#L248
    const vAlignFloat = parseFloat(body.style.verticalAlign) * EX_CONSTANT;
    const constHeight = parseFloat(fontSize) - vAlignFloat;
    const scaledWidth = (constHeight / height) * width;

    resolve(ok({ body, width: scaledWidth, height: constHeight }));
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

/**
 *
 * @param text the content of the text
 * @param font the CSS font rule for the text
 *
 * NOTE: the `font` CSS rule -> https://developer.mozilla.org/en-US/docs/Web/CSS/font
 * @returns `TextMeasurement` object and includes data such as `width` and `height` of the text.
 */
export function measureText(text: string, font: string): TextMeasurement {
  const measureTextElement = document.createElement("canvas");
  const measureTextContext = measureTextElement.getContext("2d")!;
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
      meta[x.key].init.tag === "Pending"
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

import { browserAdaptor } from "mathjax-full/js/adaptors/browserAdaptor.js";
import { RegisterHTMLHandler } from "mathjax-full/js/handlers/html.js";
import { TeX } from "mathjax-full/js/input/tex.js";
import { AllPackages } from "mathjax-full/js/input/tex/AllPackages.js";
import { mathjax } from "mathjax-full/js/mathjax.js";
import { SVG } from "mathjax-full/js/output/svg.js";
import { Equation } from "../shapes/Equation.js";
import { InputMeta } from "../shapes/Samplers.js";
import { Shape } from "../shapes/Shapes.js";
import { Text } from "../shapes/Text.js";
import * as ad from "../types/ad.js";
import { PenroseError } from "../types/errors.js";
import { EquationData, LabelCache, State, TextData } from "../types/state.js";
import { FloatV } from "../types/value.js";
import { Result, err, ok } from "./Error.js";
import { getAdValueAsString, getValueAsShapeList, safe } from "./Util.js";

export const mathjaxInit = (): ((
  input: string,
) => Result<HTMLElement, string>) => {
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

  const convert = (input: string): Result<HTMLElement, string> => {
    // HACK: workaround for newlines. This workaround will force MathJax to always return the same heights regardless of the text content.
    // https://github.com/mathjax/MathJax/issues/2312#issuecomment-538185951
    // if(input) {
    //   const newline_escaped = `\\displaylines{${input}}`;
    // }
    try {
      const node = html.convert(input, {});
      return ok(node.firstChild);
    } catch (error: any) {
      return err(error.message);
    }
  };
  return convert;
};

type Output = {
  body: HTMLElement;
  width: number;
  height: number;
  descent: number;
  ascent: number;
};

const parseFontSize = (
  fontSize: string,
): { number: number; unit: string } | undefined => {
  const regex = /^(\d+(?:\.\d+)?)\s*(px|in|cm|mm)$/;
  const match = fontSize.match(regex);

  if (!match) {
    return;
  }

  const number = parseFloat(match[1]);
  const unit = match[2];

  return { number, unit };
};

// Convert from a font size in absolute unit (px, in, cm, mm) to pixels
const toPxFontSize = (number: number, unit: string): number => {
  const inPX: { [unit: string]: number } = {
    px: 1,
    in: 96, // 96 px to an inch
    cm: 96 / 2.54, // 2.54 cm to an inch
    mm: 96 / 25.4, // 10 mm to a cm
  };
  return inPX[unit] * number;
};

/**
 * Call MathJax to render __non-empty__ labels.
 */
const tex2svg = async (
  properties: Equation<ad.Num>,
  convert: (input: string) => Result<HTMLElement, string>,
): Promise<Result<Output, string>> =>
  new Promise((resolve) => {
    const contents = getAdValueAsString(properties.string, "");
    const fontSize = getAdValueAsString(properties.fontSize, "");

    // Raise error if string or fontSize are empty or optimized
    if (fontSize === "" || contents === "") {
      resolve(
        err(
          `Label 'string' and 'fontSize' must be non-empty and non-optimized for ${properties.name.contents}`,
        ),
      );
    }

    // Render the label
    const output = convert(contents);
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

    // Get re-scaled dimensions of label according to
    // https://github.com/mathjax/MathJax-src/blob/32213009962a887e262d9930adcfb468da4967ce/ts/output/svg.ts#L248
    // all viewbox units are divided by 1000 because MathJax scaled them by 1000
    // these viewbox props are in em units * 1000
    const viewBoxArr = viewBox.split(" ");
    const width = parseFloat(viewBoxArr[2]) / 1000;
    const height = parseFloat(viewBoxArr[3]) / 1000;

    // the vertical align adjustment and height in ex unit. This is used to avoid dealing with ex to px conversion
    const d = -parseFloat(body.style.verticalAlign);
    const exH = parseFloat(body.getAttribute("height")!);

    // em is really the pixel value of the font size
    const parsedFontSize = parseFontSize(fontSize);
    if (parsedFontSize) {
      const { number, unit } = parsedFontSize;
      const em_to_px = (n: number) => n * toPxFontSize(number, unit);
      const scaledWidth = em_to_px(width);
      const scaledHeight = em_to_px(height);
      const scaledD = (d / exH) * scaledHeight;
      const scaledDescent = scaledD;
      const scaledAscent = scaledHeight - scaledDescent; // HACK: interpreting ascent to be height - descent, which might be very wrong

      resolve(
        ok({
          body,
          width: scaledWidth,
          height: scaledHeight,
          descent: scaledDescent,
          ascent: scaledAscent,
        }),
      );
    } else {
      resolve(
        err(
          'Invalid font size format. Only "px", "in", "cm", and "mm" units are supported.',
        ),
      );
      return;
    }
  });

const floatV = (contents: number): FloatV<number> => ({
  tag: "FloatV",
  contents,
});

const textData = (
  width: number,
  height: number,
  descent: number,
  ascent: number,
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
  ascent: number,
  descent: number,
  rendered: HTMLElement,
): EquationData => ({
  tag: "EquationData",
  width: floatV(width),
  height: floatV(height),
  ascent: floatV(ascent),
  descent: floatV(descent),
  rendered,
});

/**
 * Get the CSS string for the font setting of a `Text` shape.
 * @param shape A text shape
 *
 * NOTE: the `font` CSS rule -> https://developer.mozilla.org/en-US/docs/Web/CSS/font
 *
 * @returns a CSS rule string of its font settings
 */
export const toFontRule = <T>(properties: Text<T>): string => {
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
  allShapes: Shape<ad.Num>[],
  convert: (input: string) => Result<HTMLElement, string>,
): Promise<Result<LabelCache, PenroseError>> => {
  const labels: LabelCache = new Map();
  for (const s of allShapes) {
    if (s.shapeType === "Equation") {
      const shapeName = getAdValueAsString(s.name);
      const svg = await tex2svg(s, convert);

      if (svg.isErr()) {
        return err({
          errorType: "SubstanceError",
          tag: "Fatal",
          message: svg.error,
        });
      }

      const { body, width, height, ascent, descent } = svg.value;

      // Instead of directly overwriting the properties, cache them temporarily
      // NOTE: in the case of empty strings, `tex2svg` returns infinity sometimes. Convert to 0 to avoid NaNs in such cases.
      const label: EquationData = equationData(
        width === Infinity ? 0 : width,
        height === Infinity ? 0 : height,
        ascent,
        descent,
        body,
      );
      labels.set(shapeName, label);
    } else if (s.shapeType === "Text") {
      const shapeName: string = getAdValueAsString(s.name);
      let label: TextData;
      // Use canvas to measure text data
      const measure: TextMeasurement = measureText(
        getAdValueAsString(s.string),
        toFontRule(s),
      );

      // If the width and height are defined, the renderer will render the text. `actualDescent` is currently not used in rendering.
      if (measure.width && measure.height) {
        label = textData(
          measure.width,
          measure.height,
          measure.actualDescent,
          measure.actualAscent,
        );
      } else {
        label = textData(0, 0, 0, 0);
      }
      labels.set(shapeName, label);
    } else if (s.shapeType === "Group") {
      const subShapes = getValueAsShapeList(s.shapes);
      const subLabels = await collectLabels(subShapes, convert);
      if (subLabels.isErr()) {
        return subLabels;
      }
      for (const [key, value] of subLabels.value.entries()) {
        labels.set(key, value);
      }
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
  measureTextElement.remove();
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

type InputMap = Map<
  ad.Var,
  {
    index: number;
    meta: InputMeta;
  }
>;

const setPendingProperty = (
  xs: number[],
  inputs: InputMap,
  before: FloatV<ad.Num>,
  after: FloatV<number>,
) => {
  if (typeof before.contents !== "number" && before.contents.tag === "Var") {
    const { index, meta } = safe(inputs.get(before.contents), "missing input");
    if (meta.init.tag === "Pending") xs[index] = after.contents;
  }
};

const insertPendingHelper = (
  shapes: Shape<ad.Num>[],
  xs: number[],
  labelCache: LabelCache,
  inputs: InputMap,
): void => {
  for (const s of shapes) {
    if (s.shapeType === "Group") {
      const subShapes = getValueAsShapeList(s.shapes);
      insertPendingHelper(subShapes, xs, labelCache, inputs);
    } else if (s.shapeType === "Equation") {
      const labelData = safe(labelCache.get(s.name.contents), "missing label");
      if (labelData.tag !== "EquationData")
        throw Error(
          `for ${s.shapeType} ${s.name.contents} got unexpected ${labelData.tag}`,
        );
      setPendingProperty(xs, inputs, s.width, labelData.width);
      setPendingProperty(xs, inputs, s.height, labelData.height);
      setPendingProperty(xs, inputs, s.ascent, labelData.ascent);
      setPendingProperty(xs, inputs, s.descent, labelData.descent);
    } else if (s.shapeType === "Text") {
      const labelData = safe(labelCache.get(s.name.contents), "missing label");
      if (labelData.tag !== "TextData")
        throw Error(
          `for ${s.shapeType} ${s.name.contents} got unexpected ${labelData.tag}`,
        );
      setPendingProperty(xs, inputs, s.width, labelData.width);
      setPendingProperty(xs, inputs, s.height, labelData.height);
      setPendingProperty(xs, inputs, s.ascent, labelData.ascent);
      setPendingProperty(xs, inputs, s.descent, labelData.descent);
    }
  }
};

export const insertPending = (state: State): State => {
  const varyingValues = [...state.varyingValues];
  const inputs = new Map(
    state.inputs.map(({ handle, meta }, index) => [handle, { index, meta }]),
  );
  insertPendingHelper(state.shapes, varyingValues, state.labelCache, inputs);
  return { ...state, varyingValues };
};

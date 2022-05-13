import { browserAdaptor } from "mathjax-full/js/adaptors/browserAdaptor.js";
import { RegisterHTMLHandler } from "mathjax-full/js/handlers/html.js";
import { TeX } from "mathjax-full/js/input/tex.js";
import { AllPackages } from "mathjax-full/js/input/tex/AllPackages.js";
import { mathjax } from "mathjax-full/js/mathjax.js";
import { SVG } from "mathjax-full/js/output/svg.js";
import { PenroseError } from "types/errors";
import { Shape } from "types/shape";
import { EquationData, LabelCache, LabelData, TextData } from "types/state";
import { IFloatV } from "types/value";
import { err, ok, Result } from "./Error";

// https://github.com/mathjax/MathJax-demos-node/blob/master/direct/tex2svg
// const adaptor = chooseAdaptor();
const adaptor = browserAdaptor();
RegisterHTMLHandler(adaptor as any);
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

const convert = (
  input: string,
  fontSize: string
): Result<HTMLElement, string> => {
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
    return ok(node.firstChild as HTMLElement);
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
  contents: string,
  name: string,
  fontSize: string
): Promise<Result<Output, string>> =>
  new Promise((resolve) => {
    const output = convert(contents, fontSize);
    if (output.isErr()) {
      resolve(err(`MathJax could not render $${contents}$: ${output.error}`));
      return;
    }
    const body = output.value;
    // console.log(output);
    // console.log(output.viewBox.baseVal);
    const viewBox = body.getAttribute("viewBox")!.split(" ");
    const width = parseFloat(viewBox[2]);
    const height = parseFloat(viewBox[3]);

    // rescaling according to
    // https://github.com/mathjax/MathJax-src/blob/32213009962a887e262d9930adcfb468da4967ce/ts/output/svg.ts#L248
    const vAlignFloat = parseFloat(body.style.verticalAlign) * EX_CONSTANT;
    const constHeight = parseFloat(fontSize) - vAlignFloat;
    const scaledWidth = (constHeight / height) * width;
    resolve(ok({ body, width: scaledWidth, height: constHeight }));
  });

export const retrieveLabel = (
  shapeName: string,
  labels: LabelCache
): LabelData | undefined => {
  if (labels) {
    const res = labels.find(([name]) => name === shapeName);
    if (res) {
      return res[1];
    } else {
      return undefined;
    }
  } else return undefined;
};

const floatV = (contents: number): IFloatV<number> => ({
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
export const toFontRule = ({ properties }: Shape): string => {
  const fontFamily = properties.fontFamily.contents as string;
  const fontSize = properties.fontSize.contents as string;
  const fontStretch = properties.fontStretch.contents as string;
  const fontStyle = properties.fontStyle.contents as string;
  const fontVariant = properties.fontVariant.contents as string;
  const fontWeight = properties.fontWeight.contents as string;
  const lineHeight = properties.lineHeight.contents as string;
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
  allShapes: Shape[]
): Promise<Result<LabelCache, PenroseError>> => {
  const labels: LabelCache = [];
  for (const s of allShapes) {
    const { shapeType, properties } = s;
    if (shapeType === "Equation" || shapeType === "EquationTransform") {
      const shapeName: string = properties.name.contents as string;
      // HACK: getting type errors for not being able to resolve the Value type
      const svg = await tex2svg(
        properties.string.contents as string,
        shapeName,
        properties.fontSize.contents as string
      );

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
      labels.push([shapeName, label]);
    } else if (shapeType === "Text") {
      const shapeName: string = properties.name.contents as string;
      let label: TextData;
      // Use canvas to measure text data
      const measure: TextMeasurement = measureText(
        properties.string.contents as string,
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
      labels.push([shapeName, label]);
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
  measureText.context.textBaseline = "alphabetic";
  measureText.context.font = font;
  const measurements = measureText.context.measureText(text);
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
// static variable
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace measureText {
  export const element = document.createElement("canvas");
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  export const context = element.getContext("2d")!;
}

//#endregion

import memoize from "fast-memoize";
import { Shape } from "types/shape";
import { mathjax } from "mathjax-full/js/mathjax.js";
import { TeX } from "mathjax-full/js/input/tex.js";
import { SVG } from "mathjax-full/js/output/svg.js";

// Auto-switch between browser and native (Lite) --
// not sure about the latter's fallback behavior
import { chooseAdaptor } from "mathjax-full/js/adaptors/chooseAdaptor.js";
import { browserAdaptor } from "mathjax-full/js/adaptors/browserAdaptor.js";
import { RegisterHTMLHandler } from "mathjax-full/js/handlers/html.js";
import { AllPackages } from "mathjax-full/js/input/tex/AllPackages.js";
import { LabelCache, LabelData } from "types/state";
import { err, ok, Result } from "./Error";
import { PenroseError } from "types/errors";

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
 * NOTE: this function is memoized.
 */
const tex2svg = memoize(
  async (
    contents: string,
    name: string,
    fontSize: string
  ): Promise<Result<Output, string>> =>
    new Promise((resolve) => {
      const output = convert(contents, fontSize);
      if (output.isErr()) {
        resolve(
          err(`MathJax could not render \$${contents}\$: ${output.error}`)
        );
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
    })
);

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

// https://stackoverflow.com/a/44564236
export const collectLabels = async (
  allShapes: Shape[]
): Promise<Result<LabelCache, PenroseError>> => {
  const labels: LabelCache = [];
  for (const s of allShapes) {
    const { shapeType, properties } = s;
    if (shapeType === "Text" || shapeType === "TextTransform") {
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
      const label: LabelData = {
        w: {
          tag: "FloatV",
          contents: width === Infinity ? 0 : width,
        },
        h: {
          tag: "FloatV",
          contents: height === Infinity ? 0 : height,
        },
        rendered: body,
      };
      labels.push([shapeName, label]);
    }
  }
  return ok(labels);
};

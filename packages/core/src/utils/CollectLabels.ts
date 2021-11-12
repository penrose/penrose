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
});
const svg = new SVG({ fontCache: "none" });
const html = mathjax.document("", { InputJax: tex, OutputJax: svg });

// to re-scale baseline
const EX_CONSTANT = 10;

const convert = (input: string, fontSize: string) => {
  // HACK: workaround for newlines
  // https://github.com/mathjax/MathJax/issues/2312#issuecomment-538185951
  const newline_escaped = `\\displaylines{${input}}`;
  // https://github.com/mathjax/MathJax-src/blob/master/ts/core/MathDocument.ts#L689
  const node = html.convert(newline_escaped, { ex: EX_CONSTANT });
  // Not sure if this call does anything:
  // https://github.com/mathjax/MathJax-src/blob/master/ts/adaptors/liteAdaptor.ts#L523
  adaptor.setStyle(node, "font-size", fontSize);
  return node.firstChild as SVGSVGElement;
};

/**
 * Call MathJax to render __non-empty__ labels.
 * NOTE: this function is memoized.
 */
const tex2svg = memoize(
  async (contents: string, name: string, fontSize: string): Promise<any> =>
    new Promise((resolve) => {
      const output = convert(contents, fontSize);
      if (!output) {
        console.error(`MathJax could not render ${contents}`);
        resolve({ output: undefined, width: 0, height: 0 });
        return;
      }
      // console.log(output);
      // console.log(output.viewBox.baseVal);
      const viewBox = output.getAttribute("viewBox")!.split(" ");
      const width = parseFloat(viewBox[2]);
      const height = parseFloat(viewBox[3]);

      // rescaling according to
      // https://github.com/mathjax/MathJax-src/blob/32213009962a887e262d9930adcfb468da4967ce/ts/output/svg.ts#L248
      const vAlignFloat = parseFloat(output.style.verticalAlign) * EX_CONSTANT;
      const constHeight = parseFloat(fontSize) - vAlignFloat;
      const scaledWidth = (constHeight / height) * width;
      resolve({ body: output, width: scaledWidth, height: constHeight });
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
): Promise<LabelCache> => {
  const labels: LabelCache = [];
  for (const s of allShapes) {
    const { shapeType, properties } = s;
    if (shapeType === "Text" || shapeType === "TextTransform") {
      const shapeName: string = properties.name.contents as string;
      // HACK: getting type errors for not being able to resolve the Value type
      const { body, width, height } = await tex2svg(
        properties.string.contents as string,
        shapeName,
        properties.fontSize.contents as string
      );

      // Instead of directly overwriting the properties, cache them temporarily
      // NOTE: in the case of empty strings, `tex2svg` returns infinity sometimes. Convert to 0 to avoid NaNs in such cases.
      const label: LabelData = {
        w: {
          tag: "FloatV",
          contents: width === Infinity ? 0 : (width as number),
        },
        h: {
          tag: "FloatV",
          contents: height === Infinity ? 0 : (height as number),
        },
        rendered: body as HTMLElement,
      };
      labels.push([shapeName, label]);
    }
  }
  return Promise.all(labels);
};

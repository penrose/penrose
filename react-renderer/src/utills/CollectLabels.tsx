import memoize from "fast-memoize";
const mathjax = require("mathjax-full/js/mathjax.js").mathjax;
const TeX = require("mathjax-full/js/input/tex.js").TeX;
const SVG = require("mathjax-full/js/output/svg.js").SVG;
const liteAdaptor = require("mathjax-full/js/adaptors/liteAdaptor.js")
  .liteAdaptor;
const RegisterHTMLHandler = require("mathjax-full/js/handlers/html.js")
  .RegisterHTMLHandler;
const AllPackages = require("mathjax-full/js/input/tex/AllPackages.js")
  .AllPackages;

// https://github.com/mathjax/MathJax-demos-node/blob/master/direct/tex2svg
const adaptor = liteAdaptor();
RegisterHTMLHandler(adaptor);
const tex = new TeX({ packages: AllPackages });
const svg = new SVG({ fontCache: "none" });
const html = mathjax.document("", { InputJax: tex, OutputJax: svg });

/**
 * Find bounding box of an SVG element.
 * NOTE: this is a wrapper around `getBBox`, which is known to have problems in certain browsers
 *
 * @param svgEl rendered SvG element
 */
export function svgBBox(svgEl: SVGSVGElement) {
  const tempDiv = document.createElement("div");
  tempDiv.setAttribute(
    "style",
    "position:absolute; visibility:hidden; width:0; height:0"
  );
  document.body.appendChild(tempDiv);
  const tempSvg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  const tempG = document.createElementNS("http://www.w3.org/2000/svg", "g");
  tempSvg.appendChild(tempG);
  tempDiv.appendChild(tempSvg);
  const tempEl = svgEl.cloneNode(true) as SVGSVGElement;
  tempG.appendChild(tempEl);
  const bb = tempG.getBBox();
  document.body.removeChild(tempDiv);
  return bb;
}

const convert = (input: string, fontSize: string) => {
  const node = html.convert(input, {});
  // Not sure if this call does anything:
  adaptor.setStyle(node, "font-size", fontSize);
  const inner = adaptor.innerHTML(node);
  const doc = new DOMParser().parseFromString(inner, "text/html").body
    .firstChild as any;
  doc.style.fontSize = fontSize;
  return doc as SVGSVGElement;
};

/**
 * Call MathJax to render __non-empty__ labels.
 * NOTE: this function is memoized.
 */
const tex2svg = memoize(
  async (contents: string, name: string, fontSize: string): Promise<any> =>
    new Promise((resolve) => {
      // HACK: Style compiler decides to give empty labels if not specified
      if (contents !== "") {
        const output = convert(contents, fontSize);
        if (!output) {
          console.error(`MathJax could not render ${contents}`);
          resolve({ output: undefined, width: 0, height: 0 });
          return;
        }
        const { width, height } = svgBBox(output);

        const body = output;
        resolve({ body, width, height });
      } else {
        resolve({ output: undefined, width: 0, height: 0 });
      }
    })
);

// https://stackoverflow.com/a/44564236
export const collectLabels = async (allShapes: Shape[]) => {
  return Promise.all(
    allShapes.map(async ({ shapeType, properties }: Shape) => {
      if (shapeType === "Text" || shapeType === "TextTransform") {
        // HACK: getting type errors for not being able to resolve the Value type
        const { body, width, height } = await tex2svg(
          properties.string.contents as string,
          properties.name.contents as string,
          properties.fontSize.contents as string
        );
        // Instead of directly overwriting the properties, cache them temporarily and let `propogateUpdate` decide what to do
        // TODO: need to give a type to this kind of updated shape
        const obj2: any = { ...properties };
        obj2.w.updated = { tag: "FloatV", contents: width };
        obj2.h.updated = { tag: "FloatV", contents: height };
        // HACK: this behavior needs to be encoded in our type system
        // Add omit: true flag so it doesn't get sent to the server
        obj2.rendered = { contents: body, omit: true };
        return { shapeType, properties: obj2 };
      } else {
        return { shapeType, properties };
      }
    })
  );
};

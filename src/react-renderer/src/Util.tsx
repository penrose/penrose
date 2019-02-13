import * as React from "react";

declare const MathJax: any;
import memoize from "fast-memoize";

export const StartArrowhead = (props: {
  id: string;
  color: string;
  opacity: number;
}) => {
  return (
    <marker
      id={props.id}
      markerUnits="strokeWidth"
      markerWidth="12"
      markerHeight="12"
      viewBox="0 0 12 12"
      refX="6"
      refY="6"
      orient="auto"
    >
      <path
        d="M10,10 A30,30,0,0,0,2,6 A30,30,0,0,0,10,2 L7.5,6 z"
        fill={props.color}
        fillOpacity={props.opacity}
      />
    </marker>
  );
};
export const EndArrowhead = (props: {
  id: string;
  color: string;
  opacity: number;
}) => {
  return (
    <marker
      id={props.id}
      markerUnits="strokeWidth"
      markerWidth="12"
      markerHeight="12"
      viewBox="0 0 12 12"
      refX="6"
      refY="6"
      orient="auto"
    >
      <path
        d="M2,2 A30,30,0,0,0,10,6 A30,30,0,0,0,2,10 L4.5,6 z"
        fill={props.color}
        fillOpacity={props.opacity}
      />
    </marker>
  );
};
export const toScreen = (
  [x, y]: [number, number],
  canvasSize: [number, number]
) => {
  const [width, height] = canvasSize;
  return [width / 2 + x, height / 2 - y];
};
export const toHex = (rgba: [number, number, number, number]) => {
  return rgba.slice(0, 3).reduce((prev, cur) => {
    const hex = Math.round(255 * cur).toString(16);
    const padded = hex.length === 1 ? "0" + hex : hex;
    return prev + padded;
  }, "#");
};

export const getAngle = (x1: number, y1: number, x2: number, y2: number) => {
  const x = x1 - x2;
  const y = y1 - y2;
  if (!x && !y) {
    return 0;
  }
  return (180 + (Math.atan2(-y, -x) * 180) / Math.PI + 360) % 360;
};

export const getLen = (x1: number, y1: number, x2: number, y2: number) => {
  return Math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));
};

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
const tex2svg = memoize(
  async (contents: string, name: string): Promise<any> =>
    new Promise(resolve => {
      const wrapper = document.createElement("div");
      // HACK: Style compiler decides to give empty labels if not specified
      if (contents !== "") {
        wrapper.innerHTML = "$" + contents + "$";
        MathJax.Hub.Queue(["Typeset", MathJax.Hub, wrapper]);
        MathJax.Hub.Queue(() => {
          const output = wrapper.getElementsByTagName("svg")[0];
          output.setAttribute("xmlns", "http://www.w3.org/2000/svg");
          // TODO: need to check whether MathJax returns a non-null response
          // NOTE: This is where you can directly control the width/height of the LaTeX
          const { width, height } = svgBBox(output);
          output.setAttribute("width", width.toString());
          output.setAttribute("height", height.toString());
          const body = output;
          // const body = output.outerHTML + `<title>${name}</title>`; // need to keep properties in <svg>
          resolve({ body, width, height });
        });
      } else {
        resolve({ output: undefined, width: 0, height: 0 });
      }
    })
);
export const collectLabels = async (allShapes: any[]) => {
  MathJax.Hub.Config({
    skipStartupTypeset: true,
    extensions: ["tex2jax.js", "TeX/AMSmath.js"],
    jax: ["input/TeX", "output/SVG"],
    SVG: {
      useGlobalCache: false // Needed for SVG inline export
    },
    tex2jax: {
      inlineMath: [["$", "$"], ["\\(", "\\)"]],
      processEscapes: true
    }
  });
  return Promise.all(
    allShapes.map(async ([type, obj]: [string, any]) => {
      if (type === "Text") {
        const { body, width, height } = await tex2svg(
          obj.string.contents,
          obj.name.contents
        );
        const obj2 = { ...obj };
        obj2.w.contents = width;
        obj2.h.contents = height;
        // Add omit: true flag so it doesn't get sent to the server
        obj2.rendered = {contents: body, omit: true};
        return [type, obj2];
      } else {
        return [type, obj];
      }
    })
  );
};

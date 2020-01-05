import * as React from "react";

declare const MathJax: any;
import memoize from "fast-memoize";

export const arrowheads = {
  "arrowhead-1": {
    width: 8,
    height: 8,
    viewbox: "0 0 8 8",
    refX: "4",
    refY: "4", // HACK: to avoid paths from bleeding through the arrowhead
    path: "M0,0 A30,30,0,0,0,8,4 A30,30,0,0,0,0,8 L2.5,4 z"
  },
  "arrowhead-2": {
    width: 9.95,
    height: 8.12,
    viewbox: "0 0 9.95 8.12",
    refX: "2.36", // HACK: to avoid paths from bleeding through the arrowhead
    refY: "4.06",
    path: "M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z"
  }
};

export const Shadow = (props: { id: string }) => {
  return (
    <filter id={props.id} x="0" y="0" width="200%" height="200%">
      <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5" />
      <feGaussianBlur result="blurOut" in="offOut" stdDeviation="4" />
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal" />
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.5" />
      </feComponentTransfer>
      <feMerge>
        <feMergeNode />
        <feMergeNode in="SourceGraphic" />
      </feMerge>
    </filter>
  );
};

export const Arrowhead = (props: {
  id: string;
  color: string;
  opacity: number;
  style: string;
  size: number;
}) => {
  const { size, style } = props;
  const arrow = arrowheads[style];
  return (
    <marker
      id={props.id}
      markerUnits="strokeWidth"
      markerWidth={arrow.width * size}
      markerHeight={arrow.height * size}
      viewBox={arrow.viewbox}
      refX={arrow.refX}
      refY={arrow.refY}
      orient="auto-start-reverse"
    >
      <path d={arrow.path} fill={props.color} fillOpacity={props.opacity} />
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

export const penroseToSVG = (canvasSize: [number, number]) => {
  const [width, height] = canvasSize;
  const flipYStr = "matrix(1 0 0 -1 0 0)";
  const translateStr = "translate(" + width / 2 + ", " + height / 2 + ")";
  // Flip Y direction, then translate shape to origin mid-canvas
  return [translateStr, flipYStr].join(" ");
};

export const penroseTransformStr = (tf: any) => {
  // console.log("shape transformation", tf);
  const transformList = [
    tf.xScale,
    tf.ySkew,
    tf.xSkew,
    tf.yScale,
    tf.dx,
    tf.dy
  ];
  const penroseTransform = "matrix(" + transformList.join(" ") + ")";
  return penroseTransform;
};

export const svgTransformString = (tf: any, canvasSize: [number, number]) => {
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
  // `tf` is `shape.transformation.contents`, an HMatrix from the backend
  // It is the *full* transform, incl. default transform
  // Do Penrose transform, then SVG
  const transformStr = [penroseToSVG(canvasSize), penroseTransformStr(tf)].join(
    " "
  );
  // console.log("transformStr", transformStr);
  return transformStr;
};

export const toPointListString = memoize(
  // Why memoize?
  (ptList: any[], canvasSize: [number, number]) =>
    ptList
      .map((coords: [number, number]) => {
        const pt = coords;
        return pt[0].toString() + " " + pt[1].toString();
      })
      .join(" ")
);

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
  async (contents: string, name: string, fontSize: string): Promise<any> =>
    new Promise(resolve => {
      const wrapper = document.createElement("div");
      // HACK: Style compiler decides to give empty labels if not specified
      if (contents !== "") {
        wrapper.innerHTML = "$" + contents + "$";
        MathJax.Hub.Queue(["Typeset", MathJax.Hub, wrapper]);
        MathJax.Hub.Queue(() => {
          const output = wrapper.getElementsByTagName("svg")[0];
          output.setAttribute("xmlns", "http://www.w3.org/2000/svg");
          output.setAttribute("style", `font-size: ${fontSize}`);
          // TODO: need to check whether MathJax returns a non-null response
          // NOTE: This is where you can directly control the width/height of the LaTeX
          const { width, height } = svgBBox(output);

          // TODO: These seem to be extraneous. Remove?
          // output.setAttribute("width", width.toString());
          // output.setAttribute("height", height.toString());
          const body = output;
          resolve({ body, width, height });
        });
      } else {
        resolve({ output: undefined, width: 0, height: 0 });
      }
      wrapper.remove();
    })
);

export const collectLabels = async (allShapes: any[]) => {
  MathJax.Hub.Config({
    skipStartupTypeset: true,
    extensions: ["tex2jax.js", "TeX/AMSmath.js"],
    jax: ["input/TeX", "output/SVG"],
    // https://docs.mathjax.org/en/v2.7-latest/options/output-processors/SVG.html
    SVG: {
      // font: "Gyre-Pagella", // TODO: This doesn't seem to work
      matchFontHeight: false,
      useGlobalCache: false, // Needed for SVG inline export
      useFontCache: false // further reduces the reuse of paths
    },
    tex2jax: {
      inlineMath: [["$", "$"], ["\\(", "\\)"]],
      processEscapes: true
    }
  });
  return Promise.all(
    allShapes.map(async ([type, obj]: [string, any]) => {
      if (type === "Text" || type === "TextTransform") {
        const { body, width, height } = await tex2svg(
          obj.string.contents,
          obj.name.contents,
          obj.fontSize.contents
        );
        // Instead of directly overwriting the properties, cache them temporarily and let `propogateUpdate` decide what to do
        const obj2 = { ...obj };
        obj2.w.updated = width;
        obj2.h.updated = height;
        // console.log(body, obj2);
        // Add omit: true flag so it doesn't get sent to the server
        obj2.rendered = { contents: body, omit: true };
        return [type, obj2];
      } else {
        return [type, obj];
      }
    })
  );
};

export const loadImageElement = memoize(
  async (url: string): Promise<any> =>
    new Promise((resolve, reject) => {
      const img = new Image();
      img.onload = () => resolve(img);
      img.onerror = reject;
      img.src = url;
    })
);

// Load images asynchronously so we can send the dimensions to the backend and use it in the frontend

export const loadImages = async (allShapes: any[]) => {
  return Promise.all(
    allShapes.map(async ([type, obj]: [string, any]) => {
      if (type === "ImageTransform") {
        const path = obj.path.contents;
        const fullPath = process.env.PUBLIC_URL + path;
        const loadedImage = await loadImageElement(fullPath);
        const obj2 = { ...obj };

        obj2.initWidth.contents = loadedImage.naturalWidth;
        obj2.initHeight.contents = loadedImage.naturalHeight;
        // We discard the loaded image <img> in favor of making an <image> inline in the image GPI file
        // obj2.rendered = { contents: loadedImage, omit: true };

        return [type, obj2];
      } else {
        return [type, obj];
      }
    })
  );
};

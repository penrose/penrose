import Canvas from "../react-renderer/src/StaticCanvas";
import * as React from "react";

const fs = require("fs");
const mathjax = require("mathjax-node");
const { propagateUpdate } = require("../react-renderer/src/PropagateUpdate");
// const Canvas = require("../react-renderer/src/Canvas");
const ReactDOMServer = require("react-dom/server");

const args = process.argv.slice(2);

const st = fs.readFileSync("./state.json");

const state = JSON.parse(st).contents[0];

const allShapes = state.shapesr;

(async () => {
  const collected = await Promise.all(
    allShapes.map(async ([type, obj]) => {
      if (type === "Text" || type === "TextTransform") {
        const data = await mathjax.typeset({
          math: obj.string.contents,
          format: "TeX",
          svg: true,
          svgNode: true,
          useFontCache: false,
          useGlobalCache: false,
          ex: 12
        });
        if (data.errors) {
          console.error(
            `Could not render ${obj.string.contents}: `,
            data.errors
          );
          return;
        }
        const { width, height } = data;
        const textGPI = { ...obj };
        const SCALE_FACTOR = 7;

        // Take substring to omit `ex`
        textGPI.w.updated =
          +width.substring(0, width.length - 2) * SCALE_FACTOR;
        textGPI.h.updated =
          +height.substring(0, height.length - 2) * SCALE_FACTOR;

        data.svgNode.setAttribute("width", textGPI.w.updated);
        data.svgNode.setAttribute("height", textGPI.h.updated);

        // TODO: can't store this in the state, return it separately (3 tuple?)
        textGPI.rendered = data.svgNode.outerHTML;
      }
      return [type, obj];
    })
  );

  // update the state with newly generated labels and label dimensions
  // const updated = await propagateUpdate({ ...state, shapesr: collected });

  const canvas = ReactDOMServer.renderToString(<Canvas state={state}></Canvas>);
  console.log(canvas);
})();

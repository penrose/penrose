import * as React from "react";

const fs = require("fs");
const mathjax = require("mathjax-node");
const { propagateUpdate } = require("../react-renderer/src/PropagateUpdate");
const Canvas = require("../react-renderer/src/Canvas");
const Packets = require("../react-renderer/src/packets");
const { loadImages } = require("../react-renderer/src/Util");
const ReactDOMServer = require("react-dom/server");
const { spawn } = require("child_process");

const args = process.argv.slice(2);

const fetched = args.map(arg =>
  fs.readFileSync(`../examples/${arg}`, "utf8").toString()
);

const compilePacket = JSON.stringify(Packets.CompileTrio(...fetched));

const penrose = spawn("penrose", ["runAPI"]);
penrose.stdin.setEncoding("utf-8");
penrose.stdin.write(compilePacket + "\n");
let data = "";
penrose.stdout.on("data", async d => (data += d.toString()));
penrose.stdout.on("close", async cl => {
  console.log(data);
  const state = JSON.parse(data).contents[0];

  const allShapes = state.shapesr;

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
        textGPI.rendered = {
          contents: data.svgNode
        };
        return [type, textGPI];
      }
      return [type, obj];
    })
  );
  // TODO: images (see prepareSVG method in canvas)
  const sortedShapes = await Canvas.default.sortShapes(
    collected,
    state.shapeOrdering
  );
  // update the state with newly generated labels and label dimensions
  const updated = await propagateUpdate({ ...state, shapesr: sortedShapes });

  const canvas = ReactDOMServer.renderToString(
    <Canvas.default data={updated} lock={true} />
  );
  console.log(canvas);
  fs.writeFile("output.svg", canvas, err => {
    if (err) throw err;
    console.log("The file has been saved!");
  });
});

penrose.stdin.end();

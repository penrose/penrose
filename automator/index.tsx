import * as React from "react";

const fs = require("fs");
const mathjax = require("mathjax-node");
const { propagateUpdate } = require("../react-renderer/src/PropagateUpdate");
const Canvas = require("../react-renderer/src/Canvas");
const Packets = require("../react-renderer/src/packets");
const { loadImages } = require("../react-renderer/src/Util");
const ReactDOMServer = require("react-dom/server");
const { spawn } = require("child_process");

const runPenrose = (packet: object) =>
  new Promise((resolve, reject) => {
    const penrose = spawn("penrose", ["runAPI"]);
    penrose.stdin.setEncoding("utf-8");
    penrose.stdin.write(JSON.stringify(packet) + "\n");
    let data = "";
    penrose.stdout.on("data", async d => (data += d.toString()));
    penrose.stdout.on("close", async cl => resolve(data));

    penrose.stdin.end();
  }) as any;

const collectLabels = async (state: any, includeRendered: boolean) => {
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

        if (includeRendered) {
          textGPI.rendered = {
            contents: data.svgNode
          };
        }
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
  return updated;
};

// Process command-line arguments
const args = process.argv.slice(2);

const trio = args.map(arg =>
  fs.readFileSync(`../examples/${arg}`, "utf8").toString()
);

(async () => {
  console.log("Compiling ...");
  const compilePacket = Packets.CompileTrio(...trio);
  const data = await runPenrose(compilePacket);
  const state = JSON.parse(data).contents[0];
  const updated = await collectLabels(state, false);
  console.log("Stepping ...");
  const convergePacket = Packets.StepUntilConvergence(updated);
  const newData = await runPenrose(convergePacket);
  const newState = JSON.parse(newData).contents;
  const newUpdated = await collectLabels(newState, true);

  const canvas = ReactDOMServer.renderToString(
    <Canvas.default data={newUpdated} lock={true} />
  );
  console.log(canvas);
  fs.writeFile("output.svg", canvas, err => {
    if (err) throw err;
    console.log("The file has been saved!");
  });
})();

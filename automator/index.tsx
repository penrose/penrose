import * as React from "react";

const fs = require("fs");
const mathjax = require("mathjax-node");
const { propagateUpdate } = require("../react-renderer/src/PropagateUpdate");
const Canvas = require("../react-renderer/src/Canvas");
const Packets = require("../react-renderer/src/packets");
// const { loadImages } = require("../react-renderer/src/Util"); // TODO: implement image import
const ReactDOMServer = require("react-dom/server");
const { spawn } = require("child_process");
const neodoc = require("neodoc");

const USAGE = `
Usage: automator SUBSTANCE STYLE DOMAIN [--outFile=PATH]
Options:
  -o, --outFile PATH
`;

/**
 * Run the penrose binary using the supplied packet as the input
 * @param packet packet to be processed by the backend
 */
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

/**
 * Collect all the labels in the state by calling MathJax
 * @param state initial state
 * @param includeRendered whether to include the rendered SVG nodes in the output state
 */
const collectLabels = async (state: any, includeRendered: boolean) => {
  if (!state.shapesr) {
    console.error(`Could not find shapesr key in returned state: ${state}`);
    return;
  }
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
        const SCALE_FACTOR = 7; // HACK: empirically determined conversion factor from em to Penrose unit

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
const args = neodoc.run(USAGE, { smartOptions: true });

// Fetch Substance, Style, and Domain files
// TODO: make this optional in the command line args
const trio = [args.SUBSTANCE, args.STYLE, args.DOMAIN].map(arg =>
  fs.readFileSync(`../examples/${arg}`, "utf8").toString()
);

// Determine the output file path
const outFile = args["--outFile"] ? args["--outFile"] : "output.svg";

// In an async context, communicate with the backend to compile and optimize the diagram
(async () => {
  console.log("Compiling ...");
  const compilePacket = Packets.CompileTrio(...trio);
  const compilerOutput = await runPenrose(compilePacket);
  const compiledState = JSON.parse(compilerOutput);
  if (compiledState.type === "error") {
    const err = compiledState.contents;
    console.error(`Compilation failed:\n${err.tag}\n${err.contents}`);
    process.exit(1);
  }
  const initialState = await collectLabels(compiledState.contents[0], false);
  console.log("Stepping ...");
  const convergePacket = Packets.StepUntilConvergence(initialState);
  const optimizerOutput = await runPenrose(convergePacket);
  const optimizedState = JSON.parse(optimizerOutput).contents;
  const state = await collectLabels(optimizedState, true);

  const canvas = ReactDOMServer.renderToString(
    <Canvas.default data={state} lock={true} />
  );
  fs.writeFile(outFile, canvas, err => {
    if (err) throw err;
    console.log(`The diagram has been saved to ${outFile}`);
  });
})();

import * as React from "react";
import {
  Canvas,
  EngineUtils,
  Packets,
  PropagateUpdate,
  Optimizer,
  Evaluator,
} from "@penrose/core";

const fs = require("fs");
const mathjax = require("mathjax-node");
const ReactDOMServer = require("react-dom/server");
const { spawn } = require("child_process");
const chalk = require("chalk");
const neodoc = require("neodoc");
const uniqid = require("uniqid");
const convertHrtime = require("convert-hrtime");

const USAGE = `
Penrose Automator.

Usage:
  automator SUBSTANCE STYLE DOMAIN [--folders] [--outFile=PATH] [--src-prefix=PREFIX]
  automator batch SUBSTANCELIB STYLELIB DOMAINLIB OUTFOLDER [--folders]  [--src-prefix=PREFIX]

Options:
  -o, --outFile PATH Path to either an SVG file or a folder, depending on the value of --folders. [default: output.svg]
  --folders Include metadata about each output diagram. If enabled, outFile has to be a path to a folder.
  "type": "module"
  --src-prefix PREFIX the prefix to SUBSTANCE, STYLE, and DOMAIN, or the library equivalent in batch mode. No trailing "/" required. [default: ../examples]
`;

const nonZeroConstraints = (
  state: any,
  constrVals: [number],
  threshold: number
) => {
  const constrFns = state.constrFns;
  const fnsWithVals = constrFns.map((f: any, i: string | number) => [
    f,
    constrVals[i],
  ]);
  const nonzeroConstr = fnsWithVals.filter(
    (c: (string | number)[]) => +c[1] > threshold
  );
  return nonzeroConstr;
};

/**
 * Run the penrose binary using the supplied packet as the input
 * @param packet packet to be processed by the backend
 */
const runPenrose = (packet: object) =>
  new Promise((resolve, reject) => {
    const penrose = spawn("penrose", ["runAPI"]);
    // penrose.stdin.setEncoding("utf-8");
    penrose.stdin.write(JSON.stringify(packet) + "\n");
    let data = "";
    penrose.stdout.on("data", async (d: { toString: () => string }) => {
      data += d.toString();
    });
    penrose.stdout.on("close", async (cl: any) => {
      resolve(data);
    });

    penrose.stdin.end();
  }) as any;

/**
 * Collect all the labels in the state by calling MathJax
 * @param state initial state
 * @param includeRendered whether to include the rendered SVG nodes in the output state
 */
const collectLabels = async (state: any, includeRendered: boolean) => {
  if (!state.shapes) {
    console.error(`Could not find shapes key in returned state: ${state}`);
    return;
  }
  const allShapes = state.shapes;

  const collected = await Promise.all(
    allShapes.map(async ({ shapeType, properties }: any) => {
      if (shapeType === "Text" || shapeType === "TextTransform") {
        const data = await mathjax.typeset({
          math: properties.string.contents,
          format: "TeX",
          svg: true,
          svgNode: true,
          useFontCache: false,
          useGlobalCache: false,
          ex: 12,
        });
        if (data.errors) {
          console.error(
            `Could not render ${properties.string.contents}: `,
            data.errors
          );
          return;
        }
        const { width, height } = data;
        const textGPI = { ...properties };
        const SCALE_FACTOR = 7; // HACK: empirically determined conversion factor from em to Penrose unit
        const computedWidth =
          +width.substring(0, width.length - 2) * SCALE_FACTOR;
        const computedHeight =
          +height.substring(0, height.length - 2) * SCALE_FACTOR;

        // Take substring to omit `ex`
        textGPI.w.updated = { tag: "FloatV", contents: computedWidth };
        textGPI.h.updated = { tag: "FloatV", contents: computedHeight };

        data.svgNode.setAttribute(
          "style",
          `font-size: ${properties.fontSize.contents}`
        );
        // TODO: this is not setting the width and height correctly, why?
        data.svgNode.setAttribute("width", computedWidth.toString());
        data.svgNode.setAttribute("height", computedHeight.toString());

        if (includeRendered) {
          textGPI.rendered = {
            contents: data.svgNode,
          };
        }
        return { shapeType, properties: textGPI };
      }
      return { shapeType, properties };
    })
  );
  // TODO: images (see prepareSVG method in canvas)
  const sortedShapes = await Canvas.sortShapes(collected, state.shapeOrdering);
  const nonEmpties = await sortedShapes.filter(Canvas.notEmptyLabel);
  // update the state with newly generated labels and label dimensions
  const updated = await PropagateUpdate.insertPending({
    ...state,
    shapes: nonEmpties,
  });
  return updated;
};

const toMs = (hr: any) => hr[1] / 1000000;

// In an async context, communicate with the backend to compile and optimize the diagram
const singleProcess = async (
  sub: any,
  sty: any,
  dsl: string,
  folders: boolean,
  out: string,
  prefix: string,
  meta = {
    substanceName: sub,
    styleName: sty,
    domainName: dsl,
    id: uniqid("instance-"),
  }
) => {
  // Fetch Substance, Style, and Domain files
  const trio = [sub, sty, dsl].map((arg) =>
    fs.readFileSync(`${prefix}/${arg}`, "utf8").toString()
  );

  // Compilation
  console.log(`Compiling for ${out}/${sub} ...`);
  const overallStart = process.hrtime();
  const compilePacket = Packets.CompileTrio(...trio);
  const compileStart = process.hrtime();
  const compilerOutput = await runPenrose(compilePacket);
  const compileEnd = process.hrtime(compileStart);
  let compiledState;
  try {
    compiledState = JSON.parse(compilerOutput);
  } catch (e) {
    console.error(`Cannot parse compiler output "${compilerOutput}": ${e}`);
    process.exit(1);
  }
  if (compiledState.type === "error") {
    const err = compiledState.contents;
    console.error(`Compilation failed:\n${err.tag}\n${err.contents}`);
    process.exit(1);
  }

  // State preparation: decode, autodiff types, and evaluation
  const decodedState = Evaluator.decodeState(compiledState.contents[0]);
  const stateAD = {
    ...decodedState,
    translation: EngineUtils.makeTranslationDifferentiable(
      decodedState.translation
    ),
  };
  const stateEvaled = Evaluator.evalShapes(stateAD);

  // Labeling and resolving pending vars
  const labelStart = process.hrtime();
  const initialState = await collectLabels(stateEvaled, false);
  const labelEnd = process.hrtime(labelStart);

  console.log(`Stepping for ${out} ...`);

  const convergeStart = process.hrtime();
  const optimizedState = await Optimizer.stepUntilConvergence(initialState);
  const convergeEnd = process.hrtime(convergeStart);

  // We don't time this individually since it's usually memoized anyway
  const labeledState: any = await collectLabels(optimizedState, true);

  // TODO: include metadata prop?
  const reactRenderStart = process.hrtime();

  const canvas = ReactDOMServer.renderToString(
    <Canvas data={labeledState} lock={true} />
  );
  const reactRenderEnd = process.hrtime(reactRenderStart);
  const overallEnd = process.hrtime(overallStart);
  if (folders) {
    // Check for non-zero constraints
    const energies = JSON.parse(
      await runPenrose(Packets.EnergyValues(optimizedState))
    );
    const constrs = nonZeroConstraints(optimizedState, energies.contents[1], 1);
    if (constrs.length > 0) {
      console.log("This instance has non-zero constraints: ");
      // return;
    }

    const metadata = {
      ...meta,
      renderedOn: Date.now(),
      timeTaken: {
        // includes overhead like JSON, recollecting labels
        overall: convertHrtime(overallEnd).milliseconds,
        compilation: convertHrtime(compileEnd).milliseconds,
        labelling: convertHrtime(labelEnd).milliseconds,
        optimization: convertHrtime(convergeEnd).milliseconds,
        rendering: convertHrtime(reactRenderEnd).milliseconds,
      },
      violatingConstraints: constrs,
      nonzeroConstraints: constrs.length > 0,
      selectorMatches: optimizedState.selectorMatches,
      optProblem: {
        constraintCount: optimizedState.constrFns.length,
        objectiveCount: optimizedState.objFns.length,
      },
    };
    if (!fs.existsSync(out)) {
      fs.mkdirSync(out);
    }
    fs.writeFileSync(`${out}/output.svg`, canvas);
    fs.writeFileSync(`${out}/substance.sub`, trio[0]);
    fs.writeFileSync(`${out}/style.sty`, trio[1]);
    fs.writeFileSync(`${out}/domain.dsl`, trio[2]);
    fs.writeFileSync(`${out}/meta.json`, JSON.stringify(metadata));
    console.log(`The diagram and metadata has been saved to ${out}`);
    // returning metadata for aggregation
    return metadata;
  } else {
    fs.writeFileSync(out, canvas);
    console.log(`The diagram has been saved as ${out}`);
    // HACK: return empty metadata??
    return null;
  }
};

// Takes a trio of registries/libraries and runs `singleProcess` on each substance program.
const batchProcess = async (
  sublib: any,
  stylib: any,
  dsllib: string,
  folders: boolean,
  out: string,
  prefix: string
) => {
  const substanceLibrary = JSON.parse(
    fs.readFileSync(`${prefix}/${sublib}`).toString()
  );
  const styleLibrary = JSON.parse(
    fs.readFileSync(`${prefix}/${stylib}`).toString()
  );
  const domainLibrary = JSON.parse(
    fs.readFileSync(`${prefix}/${dsllib}`).toString()
  );
  console.log(`Processing ${substanceLibrary.length} substance files...`);

  const finalMetadata = {};
  // NOTE: for parallelism, use forEach.
  // But beware the console gets messy and it's hard to track what failed
  for (const { name, substanceURI, element, style } of substanceLibrary) {
    // TODO: find JSON by value
    if (styleLibrary[style].plugin) {
      console.log(
        chalk.red(
          `Skipping "${name}" (${substanceURI}) for now; this domain requires a plugin or has known issues.`
        )
      );
      continue;
    }
    const foundStyle = styleLibrary.find(({ value }: any) => value === style);
    const foundDomain = domainLibrary.find(
      ({ value }: any) => value === element
    );

    const stylePath = foundStyle.uri;
    const domainPath = foundDomain.uri;
    const styleName = foundStyle.label;
    const domainName = foundDomain.label;
    // Warning: will face id conflicts if parallelism used
    const id = uniqid("instance-");

    const meta = await singleProcess(
      substanceURI,
      stylePath,
      domainPath,
      folders,
      `${out}/${name}-${id}${folders ? "" : ".svg"}`,
      prefix,
      {
        substanceName: name,
        styleName,
        domainName,
        id,
      }
    );
    if (folders) {
      finalMetadata[id] = meta;
    }
  }
  if (folders) {
    fs.writeFileSync(
      `${out}/aggregateData.json`,
      JSON.stringify(finalMetadata)
    );
    console.log(`The Aggregate metadata has been saved to ${out}.`);
  }
  console.log("done.");
};

(async () => {
  // Process command-line arguments
  const args = neodoc.run(USAGE, { smartOptions: true });

  // Determine the output file path
  const folders = args["--folders"] || false;
  const outFile = args["--outFile"];
  const prefix = args["--src-prefix"];

  if (args.batch) {
    await batchProcess(
      args.SUBSTANCELIB,
      args.STYLELIB,
      args.DOMAINLIB,
      folders,
      args.OUTFOLDER,
      prefix
    );
  } else {
    await singleProcess(
      args.SUBSTANCE,
      args.STYLE,
      args.DOMAIN,
      folders,
      outFile,
      prefix
    );
  }
})();

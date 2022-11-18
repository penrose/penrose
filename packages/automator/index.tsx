require("global-jsdom/register");
import {
  compileTrio,
  evalEnergy,
  makeCanvas,
  prepareState,
  RenderStatic,
  shapedefs,
  showError,
  simpleContext,
  stepUntilConvergence,
} from "@penrose/core";
import { ShapeDef } from "@penrose/core/build/dist/shapes/Shapes";
import chalk from "chalk";
import convertHrtime from "convert-hrtime";
import { randomBytes } from "crypto";
import * as fs from "fs";
import neodoc from "neodoc";
import fetch from "node-fetch";
import { dirname, join, parse, resolve } from "path";
import * as prettier from "prettier";
import uniqid from "uniqid";
import { printTextChart, renderArtifacts } from "./artifacts";
import { AggregateData, InstanceData } from "./types";

const USAGE = `
Penrose Automator.

Usage:
  automator batch LIB OUTFOLDER [--folders] [--src-prefix=PREFIX] [--repeat=TIMES] [--render=OUTFOLDER] [--cross-energy]
  automator render ARTIFACTSFOLDER OUTFOLDER
  automator textchart ARTIFACTSFOLDER OUTFILE
  automator draw SUBSTANCE STYLE DOMAIN OUTFOLDER [--src-prefix=PREFIX] [--variation=VARIATION] [--folders] [--cross-energy]
  automator shapedefs [SHAPEFILE]

Options:
  -o, --outFile PATH Path to either a file or a folder, depending on the value of --folders. [default: output.svg]
  --folders Include metadata about each output diagram. If enabled, outFile has to be a path to a folder.
  --src-prefix PREFIX the prefix to SUBSTANCE, STYLE, and DOMAIN, or the library equivalent in batch mode. No trailing "/" required. [default: .]
  --repeat TIMES the number of instances
  --cross-energy Compute the cross-instance energy
  --variation The variation to use
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

const toMs = (hr: any) => hr[1] / 1000000;

// In an async context, communicate with the backend to compile and optimize the diagram
const singleProcess = async (
  variation: string,
  sub: string,
  sty: string,
  dsl: string,
  folders: boolean,
  out: string,
  prefix: string,
  meta = {
    substanceName: sub,
    styleName: sty,
    domainName: dsl,
    id: uniqid("instance-"),
  },
  reference?,
  referenceState?,
  extrameta?,
  ciee?
) => {
  // Fetch Substance, Style, and Domain files
  const [subIn, styIn, dslIn] = [sub, sty, dsl].map((arg) =>
    fs.readFileSync(join(prefix, arg), "utf8").toString()
  );

  // Compilation
  console.log(`Compiling for ${out}/${sub} ...`);
  const overallStart = process.hrtime();
  const compileStart = process.hrtime();
  const compilerOutput = compileTrio({
    substance: subIn,
    style: styIn,
    domain: dslIn,
    variation,
  });
  const compileEnd = process.hrtime(compileStart);
  if (compilerOutput.isErr()) {
    const err = compilerOutput.error;
    throw new Error(`Compilation failed:\n${showError(err)}`);
  }
  const compiledState = compilerOutput.value;

  const labelStart = process.hrtime();
  const initialState = await prepareState(compiledState);
  const labelEnd = process.hrtime(labelStart);

  console.log(`Stepping for ${out} ...`);

  const convergeStart = process.hrtime();
  let optimizedState;
  const optimizedOutput = stepUntilConvergence(initialState, 10000);
  if (optimizedOutput.isOk()) {
    optimizedState = optimizedOutput.value;
  } else {
    throw new Error(
      `Optimization failed:\n${showError(optimizedOutput.error)}`
    );
  }
  const convergeEnd = process.hrtime(convergeStart);
  const reactRenderStart = process.hrtime();

  const resolvePath = async (filePath: string) => {
    // Handle absolute URLs
    if (/^(http|https):\/\/[^ "]+$/.test(filePath)) {
      const fileURL = new URL(filePath).href;
      try {
        const fileReq = await fetch(fileURL);
        return fileReq.text();
      } catch (e) {
        return undefined;
      }
    }

    // Relative paths
    const parentDir = parse(join(prefix, sty)).dir;
    const joined = resolve(parentDir, filePath);
    return fs.readFileSync(joined, "utf8").toString();
  };
  const canvas = (await RenderStatic(optimizedState, resolvePath)).outerHTML;

  const reactRenderEnd = process.hrtime(reactRenderStart);
  const overallEnd = process.hrtime(overallStart);

  // cross-instance energy evaluation

  if (folders) {
    // TODO: check for non-zero constraints
    // const energies = JSON.parse(
    //   await runPenrose(Packets.EnergyValues(optimizedState))
    // );
    // const constrs = nonZeroConstraints(optimizedState, energies.contents[1], 1);
    // if (constrs.length > 0) {
    //   console.log("This instance has non-zero constraints: ");
    //   // return;
    // }
    let crossEnergy: number = Infinity;
    if (ciee) {
      console.log(chalk.yellow(`Computing cross energy...`));
      if (referenceState) {
        const crossState = {
          ...optimizedState,
          constrFns: referenceState.constrFns,
          objFns: referenceState.objFns,
        };
        try {
          crossEnergy = evalEnergy(await prepareState(crossState));
        } catch (e) {
          console.warn(
            chalk.yellow(
              `Cross-instance energy failed. Returning infinity instead. \n${e}`
            )
          );
        }
      }
    }

    // fetch metadata if available
    let extraMetadata;
    if (extrameta) {
      extraMetadata = JSON.parse(
        fs.readFileSync(join(prefix, extrameta), "utf8").toString()
      );
    }

    const metadata: InstanceData = {
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
      // violatingConstraints: constrs,
      // nonzeroConstraints: constrs.length > 0,
      // selectorMatches: optimizedState.selectorMatches,
      selectorMatches: [],
      optProblem: {
        constraintCount: optimizedState.constrFns.length,
        objectiveCount: optimizedState.objFns.length,
      },
      reference,
      ciee: crossEnergy,
      extra: extraMetadata,
    };
    if (!fs.existsSync(out)) {
      fs.mkdirSync(out, { recursive: true });
    }

    fs.writeFileSync(
      join(out, "output.svg"),
      prettier.format(canvas, { parser: "html" })
    );

    fs.writeFileSync(join(out, "substance.sub"), subIn);
    fs.writeFileSync(join(out, "style.sty"), styIn);
    fs.writeFileSync(join(out, "domain.dsl"), dslIn);
    fs.writeFileSync(join(out, "meta.json"), JSON.stringify(metadata, null, 2));
    console.log(
      chalk.green(`The diagram and metadata has been saved to ${out}`)
    );
    // returning metadata for aggregation
    return { metadata, state: optimizedState };
  } else {
    const parentFolder = dirname(out);
    if (!fs.existsSync(parentFolder)) {
      fs.mkdirSync(parentFolder, { recursive: true });
    }
    fs.writeFileSync(out, prettier.format(canvas, { parser: "html" }));
    console.log(chalk.green(`The diagram has been saved as ${out}`));

    // HACK: return empty metadata??
    return undefined;
  }
};

// Takes a trio of registries/libraries and runs `singleProcess` on each substance program.
const batchProcess = async (
  lib: any,
  folders: boolean,
  out: string,
  prefix: string
) => {
  const registry = JSON.parse(fs.readFileSync(join(prefix, lib)).toString());
  const substanceLibrary = registry["substances"];
  const styleLibrary = registry["styles"];
  const domainLibrary = registry["domains"];
  const trioLibrary = registry["trios"];
  console.log(`Processing ${trioLibrary.length} substance files...`);

  let referenceFlag = true;
  let reference = trioLibrary[0];
  let referenceState = undefined;

  const finalMetadata: AggregateData = {};
  // NOTE: for parallelism, use forEach.
  // But beware the console gets messy and it's hard to track what failed
  for (const { domain, style, substance, variation, meta } of trioLibrary) {
    // try to render the diagram
    const id = uniqid("instance-");
    const name = `${substance}-${style}`;
    try {
      const { name: subName, URI: subURI } = substanceLibrary[substance];
      const { name: styName, URI: styURI, plugin } = styleLibrary[style];
      const { name: dslName, URI: dslURI } = domainLibrary[domain];

      if (plugin) {
        console.log(
          chalk.red(
            `Skipping "${name}" (${subURI}) for now; this domain requires a plugin or has known issues.`
          )
        );
        continue;
      }

      // Warning: will face id conflicts if parallelism used
      const res = await singleProcess(
        variation,
        subURI,
        styURI,
        dslURI,
        folders,
        join(out, `${name}${folders ? "" : ".svg"}`),
        prefix,
        {
          substanceName: subName,
          styleName: styName,
          domainName: dslName,
          id,
        },
        reference,
        referenceState,
        meta
      );
      if (folders && res !== undefined) {
        const { metadata, state } = res;
        if (referenceFlag) {
          referenceState = state;
          referenceFlag = false;
        }
        finalMetadata[id] = metadata;
      }
    } catch (e) {
      console.trace(
        chalk.red(
          `${id} exited with an error. The Substance program ID is ${substance}. The error message is:\n${e}`
        )
      );
    }
  }

  if (folders) {
    fs.writeFileSync(
      join(out, "aggregateData.json"),
      JSON.stringify(finalMetadata, null, 2)
    );
    console.log(`The Aggregate metadata has been saved to ${out}.`);
  }
  console.log("done.");
};

/**
 * Retrieves defintions for all shapes and writes their properties to a JSON
 * file.  If a filename is not provided, the result is written to stdout.
 *
 * @param outFile The output file (optional)
 */
const getShapeDefs = (outFile?: string): void => {
  const outShapes = {}; // List of shapes with properties
  const size = 19; // greater than 3*6; see randFloat usage in Samplers.ts

  // Loop over the shapes
  for (const shapeName in shapedefs) {
    const thisShapeDef: ShapeDef = shapedefs[shapeName];
    const shapeSample1 = thisShapeDef.sampler(
      simpleContext("ShapeProps sample 1"),
      makeCanvas(size, size)
    );
    const shapeSample2 = thisShapeDef.sampler(
      simpleContext("ShapeProps sample 2"),
      makeCanvas(size, size)
    );
    const outThisShapeDef = { sampled: {}, defaulted: {} };
    outShapes[shapeName] = outThisShapeDef;

    // Loop over the properties
    for (const propName in shapeSample1) {
      const outThisShapeProp = {};

      const sample1Str = JSON.stringify(shapeSample1[propName].contents);
      const sample2Str = JSON.stringify(shapeSample2[propName].contents);

      if (sample1Str === sample2Str) {
        outThisShapeDef.defaulted[propName] = shapeSample1[propName];
      } else {
        outThisShapeDef.sampled[propName] = shapeSample1[propName];
      }
    }
  }

  // Write the shape definition output
  if (outFile === undefined) {
    console.log(JSON.stringify(outShapes, null, 2));
  } else {
    fs.writeFileSync(outFile, JSON.stringify(outShapes, null, 2));
    console.log(`Wrote shape definitions to: ${outFile}`);
  }
};

/**
 * Main function body
 */
(async () => {
  // Process command-line arguments
  const args = neodoc.run(USAGE, { smartOptions: true });

  // Determine the output file path
  const folders = args["--folders"] || false;
  const ciee = args["--cross-energy"] || false;
  const browserFolder = args["--render"];
  const outFile =
    args["--outFile"] || join(args.OUTFOLDER || "./", "output.svg");
  const times = args["--repeat"] || 1;
  const prefix = args["--src-prefix"];
  const variation = args["--variation"] || randomBytes(20).toString("hex");

  if (args.batch) {
    for (let i = 0; i < times; i++) {
      await batchProcess(args.LIB, folders, args.OUTFOLDER, prefix);
    }
    if (browserFolder) {
      renderArtifacts(args.OUTFOLDER, browserFolder);
    }
  } else if (args.render) {
    renderArtifacts(args.ARTIFACTSFOLDER, args.OUTFOLDER);
  } else if (args.textchart) {
    printTextChart(args.ARTIFACTSFOLDER, args.OUTFILE);
  } else if (args.draw) {
    await singleProcess(
      variation,
      args.SUBSTANCE,
      args.STYLE,
      args.DOMAIN,
      folders,
      folders ? args.OUTFOLDER : outFile,
      prefix,
      {
        substanceName: args.SUBSTANCE,
        styleName: args.STYLE,
        domainName: args.DOMAIN,
        id: uniqid("instance-"),
      },
      undefined, // reference
      undefined, // referenceState
      undefined, // extraMetadata
      ciee
    );
  } else if (args.shapedefs) {
    getShapeDefs(args["SHAPEFILE"]);
  } else {
    throw new Error("Invalid command line argument");
  }
})();

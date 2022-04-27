require("global-jsdom/register");
import {
  compileTrio,
  evalEnergy,
  getListOfStagedStates,
  prepareState,
  RenderStatic,
  resample,
  showError,
  stepUntilConvergence,
} from "@penrose/core";
import chalk from "chalk";
import convertHrtime from "convert-hrtime";
import { randomBytes } from "crypto";
import * as fs from "fs";
import neodoc from "neodoc";
import { dirname, join, parse, resolve } from "path";
import uniqid from "uniqid";
import { renderArtifacts } from "./artifacts";

const USAGE = `
Penrose Automator.

Usage:
  automator batch LIB OUTFOLDER [--folders] [--src-prefix=PREFIX] [--repeat=TIMES] [--render=OUTFOLDER] [--staged] [--cross-energy]
  automator render ARTIFACTSFOLDER OUTFOLDER
  automator draw SUBSTANCE STYLE DOMAIN OUTFOLDER [--src-prefix=PREFIX] [--staged] [--variation=VARIATION] [--folders] [--cross-energy]

Options:
  -o, --outFile PATH Path to either an SVG file or a folder, depending on the value of --folders. [default: output.svg]
  --folders Include metadata about each output diagram. If enabled, outFile has to be a path to a folder.
  --src-prefix PREFIX the prefix to SUBSTANCE, STYLE, and DOMAIN, or the library equivalent in batch mode. No trailing "/" required. [default: .]
  --repeat TIMES the number of instances 
  --staged Generate staged SVGs of the final diagram
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
  staged: boolean,
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
  let compiledState;
  if (compilerOutput.isOk()) {
    compiledState = compilerOutput.value;
  } else {
    const err = compilerOutput.error;
    throw new Error(`Compilation failed:\n${showError(err)}`);
  }

  const labelStart = process.hrtime();
  // resample because initial sampling did not use the special sampling seed
  const initialState = resample(await prepareState(compiledState));
  const labelEnd = process.hrtime(labelStart);

  console.log(`Stepping for ${out} ...`);

  const convergeStart = process.hrtime();
  // TODO: report runtime errors
  const optimizedState = stepUntilConvergence(
    initialState,
    10000
  ).unsafelyUnwrap();
  const convergeEnd = process.hrtime(convergeStart);

  const reactRenderStart = process.hrtime();

  // make a list of canvas data if staged (prepare to generate multiple SVGs)
  let listOfCanvasData, canvas;
  const resolvePath = async (filePath: string) => {
    const parentDir = parse(join(prefix, sty)).dir;
    const joined = resolve(parentDir, filePath);
    return fs.readFileSync(joined, "utf8").toString();
  };
  if (staged) {
    const listOfStagedStates = getListOfStagedStates(optimizedState);
    for (const state of listOfStagedStates) {
      listOfCanvasData.push((await RenderStatic(state, resolvePath)).outerHTML);
    }
  } else {
    // if not staged, we just need one canvas data (for the final diagram)
    canvas = (await RenderStatic(optimizedState, resolvePath)).outerHTML;
  }

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
    let crossEnergy = undefined;
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

    // if staged, write each canvas data out as an SVG
    if (staged) {
      const writeFileOut = (canvasData: any, index: number) => {
        // add an index num to the output filename so the user knows the order
        // and also to keep unique filenames
        let filename = join(out, `output${index.toString()}.svg`);
        fs.writeFileSync(filename, canvasData);
        console.log(chalk.green(`The diagram has been saved as ${filename}`));
      };
      listOfCanvasData.map(writeFileOut);
    } else {
      // not staged --> just need one diagram
      fs.writeFileSync(join(out, "output.svg"), canvas);
    }

    fs.writeFileSync(join(out, "substance.sub"), subIn);
    fs.writeFileSync(join(out, "style.sty"), styIn);
    fs.writeFileSync(join(out, "domain.dsl"), dslIn);
    fs.writeFileSync(join(out, "meta.json"), JSON.stringify(metadata));
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
    if (staged) {
      // write multiple svg files out
      const writeFileOut = (canvasData: any, index: number) => {
        let filename = out.slice(0, out.indexOf("svg") - 1);
        let newStr = `${filename}${index.toString()}.svg`;
        fs.writeFileSync(newStr, canvasData);
        console.log(chalk.green(`The diagram has been saved as ${newStr}`));
      };
      listOfCanvasData.map(writeFileOut);
    } else {
      // just the final diagram
      fs.writeFileSync(out, canvas);
      console.log(chalk.green(`The diagram has been saved as ${out}`));
    }

    // HACK: return empty metadata??
    return undefined;
  }
};

// Takes a trio of registries/libraries and runs `singleProcess` on each substance program.
const batchProcess = async (
  lib: any,
  folders: boolean,
  out: string,
  prefix: string,
  staged: boolean
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

  const finalMetadata = {};
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
        join(out, `${name}-${id}${folders ? "" : ".svg"}`),
        prefix,
        staged,
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
      if (folders) {
        const { metadata, state } = res;
        if (referenceFlag) {
          referenceState = state;
          referenceFlag = false;
        }
        finalMetadata[id] = metadata;
      }
    } catch (e) {
      console.log(
        chalk.red(
          `${id} exited with an error. The Substance program ID is ${substance}. The error message is:\n${e}`
        )
      );
    }
  }

  if (folders) {
    fs.writeFileSync(
      join(out, "aggregateData.json"),
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
  const ciee = args["--cross-energy"] || false;
  const browserFolder = args["--render"];
  const outFile = args["--outFile"] || join(args.OUTFOLDER, "output.svg");
  const times = args["--repeat"] || 1;
  const prefix = args["--src-prefix"];
  const staged = args["--staged"] || false;
  const variation = args["--variation"] || randomBytes(20).toString("hex");

  if (args.batch) {
    for (let i = 0; i < times; i++) {
      await batchProcess(args.LIB, folders, args.OUTFOLDER, prefix, staged);
    }
    if (browserFolder) {
      renderArtifacts(args.OUTFOLDER, browserFolder);
    }
  } else if (args.render) {
    renderArtifacts(args.ARTIFACTSFOLDER, args.OUTFOLDER);
  } else if (args.draw) {
    await singleProcess(
      variation,
      args.SUBSTANCE,
      args.STYLE,
      args.DOMAIN,
      folders,
      folders ? args.OUTFOLDER : outFile,
      prefix,
      staged,
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
  } else {
    throw new Error("Invalid command line argument");
  }
})();

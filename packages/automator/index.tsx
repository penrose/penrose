require("global-jsdom/register");
import {
  compileTrio,
  evalEnergy,
  prepareState,
  RenderStatic,
  showError,
  stepUntilConvergence,
  trackOptimizationHistory,
  PenroseState,
  getListOfStagedStates,
  PenroseOptimizerHyperparams,
  PENROSE_DEFAULT_HYPERPARAMS,
} from "@penrose/core";
import { renderArtifacts } from "./artifacts";

const fs = require("fs");
const chalk = require("chalk");
const neodoc = require("neodoc");
const uniqid = require("uniqid");
const convertHrtime = require("convert-hrtime");

const USAGE = `
Penrose Automator.

Usage:
  automator batch LIB OUTFOLDER [--folders]  [--src-prefix=PREFIX] [--repeat=TIMES] [--render=OUTFOLDER] [--staged]
  automator render ARTIFACTSFOLDER OUTFOLDER
  automator draw SUBSTANCE STYLE DOMAIN OUTFOLDER [--src-prefix=PREFIX] [--staged]
  automator evaluate SUBSTANCE STYLE DOMAIN OUTFOLDER [--repeat=TIMES] [--src-prefix=PREFIX] [--grid-search]

Options:
  -o, --outFile PATH Path to either an SVG file or a folder, depending on the value of --folders. [default: output.svg]
  --folders Include metadata about each output diagram. If enabled, outFile has to be a path to a folder.
  --src-prefix PREFIX the prefix to SUBSTANCE, STYLE, and DOMAIN, or the library equivalent in batch mode. No trailing "/" required. [default: .]
  --repeat TIMES the number of instances 
  --staged Generate staged SVGs of the final diagram
  --grid-search Systematically evaluate optimization performance using a predefined list of hyperparameters
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
  sub: any,
  sty: any,
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
  extrameta?
) => {
  // Fetch Substance, Style, and Domain files
  const [subIn, styIn, dslIn] = [sub, sty, dsl].map((arg) =>
    fs.readFileSync(`${prefix}/${arg}`, "utf8").toString()
  );

  // Compilation
  console.log(`Compiling for ${out}/${sub} ...`);
  const overallStart = process.hrtime();
  const compileStart = process.hrtime();
  const compilerOutput = compileTrio(dslIn, subIn, styIn);
  const compileEnd = process.hrtime(compileStart);
  let compiledState;
  if (compilerOutput.isOk()) {
    compiledState = compilerOutput.value;
  } else {
    const err = compilerOutput.error;
    throw new Error(`Compilation failed:\n${showError(err)}`);
  }

  const labelStart = process.hrtime();
  const initialState = await prepareState(compiledState);
  const labelEnd = process.hrtime(labelStart);

  console.log(`Stepping for ${out} ...`);

  const convergeStart = process.hrtime();
  const optimizedState = stepUntilConvergence(initialState, 10000).value;
  const convergeEnd = process.hrtime(convergeStart);

  const reactRenderStart = process.hrtime();

  // make a list of canvas data if staged (prepare to generate multiple SVGs)
  let listOfCanvasData, canvas;
  if (staged) {
    const listOfStagedStates = getListOfStagedStates(optimizedState);
    listOfCanvasData = listOfStagedStates.map((state: PenroseState) => {
      return RenderStatic(state).outerHTML;
    });
  } else {
    // if not staged, we just need one canvas data (for the final diagram)
    canvas = RenderStatic(optimizedState).outerHTML;
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
    console.log(chalk.yellow(`Computing cross energy...`));
    let crossEnergy = undefined;
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

    // fetch metadata if available
    let extraMetadata;
    if (extrameta) {
      extraMetadata = JSON.parse(
        fs.readFileSync(`${prefix}/${extrameta}`, "utf8").toString()
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
        let filename = `${out}/output${index.toString()}.svg`;
        fs.writeFileSync(filename, canvasData);
        console.log(chalk.green(`The diagram has been saved as ${filename}`));
      };
      listOfCanvasData.map(writeFileOut);
    } else {
      // not staged --> just need one diagram
      fs.writeFileSync(`${out}/output.svg`, canvas);
    }

    fs.writeFileSync(`${out}/substance.sub`, subIn);
    fs.writeFileSync(`${out}/style.sty`, styIn);
    fs.writeFileSync(`${out}/domain.dsl`, dslIn);
    fs.writeFileSync(`${out}/meta.json`, JSON.stringify(metadata));
    console.log(
      chalk.green(`The diagram and metadata has been saved to ${out}`)
    );
    // returning metadata for aggregation
    return { metadata, state: optimizedState };
  } else {
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
    return null;
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
  const registry = JSON.parse(fs.readFileSync(`${prefix}/${lib}`).toString());
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
  for (const { domain, style, substance, meta } of trioLibrary) {
    const name = `${substance}-${style}`;
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

    const id = uniqid("instance-");
    // try to render the diagram
    try {
      // Warning: will face id conflicts if parallelism used
      const res = await singleProcess(
        subURI,
        styURI,
        dslURI,
        folders,
        `${out}/${name}-${id}${folders ? "" : ".svg"}`,
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
          `${id} exited with an error. The Substance program ID is ${subName}. The error message is:\n${e}`
        )
      );
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

// Measure optimizer with given hyperparameters performance throught multiple runs
const runEvaluate = async (
  compiledState: PenroseState,
  times: number,
  hyperParams: PenroseOptimizerHyperparams
) => {
  let allResults = [];
  for (let i = 0; i < times; i++) {
    console.log(`Evaluation run ${i} out of ${times}.`);
    const initialState = await prepareState(compiledState);
    const result = trackOptimizationHistory(initialState, 10000, hyperParams);
    allResults = [...allResults, result];
  }
  return allResults;
};

// Definition of hyperparameters to evaluate uding grid search
const getAllHyperParameters = (): PenroseOptimizerHyperparams[] => {
  let hps = [];
  for (let c1 = 1e-5; c1 <= 1e-1; c1 *= 10) {
    for (let c2 = 0.1; c2 <= 0.9; c2 += 0.1) {
      const hp = {
        ...PENROSE_DEFAULT_HYPERPARAMS,
        lineSearch: {
          ...PENROSE_DEFAULT_HYPERPARAMS.lineSearch,
          c1: c1,
          c2: c2,
        },
      };
      hps = [...hps, hp];
    }
  }
  return hps;
};

// Measure optimizer performance throught multiple runs
const evaluate = async (
  sub: any,
  sty: any,
  dsl: string,
  out: string,
  prefix: string,
  times: number,
  gridSearch: boolean
) => {
  // Prepare the optimization problem
  console.log(`Preparing optimization problem from ${sub}, ${sty} and ${dsl}.`);
  const [subIn, styIn, dslIn] = [sub, sty, dsl].map((arg) =>
    fs.readFileSync(`${prefix}/${arg}`, "utf8").toString()
  );
  const compilerOutput = compileTrio(dslIn, subIn, styIn);
  let compiledState;
  if (compilerOutput.isOk()) {
    compiledState = compilerOutput.value;
  } else {
    const err = compilerOutput.error;
    throw new Error(`Compilation failed:\n${showError(err)}`);
  }

  // Run the optimizer and collect performance metrics
  let results = [];
  let allHyperParams = [PENROSE_DEFAULT_HYPERPARAMS];
  if (gridSearch) {
    allHyperParams = getAllHyperParameters();
  }
  for (let hp of allHyperParams) {
    console.log(hp);
    results = [...results, { hp: runEvaluate(compiledState, times, hp) }];
  }

  // Save results to a json file
  fs.writeFileSync(`${out}/results.json`, JSON.stringify(results));
  console.log(`The results have been saved to ${out}.`);
};

(async () => {
  // Process command-line arguments
  const args = neodoc.run(USAGE, { smartOptions: true });

  // Determine the output file path
  const folders = args["--folders"] || false;
  const browserFolder = args["--render"];
  const outFile = args["--outFile"] || `${args.OUTFOLDER}/output.svg`;
  const times = args["--repeat"] || 1;
  const prefix = args["--src-prefix"];

  const staged = args["--staged"] || false;
  const gridSearch = args["--grid-search"] || false;

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
      args.SUBSTANCE,
      args.STYLE,
      args.DOMAIN,
      folders,
      outFile,
      prefix,
      staged
    );
  } else if (args.evaluate) {
    await evaluate(
      args.SUBSTANCE,
      args.STYLE,
      args.DOMAIN,
      args.OUTFOLDER,
      prefix,
      times,
      gridSearch
    );
  } else {
    throw new Error("Invalid command line argument");
  }
})();

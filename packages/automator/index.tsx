require("global-jsdom/register");
import {
  compileTrio,
  prepareState,
  initializeMat,
  RenderStatic,
  stepUntilConvergence,
} from "@penrose/core";

const fs = require("fs");
const chalk = require("chalk");
const neodoc = require("neodoc");
const uniqid = require("uniqid");
const convertHrtime = require("convert-hrtime");

const USAGE = `
Penrose Automator.

Usage:
  automator batch LIB OUTFOLDER [--folders]  [--src-prefix=PREFIX]

Options:
  -o, --outFile PATH Path to either an SVG file or a folder, depending on the value of --folders. [default: output.svg]
  --folders Include metadata about each output diagram. If enabled, outFile has to be a path to a folder.
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
    const err = compiledState.error;
    console.error(`Compilation failed:\n${err.tag}\n${err.contents}`);
    process.exit(1);
  }

  await initializeMat();
  const labelStart = process.hrtime();
  const initialState = await prepareState(compiledState);
  const labelEnd = process.hrtime(labelStart);

  // TODO: Labeling and resolving pending vars

  console.log(`Stepping for ${out} ...`);

  const convergeStart = process.hrtime();
  const optimizedState = stepUntilConvergence(initialState);
  const convergeEnd = process.hrtime(convergeStart);

  // TODO: include metadata prop?
  const reactRenderStart = process.hrtime();
  const canvas = RenderStatic(optimizedState).outerHTML;
  const reactRenderEnd = process.hrtime(reactRenderStart);
  const overallEnd = process.hrtime(overallStart);

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
    };
    if (!fs.existsSync(out)) {
      fs.mkdirSync(out);
    }
    fs.writeFileSync(`${out}/output.svg`, canvas);
    fs.writeFileSync(`${out}/substance.sub`, subIn);
    fs.writeFileSync(`${out}/style.sty`, styIn);
    fs.writeFileSync(`${out}/domain.dsl`, dslIn);
    fs.writeFileSync(`${out}/meta.json`, JSON.stringify(metadata));
    console.log(
      chalk.green(`The diagram and metadata has been saved to ${out}`)
    );
    // returning metadata for aggregation
    return metadata;
  } else {
    fs.writeFileSync(out, canvas);
    console.log(chalk.green(`The diagram has been saved as ${out}`));
    // HACK: return empty metadata??
    return null;
  }
};

// Takes a trio of registries/libraries and runs `singleProcess` on each substance program.
const batchProcess = async (
  lib: any,
  folders: boolean,
  out: string,
  prefix: string
) => {
  const registry = JSON.parse(fs.readFileSync(`${prefix}/${lib}`).toString());
  const substanceLibrary = registry["substances"];
  const styleLibrary = registry["styles"];
  const domainLibrary = registry["domains"];
  const trioLibrary = registry["trios"];
  console.log(`Processing ${trioLibrary.length} substance files...`);

  const finalMetadata = {};
  // NOTE: for parallelism, use forEach.
  // But beware the console gets messy and it's hard to track what failed
  for (const { domain, style, substance } of trioLibrary) {
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
    // Warning: will face id conflicts if parallelism used
    const id = uniqid("instance-");
    const meta = await singleProcess(
      subURI,
      styURI,
      dslURI,
      folders,
      `${out}/${name}-${id}${folders ? "" : ".svg"}`,
      prefix,
      {
        substanceName: subName,
        styleName: styName,
        domainName: dslName,
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
    await batchProcess(args.LIB, folders, args.OUTFOLDER, prefix);
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

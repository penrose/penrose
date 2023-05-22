import "global-jsdom/register"; // must be first

import fetch from "node-fetch";
import yargs from "yargs";
import { hideBin } from "yargs/helpers";

import {
  compileTrio,
  makeCanvas,
  PenroseState,
  prepareState,
  RenderStatic,
  sampleShape,
  ShapeType,
  shapeTypes,
  showError,
  simpleContext,
  stepUntilConvergence,
} from "@penrose/core";
import chalk from "chalk";
import convertHrtime from "convert-hrtime";
import * as fs from "fs";
import { extname, join, resolve } from "path";
import prettier from "prettier";
import { printTextChart } from "./artifacts.js";
import { version } from "./package.json";
import { AggregateData, InstanceData } from "./types.js";
import watch from "./watch.js";

interface Trio {
  substance: string;
  style: string[];
  domain: string;
  variation: string;
}

interface Meta {
  name?: string;
  gallery?: boolean;
}

interface Registry {
  [id: string]: Meta;
}

// In an async context, communicate with the backend to compile and optimize the diagram
const render = async (
  variation: string,
  substance: string,
  style: string,
  domain: string,
  resolvePath: (filePath: string) => Promise<string | undefined>,
  verbose: boolean,
  meta: {
    substanceName: string;
    styleNames: string[];
    domainName: string;
    id: string;
  }
): Promise<{
  diagram: string;
  metadata: InstanceData;
  state: PenroseState;
}> => {
  // create id for reporting
  // Compilation
  if (verbose) console.debug(`Compiling ${meta.id} ...`);
  const overallStart = process.hrtime();
  const compileStart = process.hrtime();
  const compilerOutput = await compileTrio({
    substance,
    style,
    domain,
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

  if (verbose) console.debug(`Stepping for ${meta.id} ...`);

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

  const canvas = (await RenderStatic(optimizedState, resolvePath, "roger"))
    .outerHTML;

  const reactRenderEnd = process.hrtime(reactRenderStart);
  const overallEnd = process.hrtime(overallStart);

  // fetch metadata if available
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
    selectorMatches: [],
    optProblem: {
      constraintCount: optimizedState.constrFns.length,
      objectiveCount: optimizedState.objFns.length,
    },
  };

  return {
    diagram: prettier.format(canvas, { parser: "html" }),
    state: optimizedState,
    metadata,
  };
};

// set up path resolution
const resolvePath = (prefix: string, stylePaths: string[]) => {
  const stylePrefixes = stylePaths.map((sty) => join(prefix, sty, ".."));
  if (new Set(stylePrefixes).size > 1) {
    console.warn(
      chalk.yellow(
        "Warning: the styles in this trio are not co-located. The first style will be used for image resolution."
      )
    );
  }
  const stylePrefix = stylePrefixes[0];
  return async (filePath: string) => {
    // Handle absolute URLs
    if (/^(http|https):\/\/[^ "]+$/.test(filePath)) {
      const fileURL = new URL(filePath).href;
      try {
        const fileReq = await fetch(fileURL);
        return fileReq.text();
      } catch (e) {
        console.error(`Failed to resolve path: ${e}`);
        return undefined;
      }
    }

    // Relative paths
    const joined = resolve(stylePrefix, filePath);
    return fs.readFileSync(joined, "utf8");
  };
};

const readTrio = (sub: string, sty: string[], dsl: string, prefix: string) => {
  // Fetch Substance, Style, and Domain files
  const [substance, domain] = [sub, dsl].map((arg) =>
    fs.readFileSync(join(prefix, arg), "utf8")
  );
  const styles = sty.map((arg) => fs.readFileSync(join(prefix, arg), "utf8"));
  return {
    substance,
    style: styles.join("\n"),
    domain,
  };
};

const renderTrio = async (
  variation: string,
  sub: string,
  sty: string[],
  dsl: string,
  folder: boolean,
  out: string,
  prefix: string,
  meta: {
    substanceName: string;
    styleNames: string[];
    domainName: string;
    id: string;
  }
) => {
  const { substance, style, domain } = readTrio(sub, sty, dsl, prefix);
  // draw diagram and get metadata
  const { diagram, metadata } = await render(
    variation,
    substance,
    style,
    domain,
    resolvePath(prefix, sty),
    true,
    meta
  );

  // write to files
  if (folder) {
    if (!fs.existsSync(out)) {
      fs.mkdirSync(out, { recursive: true });
    }
    fs.writeFileSync(join(out, "substance.substance"), substance);
    fs.writeFileSync(join(out, "style.style"), style);
    fs.writeFileSync(join(out, "domain.domain"), domain);
    fs.writeFileSync(join(out, "meta.json"), JSON.stringify(metadata, null, 2));
    fs.writeFileSync(join(out, "output.svg"), diagram);
    console.log(
      chalk.green(`The diagram and metadata has been saved to ${resolve(out)}`)
    );
    // returning metadata for aggregation
    return metadata;
  } else {
    const dir = join(out, "..");
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }
    fs.writeFileSync(out, diagram);
    console.log(chalk.green(`The diagram has been saved as ${resolve(out)}`));
  }
};

// Takes a trio of registries/libraries and runs `singleProcess` on each substance program.
const renderRegistry = async (lib: string, folders: boolean, out: string) => {
  const prefix = join(lib, "..");
  const registry: Registry = JSON.parse(fs.readFileSync(lib, "utf8"));
  const trioLibrary = Object.keys(registry);
  console.log(`Processing ${trioLibrary.length} trios...`);

  const finalMetadata: AggregateData = {};
  for (const id of trioLibrary) {
    // try to render the diagram
    try {
      const trioPath = join(prefix, `${id}.trio.json`);
      const trioPrefix = join(trioPath, "..");
      const { substance, style, domain, variation }: Trio = JSON.parse(
        fs.readFileSync(trioPath, "utf8")
      );

      // Warning: will face id conflicts if parallelism used
      const metadata = await renderTrio(
        variation,
        substance,
        style,
        domain,
        folders,
        join(out, `${id}${folders ? "" : ".svg"}`),
        trioPrefix,
        {
          substanceName: substance,
          styleNames: style,
          domainName: domain,
          id,
        }
      );
      if (folders && metadata) {
        finalMetadata[id] = metadata;
      }
    } catch (e) {
      process.exitCode = 1;
      console.trace(
        chalk.red(
          `${id} exited with an error. The error message is:\n${e.stack}`
        )
      );
    }
  }

  if (folders) {
    const aggregatePath = join(out, "aggregateData.json");
    fs.writeFileSync(aggregatePath, JSON.stringify(finalMetadata, null, 2));
    console.log(
      chalk.green(`The Aggregate metadata has been saved to ${aggregatePath}`)
    );
  }
  console.log("Done.");
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
  for (const shapeName of shapeTypes) {
    const shapeSample1 = sampleShape(
      shapeName as ShapeType,
      simpleContext("ShapeProps sample 1"),
      makeCanvas(size, size)
    );
    const shapeSample2 = sampleShape(
      shapeName as ShapeType,
      simpleContext("ShapeProps sample 2"),
      makeCanvas(size, size)
    );
    const outThisShapeDef = { sampled: {}, defaulted: {} };
    outShapes[shapeName] = outThisShapeDef;

    // Loop over the properties
    for (const propName in shapeSample1) {
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
    console.log(chalk.green(`Wrote shape definitions to: ${outFile}`));
  }
};

const orderTrio = (unordered: string[]): string[] => {
  const ordered: { [k: string]: string } = {};
  for (const fakeType in unordered) {
    const filename = unordered[fakeType];
    const type = {
      ".sub": "substance",
      ".sty": "style",
      ".dsl": "domain",
      ".substance": "substance",
      ".style": "style",
      ".domain": "domain",
    }[extname(filename)];
    if (!type) {
      console.error(`Unrecognized file extension: ${filename}`);
      process.exit(1);
    }
    if (type in ordered) {
      console.error(
        `Duplicate ${type} files: ${ordered[type]} and ${filename}`
      );
      process.exit(1);
    }
    ordered[type] = filename;
  }
  if ("substance" in ordered && "style" in ordered && "domain" in ordered) {
    return [ordered.substance, ordered.style, ordered.domain];
  } else {
    console.error(`Invalid trio: ${unordered.join(", ")}`);
    process.exit(1);
  }
};

//#region command-line interface

yargs(hideBin(process.argv))
  .scriptName("roger")
  // .description("Command-line interface for Penrose.")
  .version(version)
  .command(
    "trio [trio..]",
    "Generate a diagram from a Penrose trio.",
    (yargs) =>
      yargs
        .options("trio", {
          desc: "Three files pointing to a Penrose trio or a JSON file that links to them.",
          type: "array",
          demandOption: true,
        })
        .option("out", {
          desc: "Name of the output SVG file.",
          alias: "o",
          type: "string",
        })
        .option("path", {
          alias: "p",
          desc: "A common path prefix for the trio files",
          default: ".",
        })
        .option("variation", {
          alias: "v",
          desc: "Variation for the Penrose diagram",
          type: "string",
        }),
    async (options) => {
      let sub: string, sty: string[], dom: string;
      let prefix = options.path;
      let variation = options.variation as string | undefined;
      if (options.trio.length === 1) {
        console.log();

        const trioPath = options.trio[0] as string;
        prefix = join(trioPath, "..");
        // read trio from a JSON file
        const paths: Trio = JSON.parse(
          fs.readFileSync(resolve(trioPath), "utf8")
        );
        dom = paths.domain;
        sub = paths.substance;
        sty = paths.style;
        variation ??= paths.variation;
      } else {
        // load all three files
        const trio = orderTrio(options.trio as string[]);
        [sub, sty, dom] = [trio[0], [trio[1]], trio[2]];
      }
      const { substance, style, domain } = readTrio(sub, sty, dom, prefix);
      // draw diagram and get metadata
      const { diagram } = await render(
        variation ?? "",
        substance,
        style,
        domain,
        resolvePath(prefix, sty),
        false,
        {
          substanceName: sub,
          styleNames: sty,
          domainName: dom,
          id: options.trio.join(", "),
        }
      );
      if (options.out) {
        fs.writeFileSync(options.out, diagram);
        console.log(
          chalk.green(`The diagram has been saved as ${resolve(options.out)}`)
        );
      } else {
        console.log(diagram);
      }
    }
  )
  .command(
    "batch <registry> <out>",
    "Generate diagrams from a registry of Penrose trios.",
    (yargs) =>
      yargs
        .positional("registry", {
          desc: "A JSON registry of Penrose trios.",
          demandOption: true,
          type: "string",
        })
        .positional("out", {
          desc: "A folder containing all generated diagrams.",
          demandOption: true,
          type: "string",
        })
        .option("folders", {
          desc: "Generate each diagram as a folder that includes metadata.",
          default: false,
          type: "boolean",
        }),
    async (options) => {
      await renderRegistry(options.registry, options.folders, options.out);
    }
  )
  .command(
    "watch",
    "Watch the current folder for files & changes (must end in .sub,.substance,.sty,.style,.dsl,.domain)",
    (yargs) =>
      yargs.option("port", {
        desc: "Port number for the WebSocket connection.",
        default: 9160,
        alias: "p",
      }),
    (options) => watch(+options.port)
  )
  .command(
    "shapedefs",
    "Generate a JSON file that contains all shape definitions in the Penrose system.",
    (yargs) =>
      yargs.option("out", {
        alias: "o",
        desc: "Output JSON file.",
        type: "string",
      }),
    (options) => getShapeDefs(options.out)
  )

  .command(
    "textchart <artifacts> <out>",
    "Generate an ASCII chart that shows the performance data.",
    (yargs) =>
      yargs
        .positional("artifacts", {
          desc: "A folder that contains roger outputs and metadata.",
          demandOption: true,
          type: "string",
        })
        .positional("out", {
          desc: "An output markdown file.",
          demandOption: true,
          type: "string",
        }),
    (options) => printTextChart(options.artifacts, options.out)
  )
  .demandCommand()
  .strict()
  .help().argv;

//#endregion

import "global-jsdom/register"; // must be first
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
import { dirname, join, parse, resolve } from "path";
import prettier from "prettier";
import uniqid from "uniqid";
import draw from "./draw";
import { version } from "./package.json";
import { AggregateData, InstanceData } from "./types";
import watch from "./watch";

// In an async context, communicate with the backend to compile and optimize the diagram
const render = async (
  variation: string,
  substance: string,
  style: string,
  domain: string,
  resolvePath: (string) => Promise<string | undefined>,
  verbose: boolean,
  meta: {
    substanceName: string;
    styleName: string;
    domainName: string;
    id: string;
  }
): Promise<{
  diagram: string;
  metadata: InstanceData;
  state: PenroseState;
}> => {
  // create id for reporting
  const { substanceName, styleName, domainName } = meta;
  const id = `${domainName}-${styleName}-${substanceName}`;
  // Compilation
  if (verbose) console.debug(`Compiling ${id} ...`);
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

  if (verbose) console.debug(`Stepping for ${id} ...`);

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

const readTrio = (sub: string, sty: string, dsl: string, prefix: string) => {
  // Fetch Substance, Style, and Domain files
  const [substance, style, domain] = [sub, sty, dsl].map((arg) =>
    fs.readFileSync(join(prefix, arg), "utf8").toString()
  );
  // set up path resolution
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
  return {
    substance,
    style,
    domain,
    resolvePath,
  };
};

const renderTrio = async (
  variation: string,
  sub: string,
  sty: string,
  dsl: string,
  folder: boolean,
  out: string,
  prefix: string,
  meta: {
    substanceName: string;
    styleName: string;
    domainName: string;
    id: string;
  }
) => {
  const { substance, style, domain, resolvePath } = readTrio(
    sub,
    sty,
    dsl,
    prefix
  );
  // draw diagram and get metadata
  const { diagram, metadata } = await render(
    variation,
    substance,
    style,
    domain,
    resolvePath,
    true,
    meta
  );

  // write to files
  const parentFolder = dirname(out);
  if (!fs.existsSync(parentFolder)) {
    fs.mkdirSync(parentFolder, { recursive: true });
  }
  if (folder) {
    fs.writeFileSync(join(out, "substance.substance"), substance);
    fs.writeFileSync(join(out, "style.style"), style);
    fs.writeFileSync(join(out, "domain.domain"), domain);
    fs.writeFileSync(join(out, "meta.json"), JSON.stringify(metadata, null, 2));
    fs.writeFileSync(join(out, "output.svg"), diagram);
    console.log(
      chalk.green(`The diagram and metadata has been saved to ${out}`)
    );
    // returning metadata for aggregation
    return metadata;
  } else {
    fs.writeFileSync(out, diagram);
    console.log(chalk.green(`The diagram has been saved as ${out}`));
  }
};

// Takes a trio of registries/libraries and runs `singleProcess` on each substance program.
const renderRegistry = async (
  lib: string,
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

  const finalMetadata: AggregateData = {};
  // NOTE: for parallelism, use forEach.
  // But beware the console gets messy and it's hard to track what failed
  for (const { domain, style, substance, variation } of trioLibrary) {
    // try to render the diagram
    const id = uniqid("instance-");
    const name = `${substance}-${style}`;
    try {
      const { URI: subURI } = substanceLibrary[substance];
      const { URI: styURI } = styleLibrary[style];
      const { URI: dslURI } = domainLibrary[domain];

      // Warning: will face id conflicts if parallelism used
      const metadata = await renderTrio(
        variation,
        subURI,
        styURI,
        dslURI,
        folders,
        join(out, `${name}${folders ? "" : ".svg"}`),
        prefix,
        {
          substanceName: substance,
          styleName: style,
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
          `${name} exited with an error. The Substance program ID is ${substance}. The error message is:\n${e}`
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

//#region command-line interface

yargs(hideBin(process.argv))
  .scriptName("roger")
  // .description("Command-line interface for Penrose.")
  .version(version)
  .command(
    "draw <json>",
    "Generate one diagram from a JSON file.",
    (yargs) =>
      yargs
        .positional("json", {
          type: "string",
          desc:
            "A JSON file that links to Domain, Substance, and Style programs.",
          demandOption: true,
        })
        .option("variation", {
          alias: "v",
          desc: "Variation for the Penrose diagram",
          default: "",
        })
        .requiresArg("json"),
    (argv) => draw(argv.json, argv.variation)
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
        desc: "Output JSON file.",
        type: "string",
      }),
    (options) => getShapeDefs(options.out)
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
          default: ".",
        })
        .option("folders", {
          desc: "Generate each diagram as a folder that includes metadata.",
          default: false,
        })
        .option("path", {
          alias: "p",
          desc:
            "Path prefix for both the registry file itself and all paths to trios in the registry.",
          default: ".",
        }),
    async (options) => {
      await renderRegistry(
        options.registry,
        options.folders,
        options.out,
        options.path
      );
    }
  )
  .command(
    "trio <substance> <style> <domain>",
    "Generate a diagram from a Penrose trio.",
    (yargs) =>
      yargs
        .positional("substance", {
          desc: "The Substance program.",
          type: "string",
          demandOption: true,
        })
        .positional("style", {
          desc: "The Style program.",
          type: "string",
          demandOption: true,
        })
        .positional("domain", {
          desc: "The Domain program.",
          type: "string",
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
          default: "",
        }),
    async (options) => {
      const { substance, style, domain, resolvePath } = readTrio(
        options.substance,
        options.style,
        options.domain,
        options.path
      );
      // draw diagram and get metadata
      const { diagram } = await render(
        options.variation,
        substance,
        style,
        domain,
        resolvePath,
        false,
        {
          substanceName: substance,
          styleName: style,
          domainName: domain,
          id: uniqid("instance-"),
        }
      );
      if (options.out) {
        fs.writeFileSync(options.out, diagram);
      } else {
        console.log(diagram);
      }
    }
  )
  .help().argv;

//#endregion

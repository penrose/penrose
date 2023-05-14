import "global-jsdom/register"; // must be first

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
import { Command } from "commander";
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
  console.log(`Compiling ${id} ...`);
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

  console.log(`Stepping for ${id} ...`);

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
  // Fetch Substance, Style, and Domain files
  const [subIn, styIn, dslIn] = [sub, sty, dsl].map((arg) =>
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

  // draw diagram and get metadata
  const { diagram, metadata } = await render(
    variation,
    subIn,
    styIn,
    dslIn,
    resolvePath,
    meta
  );

  // write to files
  const parentFolder = dirname(out);
  if (!fs.existsSync(parentFolder)) {
    fs.mkdirSync(parentFolder, { recursive: true });
  }
  if (folder) {
    fs.writeFileSync(join(out, "substance.substance"), subIn);
    fs.writeFileSync(join(out, "style.style"), styIn);
    fs.writeFileSync(join(out, "domain.domain"), dslIn);
    fs.writeFileSync(join(out, "meta.json"), JSON.stringify(metadata, null, 2));
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
  lib: any,
  folders: boolean,
  out: string,
  prefix: string
) => {
  console.log(process.cwd());

  const registry = JSON.parse(
    fs.readFileSync(join("./", prefix, lib)).toString()
  );
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

const roger = new Command();

roger
  .name("roger")
  .description("Command-line interface for Penrose.")
  .version(version);
roger
  .command("draw")
  .description("Generate one diagram from a JSON file.")
  .argument(
    "<diagram-json>",
    "A JSON file that links to Domain, Substance, and Style programs."
  )
  .option("-v, --variation", "Variation string for the diagram.", "")
  .action((trioJSON) => draw(trioJSON, ""));
roger
  .command("watch")
  .description(
    "Watch the current folder for files & changes (must end in .sub,.substance,.sty,.style,.dsl,.domain)"
  )
  .option("-p, --port", "Port number for the WebSocket connection.", "9160")
  .action((options) => watch(+options.port));
roger
  .command("shapedefs")
  .description(
    "Generate a JSON file that contains all shape definitions in the Penrose system."
  )
  .option("-o, --out <file>", "Output JSON file.")
  .action((options) => getShapeDefs(options.out));

roger
  .command("trio")
  .description("Generate a diagram from a Penrose trio.")
  .argument("<substance>", "The Substance program")
  .argument("<style>", "The Style program")
  .argument("<domain>", "The Domain program")
  .option("-o, --out <out>", "Name of the output SVG file.", "diagram.svg")
  .option("-p, --path <path>", "A common path prefix for the trio files", "")
  .option("-v, --variation", "Variation string for the diagram.", "")
  .action(async (substance, style, domain, options) => {
    await renderTrio(
      options.variation,
      substance,
      style,
      domain,
      false,
      options.out,
      options.path,
      {
        substanceName: substance,
        styleName: style,
        domainName: domain,
        id: uniqid("instance-"),
      }
    );
  });

roger
  .command("batch")
  .description("Generate diagrams from a registry of Penrose trios.")
  .argument("<registry>", "A JSON registry of Penrose trios.")
  .argument("<out>", "A folder containing all generated diagrams.")
  .option(
    "--folders",
    "Generate each diagram as a folder that includes metadata.",
    false
  )
  .option(
    "-p, --path <path>",
    "Path prefix for both the registry file itself and all paths to trios in the registry.",
    ""
  )
  .action(async (registry, out, options) => {
    await renderRegistry(registry, options.folders, out, options.path);
  });

roger.parse();
//#endregion

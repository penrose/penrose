import "global-jsdom/register"; // must be first

import fetch from "node-fetch";
import yargs from "yargs";
import { hideBin } from "yargs/helpers";

import {
  PenroseState,
  compile,
  optimize,
  showError,
  toSVG,
} from "@penrose/core";
import chalk from "chalk";
import convertHrtime from "convert-hrtime";
import * as fs from "fs";
import { basename, extname, join, resolve } from "path";
import prettier from "prettier";
// import packageJSON from "./package.json"; // TODO: no supported by node
import { InstanceData } from "./types.js";
import watch from "./watch.js";

// npx defaults to the nearest project root directory, thus preventing `roger` to be run inside of `packages/examples`. However, unlike `process.cwd()`, `INIT_CWD` does point to the origin directory. Therefore, we attempt to cd into the actual working directory when starting the script.
if (process.env.INIT_CWD) {
  process.chdir(process.env.INIT_CWD);
}

interface Trio {
  substance: string;
  style: string[];
  domain: string;
  variation: string;
  excludeWarnings?: string[];
}

// In an async context, communicate with the backend to compile and optimize the diagram
const render = async (
  variation: string,
  substance: string,
  style: string,
  domain: string,
  resolvePath: (filePath: string) => Promise<string | undefined>,
  texLabels: boolean,
  verbose: boolean,
  meta: {
    substanceName: string;
    styleNames: string[];
    domainName: string;
    id: string;
  },
  excludeWarnings: string[],
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
  const compilerOutput = await compile({
    substance,
    style,
    domain,
    variation,
    excludeWarnings,
  });
  const compileEnd = process.hrtime(compileStart);
  if (compilerOutput.isErr()) {
    const err = compilerOutput.error;
    throw new Error(`Compilation failed:\n${showError(err)}`);
  }
  const initialState = compilerOutput.value;

  if (verbose) console.debug(`Stepping for ${meta.id} ...`);

  const convergeStart = process.hrtime();
  let optimizedState;
  const optimizedOutput = optimize(initialState);
  if (optimizedOutput.isOk()) {
    optimizedState = optimizedOutput.value;
  } else {
    throw new Error(
      `Optimization failed:\n${showError(optimizedOutput.error)}`,
    );
  }
  const convergeEnd = process.hrtime(convergeStart);
  const reactRenderStart = process.hrtime();

  const canvas = (await toSVG(optimizedState, resolvePath, "roger", texLabels))
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
    diagram: await prettier.format(canvas, { parser: "html" }),
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
        "Warning: the styles in this trio are not co-located. The first style will be used for image resolution.",
      ),
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
    fs.readFileSync(join(prefix, arg), "utf8"),
  );
  const styles = sty.map((arg) => fs.readFileSync(join(prefix, arg), "utf8"));
  return {
    substance,
    style: styles.join("\n"),
    domain,
  };
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
        `Duplicate ${type} files: ${ordered[type]} and ${filename}`,
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
  // .version(packageJSON.version) // TODO: not supported by node
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
        .option("tex-labels", {
          desc: "Render Equation shapes as plain TeX strings in the output SVG",
          type: "boolean",
          default: false,
        })
        .option("variation", {
          alias: "v",
          desc: "Variation for the Penrose diagram",
          type: "string",
        }),
    async (options) => {
      let sub: string, sty: string[], dom: string;
      let prefix = options.path;
      const texLabels = options.texLabels;
      let variation = options.variation as string | undefined;
      let excludeWarnings: string[] | undefined = undefined;
      if (options.trio.length === 1) {
        const trioPath = options.trio[0] as string;
        prefix = join(trioPath, "..");
        // read trio from a JSON file
        const paths: Trio = JSON.parse(
          fs.readFileSync(resolve(trioPath), "utf8"),
        );
        dom = paths.domain;
        sub = paths.substance;
        sty = paths.style;
        variation ??= paths.variation;
        excludeWarnings = paths.excludeWarnings;
      } else {
        // load all three files
        const trio = orderTrio(options.trio as string[]);
        [sub, sty, dom] = [trio[0], [trio[1]], trio[2]];
      }

      if (excludeWarnings === undefined) {
        excludeWarnings = [];
      }

      const { substance, style, domain } = readTrio(sub, sty, dom, prefix);
      // draw diagram and get metadata
      const { diagram, state } = await render(
        variation ?? "",
        substance,
        style,
        domain,
        resolvePath(prefix, sty),
        texLabels,
        false,
        {
          substanceName: sub,
          styleNames: sty,
          domainName: dom,
          id: options.trio.join(", "),
        },
        excludeWarnings,
      );
      if (options.out) {
        fs.writeFileSync(options.out, diagram);
        console.log(
          chalk.green(`The diagram has been saved as ${resolve(options.out)}`),
        );
      } else {
        console.log(diagram);
        for (const warning of state.warnings) {
          const warnStr = showError(warning);
          console.warn(chalk.yellow("Warning in diagram: " + warnStr));
        }
      }
    },
  )
  .command(
    "trios [trios..]",
    "Create diagrams from multiple .trio.json files.",
    (yargs) =>
      yargs
        .options("trios", {
          desc: "Any number of .trio.json files of Penrose trios.",
          type: "array",
          demandOption: true,
        })
        .option("tex-labels", {
          desc: "Render Equation shapes as plain TeX strings in the output SVG",
          type: "boolean",
          default: false,
        })
        .option("out", {
          desc: "Output folder containing the SVG files",
          alias: "o",
          type: "string",
          demandOption: true,
        }),
    async (options) => {
      for (const trio of options.trios) {
        const trioPath = trio as string;
        const prefix = join(trioPath, "..");
        const texLabels = options.texLabels;
        const trioName = basename(trioPath, ".trio.json");
        // read trio from a JSON file
        const paths: Trio = JSON.parse(
          fs.readFileSync(resolve(trioPath), "utf8"),
        );
        const dom = paths.domain;
        const sub = paths.substance;
        const sty = paths.style;
        const variation = paths.variation;
        const excludeWarnings =
          paths.excludeWarnings === undefined ? [] : paths.excludeWarnings;
        const { substance, style, domain } = readTrio(sub, sty, dom, prefix);
        const { diagram, state } = await render(
          variation,
          substance,
          style,
          domain,
          resolvePath(prefix, sty),
          texLabels,
          false,
          {
            substanceName: sub,
            styleNames: sty,
            domainName: dom,
            id: trioName,
          },
          excludeWarnings,
        );
        // create out folder if it doesn't exist
        if (!fs.existsSync(options.out)) fs.mkdirSync(options.out);
        // write diagram to out folder
        const outputPath = join(options.out, `${trioName}.svg`);
        fs.writeFileSync(outputPath, diagram);
        console.log(
          chalk.green(`The diagram has been saved as ${resolve(outputPath)}`),
        );
        // TODO: print warning here
        for (const warning of state.warnings) {
          const warnStr = showError(warning);
          console.warn(chalk.yellow("Warning in diagram: " + warnStr));
        }
      }
    },
  )
  .command(
    "watch",
    "Watch the current folder for files & changes (must end in .substance, .style, .domain)",
    (yargs) =>
      yargs.option("port", {
        desc: "Port number for the WebSocket connection.",
        default: 9160,
        alias: "p",
      }),
    (options) => watch(+options.port),
  )

  .demandCommand()
  .strict()
  .help().argv;

//#endregion

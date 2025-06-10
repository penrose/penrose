import "global-jsdom/register";

import { entries, Trio } from "@penrose/examples";
import fs from "fs";
import yargs from "yargs";
import { compileTrio, removeStaging } from "../utils.js";
import { computeDiagramExploration } from "./statistics.js";
import { estimateSuccessRates } from "./success-rate.js";
import {
  BasicStagedOptimizer,
  ExteriorPointOptimizer,
  LBGFSOptimizer,
  LineSearchGDOptimizer,
  MultiStartStagedOptimizer, SimulatedAnnealing, StagedOptimizer
} from "../Optimizers.js";

// make sure to update CLI options if you add/remove optimizers
type OptimizerName =
  | "line-search-gd"
  | "lbfgs"
  | "multi-start-lbfgs"
  | "simulated-annealing-lbfgs";

const getOptimizer = (name: OptimizerName): StagedOptimizer => {
  switch (name) {
    case "line-search-gd":
      return new BasicStagedOptimizer(
        new ExteriorPointOptimizer(
          new LineSearchGDOptimizer()
        )
      );

    case "lbfgs":
      return new BasicStagedOptimizer(
        new ExteriorPointOptimizer(
          new LBGFSOptimizer()
        )
      );

    case "multi-start-lbfgs":
      return new MultiStartStagedOptimizer(
        () => new LBGFSOptimizer(),
        16
      );

    case "simulated-annealing-lbfgs":
      return new SimulatedAnnealing(
        new LBGFSOptimizer()
      );
  }
}

const maxSamplesPerDim = 50;
const totalMinutes = 1;
const totalMs = totalMinutes * 60 * 1000;
const maxMs = totalMs / entries.length;
console.log(`Max time per entry: ${maxMs} ms`);

const namesAndTrios = (
  await Promise.all(
    entries.map(async ([name, meta]) => {
      if (!meta.trio) return null;
      const trio = await meta.get();
      return [name, trio] as [string, Trio];
    }),
  )
).filter((x) => x !== null) as [string, Trio][];

const computeStatistics = async (
  argv: {
    outputDir: string;
  }
) => {
  let nextVariation = 0;
  const sampler = () => `${nextVariation++}`;

  for (const [name, trio] of namesAndTrios) {
    const state = await compileTrio(trio);
    if (!state) {
      console.error(`Failed to compile trio for ${name}`);
      continue;
    }

    const explorationInfo = computeDiagramExploration(
      removeStaging(state),
      sampler,
      maxSamplesPerDim,
      maxMs,
    );

    // save explorationInfo to output directory
    const outputFile = `${argv.outputDir}/${name}.json`;
    const outputFileDir = outputFile.split("/").slice(0, -1).join("/");
    fs.mkdirSync(outputFileDir, { recursive: true });
    fs.writeFileSync(outputFile, JSON.stringify(explorationInfo, null, 2));

    console.log(`Saved exploration info for ${name} to ${outputFile}`);
  }
};

const computeSuccessRates = async (
  argv: {
    outputDir: string;
    optimizer: OptimizerName;
  }
) => {
  const optimizer = getOptimizer(argv.optimizer);

  const successRates = await estimateSuccessRates(
    namesAndTrios,
    maxSamplesPerDim,
    maxMs,
    optimizer
  );

  // save success rates to output directory
  const outputFile = `${argv.outputDir}/success-rates.json`;
  fs.mkdirSync(argv.outputDir, { recursive: true });
  fs.writeFileSync(
    outputFile,
    JSON.stringify(Object.fromEntries(successRates), null, 2),
  );
};

yargs(process.argv.slice(2))
  .option("output-dir", {
    alias: "o",
    type: "string",
    description: "Directory to save output files",
    default: "collect-output",
  })
  .command(
    "statistics",
    "Compute statistics for each trio",
    () => {},
    (argv) => computeStatistics(argv as any)
  )
  .command(
    "success-rate <optimizer>",
    "Estimate success rates for each trio using the specified optimizer",
    (yargs) => {
      return yargs
        .positional("optimizer", {
          describe: "Name of the optimizer to use",
          type: "string",
          choices: [
            "line-search-gd",
            "lbfgs",
            "multi-start-lbfgs",
            "simulated-annealing-lbfgs"
          ],
        })
    },
    (argv) => computeSuccessRates(argv as any)
  )
  .demandCommand()
  .help()
  .parse();

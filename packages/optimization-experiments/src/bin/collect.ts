import "global-jsdom/register";

import { entries, Trio } from "@penrose/examples";
import fs from "fs";
import yargs from "yargs";
import {
  BasicStagedOptimizer,
  ExteriorPointOptimizer,
  LBGFSOptimizer,
  MultiStartStagedOptimizer, ParallelTempering, SimulatedAnnealing,
  StagedOptimizer
} from "../Optimizers.js";
import { compileTrio, removeStaging } from "../utils.js";
import { computeDiagramExploration } from "./statistics.js";
import { estimateSuccessRates } from "./success-rate.js";
import { AutoMALA } from "../samplers.js";

const defaultOutputDir = "collect-output";
const defaultNumSamples = 10;
const defaultTimeout = 30; // in seconds, per trio
const defaultSampleTimeout = Infinity; // in seconds, per sample

// make sure to update CLI options if you add/remove optimizers
type OptimizerName =
  | "lbfgs"
  | "multi-start-lbfgs"
  | "sa-automala"
  | "pt-automala"

const getOptimizer = (name: OptimizerName): StagedOptimizer => {
  switch (name) {
    case "lbfgs":
      return new BasicStagedOptimizer(
        new ExteriorPointOptimizer(new LBGFSOptimizer()),
      );

    case "multi-start-lbfgs":
      return new MultiStartStagedOptimizer(() => new LBGFSOptimizer(), 16);

    case "sa-automala": {
      const automala = new AutoMALA({
        initStepSize: 1.0,
        roundLength: 100,
        constraintWeight: 1000,
        maxStepSearches: 30,
      });
      return new BasicStagedOptimizer(
        new SimulatedAnnealing(automala, {
          initTemperature: 1000,
          coolingRate: 0.01,
          minTemperature: 0.1,
        }),
      );
    }

    case "pt-automala":
      return new BasicStagedOptimizer(
        new ParallelTempering(
          new ExteriorPointOptimizer(new LBGFSOptimizer()),
          () => new AutoMALA({
            initStepSize: 1.0,
            roundLength: 100,
            constraintWeight: 1000,
            maxStepSearches: 30,
          }),
          {
            maxTemperature: 1000,
            minTemperature: 1,
            temperatureRatio: 10,
          },
        )
      );
  }
};

const namesAndTrios = (
  await Promise.all(
    entries.map(async ([name, meta]) => {
      if (!meta.trio) return null;
      const trio = await meta.get();
      return [name, trio] as [string, Trio];
    }),
  )
).filter((x) => x !== null) as [string, Trio][];

const computeStatistics = async (argv: {
  outputDir: string;
  numSamples: number;
  timeout: number;
}) => {
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
      argv.numSamples,
      argv.timeout,
    );

    // save explorationInfo to output directory
    const outputFile = `${argv.outputDir}/${name}.json`;
    const outputFileDir = outputFile.split("/").slice(0, -1).join("/");
    fs.mkdirSync(outputFileDir, { recursive: true });
    fs.writeFileSync(outputFile, JSON.stringify(explorationInfo, null, 2));

    console.log(`Saved exploration info for ${name} to ${outputFile}`);
  }
};

const computeSuccessRates = async (argv: {
  outputDir: string;
  optimizer: OptimizerName;
  numSamples: number;
  timeout: number;
	sampleTimeout: number;
}) => {
  const optimizer = getOptimizer(argv.optimizer);

  const successRates = await estimateSuccessRates(
    namesAndTrios,
    optimizer,
    argv.numSamples,
    argv.timeout,
		argv.sampleTimeout
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
    default: defaultOutputDir,
  })
  .option("num-samples", {
    alias: "n",
    type: "number",
    description: "Number of samples per trio",
    default: defaultNumSamples,
  })
  .option("timeout", {
    alias: "t",
    type: "number",
    description: "Timeout in seconds for each trio",
    default: defaultTimeout,
  })
	.option("sample-timeout", {
		type: "number",
		description: "Timeout in seconds for each sample of a trio",
		default: defaultSampleTimeout,
	})
  .command(
    "statistics",
    "Compute statistics for each trio",
    () => {},
    (argv) => computeStatistics(argv as any),
  )
  .command(
    "success-rate <optimizer>",
    "Estimate success rates for each trio using the specified optimizer",
    (yargs) => {
      return yargs.positional("optimizer", {
        describe: "Name of the optimizer to use",
        type: "string",
        choices: [
          "lbfgs",
          "multi-start-lbfgs",
          "sa-automala",
          "pt-automala",
        ],
      });
    },
    (argv) => computeSuccessRates(argv as any),
  )
  .demandCommand()
  .help()
  .parse();

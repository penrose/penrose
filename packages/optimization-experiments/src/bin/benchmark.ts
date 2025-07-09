import "global-jsdom/register";

import fs from "fs";
import yargs from "yargs";
import {
  BasicStagedOptimizer,
  ExteriorPointOptimizer, FailedReason,
  LBGFSOptimizer,
  MultiStartStagedOptimizer, ParallelTempering, SimulatedAnnealing,
  StagedOptimizer
} from "../Optimizers.js";
import { compileTrio, getExampleNamesAndTrios } from "../utils.js";
import { AutoMALA } from "../samplers.js";
import cliProgress from "cli-progress";
import { resample } from "@penrose/core";

type OptimizerParams = {
  name: "lbfgs",
} | {
  name: "multi-start-lbfgs",
  numStarts: number,
} | {
  name: "sa-automala",
  initStepSize: number,
  roundLength: number,
  constraintWeight: number,
  maxStepSearches: number,
  initTemperature: number,
  coolingRate: number,
  minTemperature: number,
} | {
  name: "pt-automala",
  initStepSize: number,
  roundLength: number,
  constraintWeight: number,
  maxStepSearches: number,
  maxTemperature: number,
  minTemperature: number,
  temperatureRatio: number,
  maxStepsSinceLastChange: number,
}

type Options = {
  outputDir: string;
  numSamples: number;
  timeout: number; // in seconds, per trio
  sampleTimeout: number; // in seconds, per sample
  optimizer: OptimizerParams
}

const maxConstraintEnergy = 1e-1;

export interface BenchmarkResult {
  variation: string;
  samples: number;
  successes: number;
  badMinima: number;
  failures: number;
  time: number;
  sampleData: SampleData[];
}

export interface SampleData {
  time: number; // includes initialization, so time / steps =/= step time
  steps: number;
  stepTimeMean: number;
  stepTimeStdDev: number;
  objectiveSum: number | null;
  constraintSum: number | null;
  finalInputs: number[] | null;
}

const getOptimizer = (params: OptimizerParams): StagedOptimizer => {
  switch (params.name) {
    case "lbfgs":
      return new BasicStagedOptimizer(
        new ExteriorPointOptimizer(new LBGFSOptimizer()),
      );

    case "multi-start-lbfgs":
      return new MultiStartStagedOptimizer(() => new LBGFSOptimizer(), params.numStarts);

    case "sa-automala": {
      const automala = new AutoMALA(params);
      return new BasicStagedOptimizer(
        new SimulatedAnnealing(automala, params),
      );
    }

    case "pt-automala":
      return new BasicStagedOptimizer(
        new ParallelTempering(
          new ExteriorPointOptimizer(new LBGFSOptimizer()),
          () => new AutoMALA(params),
          params
        )
      );

    default:
      throw new Error(`Unknown optimizer: ${(params as OptimizerParams).name}`);
  }
};


const benchmark = async (options: Options) => {
  const namesAndTrios = await getExampleNamesAndTrios();
  const optimizer = getOptimizer(options.optimizer);

  const multibar = new cliProgress.MultiBar(
    {
      clearOnComplete: true,
      format:
        " {bar} | {name} | {value}/{total} | Sampling {sampleCount}/{totalSamples} | Elapsed: {duration_formatted} | ETA: {eta_formatted}",
    },
    cliProgress.Presets.shades_grey,
  );

  const results: Map<string, BenchmarkResult | null> = new Map();

  const trioBar = multibar.create(namesAndTrios.length, 0);

  let trioCount = 0;

  for (const [name, trio] of namesAndTrios) {
    // console.log("---------------");
    // console.log(`Estimating success rates for ${name}`);
    trioBar.update(trioCount, { name, sampleCount: 0, totalSamples: options.numSamples });

    let state = await compileTrio(trio);
    const startTime = performance.now();

    if (!state) {
      // console.error(`Failed to compile trio for ${name}`);
      results.set(name, null);
      continue;
    }

    let nextSampleNum = 0;
    const sampler = () => `${nextSampleNum++}`;

    if (performance.now() - startTime > options.timeout * 1000) {
      // console.warn(`$Timed out after 0 samples.`);
      results.set(name, null);
      continue;
    }

    let takenSamples = 0;
    let numSuccesses = 0;
    let numBadMinima = 0;
    let numFailures = 0;

    const sampleDataArr = [];

    for (let i = 0; i < options.numSamples; i++) {
      trioBar.update(trioCount, { name, sampleCount: i + 1, totalSamples: options.numSamples });
      trioBar.render();

      if (performance.now() - startTime > options.timeout * 1000) {
        // console.warn(`Timed out after ${i} samples.`);
        break;
      }

      let timedout = false;

      const sampleStartTime = performance.now();

      optimizer.init(
        state,
        Math.min(
          options.sampleTimeout * 1000,
          options.timeout * 1000 - (performance.now() - startTime)));
      state.variation = sampler();
      state.currentStageIndex = 0;
      state = resample(state);

      let stepTimeMean = 0;
      let stepTimeM2 = 0;

      let constraintSum: number | null = null;
      let objectiveSum: number | null = null;
      let finalInputs: number[] | null = null;

      let shouldStop = false;
      let numSteps = 0;
      while (!shouldStop) {
        const stepStartTime = performance.now();
        if (stepStartTime - startTime > options.timeout * 1000) {
          // console.warn(`Timed out after ${i} samples.`);
          timedout = true;
          break;
        }

        try {
          const result = optimizer.step(state);

          numSteps++;
          const stepEndTime = performance.now();
          const stepTime = stepEndTime - stepStartTime;

          // update step time statistics
          const timeDelta1 = stepTime - stepTimeMean;
          stepTimeMean += timeDelta1 / numSteps;
          const timeDelta2 = stepTime - stepTimeMean;
          stepTimeM2 += timeDelta1 * timeDelta2;

          switch (result.tag) {
            case "Converged":
              if (state.currentStageIndex === state.optStages.length - 1) {
                constraintSum = result.outputs.constraints
                  .map((c) => Math.max(0, c) ** 2)
                  .reduce((acc, p) => acc + p, 0);
                if (constraintSum < maxConstraintEnergy) {
                  numSuccesses++;
                } else {
                  numBadMinima++;
                }

                objectiveSum = result.outputs.objectives
                  .reduce((acc, o) => acc + o, 0);

                finalInputs = [...state.varyingValues];

                shouldStop = true;
              } else {
                // go to next stage
                state.currentStageIndex++;
                optimizer.init(state);
              }
              break;

            case "Unconverged":
              // continue optimizing
              break;

            case "Failed":
              // console.error(`Error during optimization step: ${result.reason}`);
              if (result.reason === FailedReason.Timeout) {
                timedout = true;
              } else {
                numFailures++;
              }
              shouldStop = true;
              break;
          }
        } catch (error) {
          console.error(`Error during optimization step: ${error}`);
          numFailures++;
          shouldStop = true;
        }
      }

      if (timedout) break;

      takenSamples++;
      sampleDataArr.push({
        time: performance.now() - sampleStartTime,
        steps: numSteps,
        stepTimeMean,
        stepTimeStdDev:
          numSteps > 1 ? Math.sqrt(stepTimeM2 / (numSteps - 1)) : 0,
        objectiveSum,
        constraintSum,
        finalInputs,
      });
    }

    const result: BenchmarkResult = {
      variation: state.variation,
      samples: takenSamples,
      successes: numSuccesses,
      badMinima: numBadMinima,
      failures: numFailures,
      sampleData: sampleDataArr,
      time: performance.now() - startTime,
    };

    console.log(`\nResults for ${name} (abridged):`);
    console.log(`  Samples: ${result.samples}`);
    console.log(`  Successes: ${result.successes}`);
    console.log(`  Bad minima: ${result.badMinima}`);
    console.log(`  Failures: ${result.failures}`);

    results.set(name, result);
    trioCount++;
  }
  multibar.stop();

  return results;
}

yargs(process.argv.slice(2))
  .command("$0 <options>",
    "Run the benchmark with the given options",
    (yargs) => {
      return yargs
        .positional("options", {
          describe: "Path to options json file",
          type: "string",
        }
      );
    },
    async (argv) => {
      const optionsPath = argv.options;
      if (optionsPath === undefined) {
        throw new Error("Please provide an options file with");
      }
      const optionsFile = fs.readFileSync(optionsPath, "utf-8");
      const options = JSON.parse(optionsFile);

      const results = await benchmark(options);

      const date = new Date();
      const outputFile = `${options.outputDir}/benchmark-${date.toISOString()}.json`;
      fs.mkdirSync(options.outputDir, { recursive: true });
      fs.writeFileSync(
        outputFile,
        JSON.stringify(Object.fromEntries(results), null, 2),
      );
    }
  )
  .help()
  .parse();



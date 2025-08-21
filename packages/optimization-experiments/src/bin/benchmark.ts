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
import {
  compileTrio, diagramCompGraphEdges,
  diagramStdDev,
  getExampleNamesAndTrios
} from "../utils.js";
import { AutoMALA } from "../samplers.js";
import cliProgress from "cli-progress";
import { OptOutputs, resample, Shape } from "@penrose/core";
import { fileURLToPath } from "url";

export type OptimizerParams = {
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
  minTemperature: number,
  maxTemperature: number,
  temperatureRatio: number,
  epStop: number,
  uoStop: number,
  uoStopSteps: number,
  initConstraintWeight: number,
  constraintWeightGrowthFactor: number,
  acceptableConstraintEnergy: number,
  finalUoStop: number,
  finalUoStopSteps: number
}

type Options = {
  outputDir: string;
  outputName?: string;
  numSamples: number;
  timeout: number; // in seconds, per trio
  sampleTimeout: number; // in seconds, per sample
  optimizer: OptimizerParams
}

export const maxConstraintEnergy = 1e-1;

export interface BenchmarkResult {
  variation: string;
  samples: number;
  successes: number;
  badMinima: number;
  timeouts: number;
  failures: number;
  time: number;
  diagramStdDev: Record<string, number>;
  compGraphEdges: number;
  sampleData: SampleData[];
}

export interface SampleData {
  time: number; // includes initialization, so time / steps =/= step time
  trio_timedout: boolean;
  sample_timedout: boolean;
  steps: number;
  stepTimeMean: number;
  stepTimeStdDev: number;
  objectiveSum: number | null;
  constraintSum: number | null;
  finalInputs: number[] | null;
}

export const getOptimizer = (params: OptimizerParams): StagedOptimizer => {
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
          (constraintWeight) => new AutoMALA({
            ...params,
            constraintWeight
          }),
          params
        )
      );

    default:
      throw new Error(`Unknown optimizer: ${(params as OptimizerParams).name}`);
  }
};


const benchmark = async (options: Options, test = false) => {
  let namesAndTrios = await getExampleNamesAndTrios();
  if (test)  {
    namesAndTrios = namesAndTrios.slice(0, 10);
  }

  const optimizer = getOptimizer(options.optimizer);

  const multibar = new cliProgress.MultiBar(
    {
      clearOnComplete: true,
      format:
        " {bar} | {name} | {value}/{total} | Sampling {sampleCount}/{totalSamples} | Elapsed: {duration_formatted} | ETA: {eta_formatted}",
    },
    cliProgress.Presets.shades_grey,
  );

  const results: Map<string, BenchmarkResult> = new Map();

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

    let takenSamples = 0;
    let numSuccesses = 0;
    let numBadMinima = 0;
    let numFailures = 0;
    let numTimeouts = 0;

    const sampleDataArr: SampleData[] = [];
    const shapeLists: Shape<number>[][] = [];

    for (let i = 0; i < options.numSamples; i++) {
      trioBar.update(trioCount, { name, sampleCount: i + 1, totalSamples: options.numSamples });
      trioBar.render();

      let trio_timedout = false;
      let sample_timedout = false;

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

      let shouldStop = false;
      let numSteps = 0;
      let lastOptOutputs: OptOutputs | null = null;
      while (!shouldStop) {
        const stepStartTime = performance.now();
        if (stepStartTime - startTime > options.timeout * 1000) {
          // console.warn(`Timed out after ${i} samples.`);
          trio_timedout = true;
          break;
        }

        try {
          const result = optimizer.step(state);

          const stepEndTime = performance.now();
          const stepTime = stepEndTime - stepStartTime;

          // update step time statistics
          const timeDelta1 = stepTime - stepTimeMean;
          stepTimeMean += timeDelta1 / (numSteps + 1);
          const timeDelta2 = stepTime - stepTimeMean;
          stepTimeM2 += timeDelta1 * timeDelta2;

          switch (result.tag) {
            case "Converged":
              if (state.currentStageIndex === state.optStages.length - 1) {
                shouldStop = true;
              } else {
                // go to next stage
                state.currentStageIndex++;
                optimizer.init(state);
              }
              lastOptOutputs = result.outputs;
              numSteps++;
              break;

            case "Unconverged":
              // continue optimizing
              numSteps++;
              lastOptOutputs = result.outputs;
              break;

            case "Failed":
              // console.error(`Error during optimization step: ${result.reason}`);
              if (result.reason === FailedReason.Timeout) {
                if (performance.now() - startTime > options.timeout * 1000) {
                  trio_timedout = true;
                } else {
                  sample_timedout = true;
                }
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

      takenSamples++;

      let constraintSum = lastOptOutputs?.constraints
        .map((c) => Math.max(0, c) ** 2)
        .reduce((acc, p) => acc + p, 0)
        ?? null;

      let objectiveSum = lastOptOutputs?.objectives
        .reduce((acc, o) => acc + o, 0)
        ?? null;

      let finalInputs = [...state.varyingValues];

      const shapes = state.computeShapes(state.varyingValues)
      shapeLists.push(shapes);

      sampleDataArr.push({
        time: performance.now() - sampleStartTime,
        trio_timedout,
        sample_timedout,
        steps: numSteps,
        stepTimeMean:
          numSteps > 0 ?
            stepTimeMean : null,
        stepTimeStdDev:
          numSteps > 1 ?
            Math.sqrt(stepTimeM2 / (numSteps - 1))
            : numSteps === 1 ? 0 : null,
        objectiveSum,
        constraintSum,
        finalInputs,
      });

      if (trio_timedout) {
        numTimeouts++;
        break;
      } else if (sample_timedout) {
        numTimeouts++;
      } else if (constraintSum! < maxConstraintEnergy) {
        numSuccesses++;
      } else {
        numBadMinima++;
      }
    }

    const time = performance.now() - startTime;
    const stdDev = diagramStdDev(shapeLists);

    const result: BenchmarkResult = {
      variation: state.variation,
      samples: takenSamples,
      successes: numSuccesses,
      badMinima: numBadMinima,
      failures: numFailures,
      timeouts: numTimeouts,
      time,
      diagramStdDev: stdDev,
      compGraphEdges: diagramCompGraphEdges(state),
      sampleData: sampleDataArr,
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

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  yargs(process.argv.slice(2))
    .command("$0 <options>",
      "Run the benchmark with the given options",
      (yargs) => {
        return yargs
          .positional("options", {
            describe: "Path to options json file",
            type: "string",
          })
          .option("test", {
            describe: "Run a test on 10 diagrams",
            type: "boolean",
            default: false,
          })
      },
      async (argv) => {
        const optionsPath = argv.options;
        if (optionsPath === undefined) {
          throw new Error("Please provide an options file with");
        }
        const optionsFile = fs.readFileSync(optionsPath, "utf-8");
        const options = JSON.parse(optionsFile);

        if (argv.test) {
          console.log("Running test benchmark with 10 diagrams");
        }

        const results = await benchmark(options, argv.test);

        const date = new Date();
        const outputName = options.outputName || "benchmark";
        const outputFile = `${options.outputDir}/${outputName}-${date.toISOString()}.json`;
        fs.mkdirSync(options.outputDir, { recursive: true });
        fs.writeFileSync(
          outputFile,
          JSON.stringify(Object.fromEntries(results), null, 2),
        );
      }
    )
    .help()
    .parse();
}

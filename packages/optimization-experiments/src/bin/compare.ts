import "global-jsdom/register";

import fs from "fs";
import yargs from "yargs";
import cliProgress from "cli-progress";
import { OptOutputs, resample } from "@penrose/core";
import { FailedReason, StagedOptimizer } from "../Optimizers.js";
import { compileTrio, getExampleNamesAndTrios } from "../utils.js";
import { OptimizerParams, getOptimizer, maxConstraintEnergy } from "./benchmark.js";
import { fileURLToPath } from "url";

type CompareOptions = {
  outputDir: string;
  outputName?: string;
  numComparisons: number;
  timeout: number; // in seconds, per trio
  optimizer1: OptimizerParams;
  optimizer2: OptimizerParams;
}

export interface ComparisonResult {
  trio: string;
  optimizer1Time: number | null;
  optimizer2Time: number | null;
  optimizer1Steps: number | null;
  optimizer2Steps: number | null;
  optimizer1ObjectiveSum: number | null;
  optimizer2ObjectiveSum: number | null;
  optimizer1BadMinima: boolean | null;
  optimizer2BadMinima: boolean | null;
  proportionalTimeChange: number | null; // (opt2 - opt1) / opt1
  proportionalStepsChange: number | null;
  proportionalObjectiveChange: number | null;
  optimizer1Success: boolean;
  optimizer2Success: boolean;
}

export interface OptimizationResult {
  time: number | null;
  steps: number | null;
  objectiveSum: number | null;
  badMinima: boolean | null;
  success: boolean;
}

export interface CompareResults {
  comparisons: ComparisonResult[];
  averageProportionalTimeChange: number | null;
  averageProportionalStepsChange: number | null;
  averageProportionalObjectiveChange: number | null;
  averageProportionalBadMinimaChange: number | null;
  averageProportionalSuccessChange: number | null;
  totalComparisons: number;
  validTimeComparisons: number;
  validStepsComparisons: number;
  validObjectiveComparisons: number;
  validBadMinimaComparisons: number;
  validSuccessComparisons: number;
}

const runOptimization = async (
  trio: [string, string, string], 
  optimizer: StagedOptimizer, 
  timeout: number,
): Promise<OptimizationResult> => {
  let state = await compileTrio(trio);
  
  if (!state) {
    return { time: null, steps: null, objectiveSum: null, badMinima: null, success: false };
  }

  state.variation = "0";
  state.currentStageIndex = 0;
  state = resample(state);

  const startTime = performance.now();
  
  optimizer.init(state, timeout * 1000);

  let shouldStop = false;
  let numSteps = 0;
  let lastOptOutputs: OptOutputs | null = null;
  let timedOut = false;

  while (!shouldStop) {
    if (performance.now() - startTime > timeout * 1000) {
      timedOut = true;
      break;
    }

    try {
      const result = optimizer.step(state);

      switch (result.tag) {
        case "Converged":
          if (state.currentStageIndex === state.optStages.length - 1) {
            shouldStop = true;
          } else {
            state.currentStageIndex++;
            optimizer.init(state);
          }
          lastOptOutputs = result.outputs;
          numSteps++;
          break;

        case "Unconverged":
          numSteps++;
          lastOptOutputs = result.outputs;
          break;

        case "Failed":
          if (result.reason === FailedReason.Timeout) {
            timedOut = true;
          } else {
            console.log(`Optimization failed: ${result.reason}`);
          }
          shouldStop = true;
          break;
      }
    } catch (error: any) {
      console.log(`Error during optimization step: ${error.message}`);
      shouldStop = true;
      break;
    }
  }

  const time = performance.now() - startTime;
  
  if (timedOut || !lastOptOutputs) {
    return { time: null, steps: null, objectiveSum: null, badMinima: null, success: false };
  }

  const constraintSum = lastOptOutputs.constraints
    .map((c) => Math.max(0, c) ** 2)
    .reduce((acc, p) => acc + p, 0);

  const objectiveSum = lastOptOutputs.objectives
    .reduce((acc, o) => acc + o, 0);

  const badMinima = constraintSum >= maxConstraintEnergy;
  console.log(`Constraint sum: ${constraintSum}, Objective sum: ${objectiveSum}, Bad minima: ${badMinima}`);
  
  return { time, steps: numSteps, objectiveSum, badMinima, success: !badMinima };
};

const compare = async (options: CompareOptions) => {
  const namesAndTrios = await getExampleNamesAndTrios();

  if (namesAndTrios.length === 0) {
    throw new Error("No trios found in examples registry");
  }

  const optimizer1 = getOptimizer(options.optimizer1);
  const optimizer2 = getOptimizer(options.optimizer2);

  const progressBar = new cliProgress.SingleBar(
    {
      format: " {bar} | {value}/{total} | Elapsed: {duration_formatted} | ETA: {eta_formatted}",
    },
    cliProgress.Presets.shades_grey,
  );

  progressBar.start(options.numComparisons, 0);

  const results: ComparisonResult[] = [];

  for (let i = 0; i < options.numComparisons; i++) {
    // Randomly select a trio
    const randomIndex = Math.floor(Math.random() * namesAndTrios.length);
    const [name, trio] = namesAndTrios[randomIndex];

    console.log(`\nComparison ${i + 1}: ${name}`);
    console.log(`  Trio: ${name}`);

    // Run optimizer 1
    const result1 = await runOptimization(trio, optimizer1, options.timeout);
    
    // Run optimizer 2
    const result2 = await runOptimization(trio, optimizer2, options.timeout);

    console.log(`  ${options.optimizer1.name} - Time: ${result1.time?.toFixed(2) ?? "N/A"} ms, Steps: ${result1.steps ?? "N/A"}, Objective Sum: ${result1.objectiveSum?.toFixed(2) ?? "N/A"}, Bad Minima: ${result1.badMinima ?? "N/A"}`);
    console.log(`  ${options.optimizer2.name} - Time: ${result2.time?.toFixed(2) ?? "N/A"} ms, Steps: ${result2.steps ?? "N/A"}, Objective Sum: ${result2.objectiveSum?.toFixed(2) ?? "N/A"}, Bad Minima: ${result2.badMinima ?? "N/A"}`);


    // Calculate proportional changes
    let proportionalTimeChange: number | null = null;
    if (result1.time !== null && result2.time !== null && result1.time > 0) {
      proportionalTimeChange = (result2.time - result1.time) / result1.time;
    }

    let proportionalStepsChange: number | null = null;
    if (result1.steps !== null && result2.steps !== null && result1.steps > 0) {
      proportionalStepsChange = (result2.steps - result1.steps) / result1.steps;
    }

    let proportionalObjectiveChange: number | null = null;
    if (result1.objectiveSum !== null && result2.objectiveSum !== null && result1.objectiveSum !== 0) {
      proportionalObjectiveChange = (result2.objectiveSum - result1.objectiveSum) / Math.abs(result1.objectiveSum);
    }

    const comparisonResult: ComparisonResult = {
      trio: name,
      optimizer1Time: result1.time,
      optimizer2Time: result2.time,
      optimizer1Steps: result1.steps,
      optimizer2Steps: result2.steps,
      optimizer1ObjectiveSum: result1.objectiveSum,
      optimizer2ObjectiveSum: result2.objectiveSum,
      optimizer1BadMinima: result1.badMinima,
      optimizer2BadMinima: result2.badMinima,
      proportionalTimeChange,
      proportionalStepsChange,
      proportionalObjectiveChange,
      optimizer1Success: result1.success,
      optimizer2Success: result2.success,
    };

    results.push(comparisonResult);
    progressBar.update(i + 1);
  }

  progressBar.stop();

  // Calculate summary statistics
  const validTimeComparisons = results.filter(r => r.proportionalTimeChange !== null);
  const averageProportionalTimeChange = validTimeComparisons.length > 0
    ? validTimeComparisons.reduce((sum, r) => sum + r.proportionalTimeChange!, 0) / validTimeComparisons.length
    : null;

  const validStepsComparisons = results.filter(r => r.proportionalStepsChange !== null);
  const averageProportionalStepsChange = validStepsComparisons.length > 0
    ? validStepsComparisons.reduce((sum, r) => sum + r.proportionalStepsChange!, 0) / validStepsComparisons.length
    : null;

  const validObjectiveComparisons = results.filter(r => r.proportionalObjectiveChange !== null);
  const averageProportionalObjectiveChange = validObjectiveComparisons.length > 0
    ? validObjectiveComparisons.reduce((sum, r) => sum + r.proportionalObjectiveChange!, 0) / validObjectiveComparisons.length
    : null;

  // Calculate proportional change in bad minima rate
  const validBadMinimaComparisons = results.filter(r =>
    r.optimizer1BadMinima !== null && r.optimizer2BadMinima !== null
  );

  const validSuccessComparisons = validBadMinimaComparisons.filter(r =>
    r.optimizer1Success && r.optimizer2Success
  );

  let averageProportionalBadMinimaChange: number | null = null;
  if (validBadMinimaComparisons.length > 0) {
    const opt1BadMinimaRate = validBadMinimaComparisons.filter(r => r.optimizer1BadMinima).length / validBadMinimaComparisons.length;
    const opt2BadMinimaRate = validBadMinimaComparisons.filter(r => r.optimizer2BadMinima).length / validBadMinimaComparisons.length;
    
    if (opt1BadMinimaRate > 0) {
      averageProportionalBadMinimaChange = (opt2BadMinimaRate - opt1BadMinimaRate) / opt1BadMinimaRate;
    } else if (opt2BadMinimaRate > 0) {
      averageProportionalBadMinimaChange = Infinity; // From 0 bad minima to some bad minima
    } else {
      averageProportionalBadMinimaChange = 0; // Both have 0 bad minima
    }
  }

  let averageProportionalSuccessChange: number | null = null;
  const opt1SuccessRate = validSuccessComparisons.filter(r => r.optimizer1Success).length / validSuccessComparisons.length;
  const opt2SuccessRate = validSuccessComparisons.filter(r => r.optimizer2Success).length / validSuccessComparisons.length;

  if (opt1SuccessRate > 0) {
    averageProportionalSuccessChange = (opt2SuccessRate - opt1SuccessRate) / opt1SuccessRate;
  } else if (opt2SuccessRate > 0) {
    averageProportionalSuccessChange = Infinity; // From 0 success to some success
  } else {
    averageProportionalSuccessChange = 0; // Both have 0 success
  }

  const compareResults: CompareResults = {
    comparisons: results,
    averageProportionalTimeChange,
    averageProportionalStepsChange,
    averageProportionalObjectiveChange,
    averageProportionalBadMinimaChange,
    averageProportionalSuccessChange,
    totalComparisons: options.numComparisons,
    validTimeComparisons: validTimeComparisons.length,
    validStepsComparisons: validStepsComparisons.length,
    validObjectiveComparisons: validObjectiveComparisons.length,
    validBadMinimaComparisons: validBadMinimaComparisons.length,
    validSuccessComparisons: validSuccessComparisons.length,
  };

  return compareResults;
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  yargs(process.argv.slice(2))
    .command("$0 <options>",
      "Compare two optimization strategies",
      (yargs) => {
        return yargs
          .positional("options", {
            describe: "Path to comparison options json file",
            type: "string",
          })
      },
      async (argv) => {
        const optionsPath = argv.options;
        if (optionsPath === undefined) {
          throw new Error("Please provide an options file");
        }
        const optionsFile = fs.readFileSync(optionsPath, "utf-8");
        const options = JSON.parse(optionsFile) as CompareOptions;

        console.log(`Comparing ${options.optimizer1.name} vs ${options.optimizer2.name}`);
        console.log(`Running ${options.numComparisons} comparisons...`);

        const results = await compare(options);

        // Print summary
        console.log("\n=== COMPARISON RESULTS ===");
        console.log(`Total comparisons: ${results.totalComparisons}`);
        console.log(`Valid time comparisons: ${results.validTimeComparisons}`);
        console.log(`Valid steps comparisons: ${results.validStepsComparisons}`);
        console.log(`Valid objective comparisons: ${results.validObjectiveComparisons}`);
        console.log(`Valid bad minima comparisons: ${results.validBadMinimaComparisons}`);

        if (results.averageProportionalTimeChange !== null) {
          console.log(`Average proportional time change: ${(results.averageProportionalTimeChange * 100).toFixed(2)}%`);
          if (results.averageProportionalTimeChange > 0) {
            console.log(`  -> Optimizer 2 is ${(results.averageProportionalTimeChange * 100).toFixed(2)}% slower on average`);
          } else {
            console.log(`  -> Optimizer 2 is ${(-results.averageProportionalTimeChange * 100).toFixed(2)}% faster on average`);
          }
        } else {
          console.log("Average proportional time change: N/A (no valid comparisons)");
        }

        if (results.averageProportionalStepsChange !== null) {
          console.log(`Average proportional steps change: ${(results.averageProportionalStepsChange * 100).toFixed(2)}%`);
          if (results.averageProportionalStepsChange > 0) {
            console.log(`  -> Optimizer 2 takes ${(results.averageProportionalStepsChange * 100).toFixed(2)}% more steps on average`);
          } else {
            console.log(`  -> Optimizer 2 takes ${(-results.averageProportionalStepsChange * 100).toFixed(2)}% fewer steps on average`);
          }
        } else {
          console.log("Average proportional steps change: N/A (no valid comparisons)");
        }

        if (results.averageProportionalObjectiveChange !== null) {
          console.log(`Average proportional objective change: ${(results.averageProportionalObjectiveChange * 100).toFixed(2)}%`);
          if (results.averageProportionalObjectiveChange > 0) {
            console.log(`  -> Optimizer 2 has ${(results.averageProportionalObjectiveChange * 100).toFixed(2)}% higher objective sum on average`);
          } else {
            console.log(`  -> Optimizer 2 has ${(-results.averageProportionalObjectiveChange * 100).toFixed(2)}% lower objective sum on average`);
          }
        } else {
          console.log("Average proportional objective change: N/A (no valid comparisons)");
        }

        if (results.averageProportionalBadMinimaChange !== null) {
          if (results.averageProportionalBadMinimaChange === Infinity) {
            console.log("Average proportional bad minima change: ∞ (optimizer 1 had no bad minima, optimizer 2 had some)");
          } else {
            console.log(`Average proportional bad minima change: ${(results.averageProportionalBadMinimaChange * 100).toFixed(2)}%`);
            if (results.averageProportionalBadMinimaChange > 0) {
              console.log(`  -> Optimizer 2 has ${(results.averageProportionalBadMinimaChange * 100).toFixed(2)}% more bad minima on average`);
            } else {
              console.log(`  -> Optimizer 2 has ${(-results.averageProportionalBadMinimaChange * 100).toFixed(2)}% fewer bad minima on average`);
            }
          }
        } else {
          console.log("Average proportional bad minima change: N/A (no valid comparisons)");
        }

        if (results.averageProportionalSuccessChange !== null) {
          if (results.averageProportionalSuccessChange === Infinity) {
            console.log("Average proportional success change: ∞ (optimizer 1 had no successes, optimizer 2 had some)");
          } else {
            console.log(`Average proportional success change: ${(results.averageProportionalSuccessChange * 100).toFixed(2)}%`);
            if (results.averageProportionalSuccessChange > 0) {
              console.log(`  -> Optimizer 2 has ${(results.averageProportionalSuccessChange * 100).toFixed(2)}% more successes on average`);
            } else {
              console.log(`  -> Optimizer 2 has ${(-results.averageProportionalSuccessChange * 100).toFixed(2)}% fewer successes on average`);
            }
          }
        } else {
          console.log("Average proportional success change: N/A (no valid comparisons)");
        }

        // Save results
        const date = new Date();
        const outputName = options.outputName || "comparison";
        const outputFile = `${options.outputDir}/${outputName}-${date.toISOString()}.json`;
        fs.mkdirSync(options.outputDir, { recursive: true });
        fs.writeFileSync(
          outputFile,
          JSON.stringify(results, null, 2),
        );

        console.log(`\nResults saved to: ${outputFile}`);
      }
    )
    .help()
    .parse();
}

import { resample } from "@penrose/core";
import { Trio } from "@penrose/examples";
import cliProgress from "cli-progress";
import { StagedOptimizer, FailedReason } from "../Optimizers.js";
import { compileTrio } from "../utils.js";

const maxConstraintEnergy = 1e-1;

export interface SuccessRateResult {
  variation: string;
  samples: number;
  successes: number;
  badMinima: number;
  failures: number;
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

export const estimateSuccessRates = async (
  namesAndTrios: [string, Trio][],
  optimizer: StagedOptimizer,
  numSamples: number,
  timeout: number,
	sampleTimeout: number,
): Promise<Map<string, SuccessRateResult | null>> => {
  const multibar = new cliProgress.MultiBar(
    {
      clearOnComplete: true,
      format:
        " {bar} | {name} | {value}/{total} | Sampling {sampleCount}/{totalSamples} | Elapsed: {duration_formatted} | ETA: {eta_formatted}",
    },
    cliProgress.Presets.shades_grey,
  );

  const results: Map<string, SuccessRateResult | null> = new Map();

  const trioBar = multibar.create(namesAndTrios.length, 0);

  let trioCount = 0;

  for (const [name, trio] of namesAndTrios) {
    // console.log("---------------");
    // console.log(`Estimating success rates for ${name}`);
    trioBar.update(trioCount, { name, sampleCount: 0, totalSamples: numSamples });

    const startTime = performance.now();
    let state = await compileTrio(trio);

    if (!state) {
      // console.error(`Failed to compile trio for ${name}`);
      results.set(name, null);
      continue;
    }

    let nextSampleNum = 0;
    const sampler = () => `${nextSampleNum++}`;

    if (performance.now() - startTime > timeout * 1000) {
      // console.warn(`$Timed out after 0 samples.`);
      results.set(name, null);
      continue;
    }

    let takenSamples = 0;
    let numSuccesses = 0;
    let numBadMinima = 0;
    let numFailures = 0;

    const sampleDataArr = [];

    for (let i = 0; i < numSamples; i++) {
			trioBar.update(trioCount, { name, sampleCount: i + 1, totalSamples: numSamples });
			trioBar.render();

      if (performance.now() - startTime > timeout * 1000) {
        // console.warn(`Timed out after ${i} samples.`);
        break;
      }

      let timedout = false;

      const sampleStartTime = performance.now();

      optimizer.init(
        state,
        Math.min(
          sampleTimeout * 1000,
          timeout * 1000 - (performance.now() - startTime)));
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
        if (stepStartTime - startTime > timeout * 1000) {
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

    const result: SuccessRateResult = {
      variation: state.variation,
      samples: takenSamples,
      successes: numSuccesses,
      badMinima: numBadMinima,
      failures: numFailures,
      sampleData: sampleDataArr
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
};

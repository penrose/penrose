import { resample } from "@penrose/core";
import { Trio } from "@penrose/examples";
import { StagedOptimizer } from "../Optimizers.js";
import { compileTrio } from "../utils.js";

const maxConstraintEnergy = 1e-1;

export interface SuccessRateResult {
  samples: number;
  successes: number;
  badMinima: number;
  failures: number;
}

export const estimateSuccessRates = async (
  namesAndTrios: [string, Trio][],
  maxSamplesPerDim: number,
  maxMsPerTrio: number,
  optimizer: StagedOptimizer,
): Promise<Map<string, SuccessRateResult>> => {
  const results: Map<string, SuccessRateResult> = new Map();

  for (const [name, trio] of namesAndTrios) {
    console.log("---------------")
    console.log(`Estimating success rates for ${name}`);

    const startTime = performance.now();
    let state = await compileTrio(trio);

    if (!state) {
      console.error(`Failed to compile trio for ${name}`);
      results.set(name, {
        samples: 0,
        successes: 0,
        badMinima: 0,
        failures: 0,
      });
      continue;
    }

    let nextSampleNum = 0;
    const sampler = () => `${nextSampleNum++}`;

    if (performance.now() - startTime > maxMsPerTrio) {
      console.warn(`Stopping ${name} after 0 samples due to time limit`);
      results.set(name, {
        samples: 0,
        successes: 0,
        badMinima: 0,
        failures: 0,
      });
      continue;
    }

    let numSamples = 0;
    let numSuccesses = 0;
    let numBadMinima = 0;
    let numFailures = 0;

    for (let i = 0; i < maxSamplesPerDim * state.inputs.length; i++) {
      if (performance.now() - startTime > maxMsPerTrio) {
        console.log(`Stopping ${name} after ${i} samples due to time limit`);
        break;
      }

      let timedout = false;

      optimizer.init(state);
      state.variation = sampler();
      state.currentStageIndex = 0;
      state = resample(state);

      let shouldStop = false;
      while (!shouldStop) {
        if (performance.now() - startTime > maxMsPerTrio) {
          console.log(`Stopping ${name} after ${i} samples due to time limit`);
          timedout = true;
          break;
        }

        const result = optimizer.step(state);
        switch (result.tag) {
          case "Converged":
            if (state.currentStageIndex === state.optStages.length - 1) {
              const constraintSum = result.outputs.constraints
                .map((c) => Math.max(0, c) * Math.max(0, c))
                .reduce((acc, p) => acc + p, 0);
              if (constraintSum < maxConstraintEnergy) {
                numSuccesses++;
              } else {
                numBadMinima++;
              }
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
            numFailures++;
            shouldStop = true;
            break;
        }
      }

      if (timedout) break;

      numSamples++;
    }

    const result = {
      samples: numSamples,
      successes: numSuccesses,
      badMinima: numBadMinima,
      failures: numFailures,
    };

    console.log(`Results for ${name}:`);
    console.log(`  Samples: ${result.samples}`);
    console.log(`  Successes: ${result.successes}`);
    console.log(`  Bad minima: ${result.badMinima}`);
    console.log(`  Failures: ${result.failures}`);

    results.set(name, result);
  }

  return results;
};

import { resample } from "@penrose/core";
import { Trio } from "@penrose/examples";
import cliProgress from "cli-progress";
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
  optimizer: StagedOptimizer,
  numSamples: number,
  timeout: number,
	stepTimeout: number,
): Promise<Map<string, SuccessRateResult>> => {
  const multibar = new cliProgress.MultiBar(
    {
      clearOnComplete: true,
      format:
        " {bar} | {name} | {value}/{total} | Sampling {sampleCount}/{totalSamples} | Elapsed: {duration_formatted} | ETA: {eta_formatted}",
    },
    cliProgress.Presets.shades_grey,
  );

  const results: Map<string, SuccessRateResult> = new Map();

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

    if (performance.now() - startTime > timeout * 1000) {
      // console.warn(`$Timed out after 0 samples.`);
      results.set(name, {
        samples: 0,
        successes: 0,
        badMinima: 0,
        failures: 0,
      });
      continue;
    }

    let takenSamples = 0;
    let numSuccesses = 0;
    let numBadMinima = 0;
    let numFailures = 0;

    for (let i = 0; i < numSamples; i++) {
			trioBar.update(trioCount, { name, sampleCount: i + 1, totalSamples: numSamples });
			trioBar.render();
      if (performance.now() - startTime > timeout * 1000) {
        // console.warn(`Timed out after ${i} samples.`);
        break;
      }

      let timedout = false;

      optimizer.init(state, stepTimeout * 1000);
      state.variation = sampler();
      state.currentStageIndex = 0;
      state = resample(state);

      let shouldStop = false;
      while (!shouldStop) {
        if (performance.now() - startTime > timeout * 1000) {
          // console.warn(`Timed out after ${i} samples.`);
          timedout = true;
          break;
        }

        try {
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
							// console.error(`Error during optimization step: ${result.reason}`);
              numFailures++;
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
    }

    const result = {
      samples: takenSamples,
      successes: numSuccesses,
      badMinima: numBadMinima,
      failures: numFailures,
    };

    console.log(`\nResults for ${name}:`);
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

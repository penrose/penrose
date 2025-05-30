import "global-jsdom/register";

import { entries, makeResolver, Trio } from "@penrose/examples";
import { render } from "@penrose/roger";
import { computeDiagramExploration } from "../landscape-exploration.js";
import { compile, PenroseState, resample } from "@penrose/core";
import fs from "fs";
import { ExteriorPointOptimizer, LBGFSOptimizer, OptimizerResult } from "../Optimizers.js";
import { removeStaging } from "../utils.js";

const args = process.argv.slice(2);
const mode = args[0];
const outputDir = args.length > 1 ? args[1] : "exploration-output";

const maxSamplesPerDim = 50;
const totalMinutes = 10;
const totalMs = totalMinutes * 60 * 1000;
const maxMs = totalMs / entries.length;
console.log(`Max time per entry: ${maxMs} ms`);

const namesAndTrios = (await Promise.all(entries
  .map(async ([name, meta]) => {
    if (!meta.trio) return null;
    const trio = await meta.get();
    return [name, trio] as [string, Trio];
  })))
  .filter(x => x !== null);

const compileTrio = async (
  trio: Trio,
): Promise<PenroseState | null> => {
  const style = trio.style.reduce((acc, s) => acc + s.contents, "");
  const result = await compile({
    substance: trio.substance,
    style,
    domain: trio.domain,
    variation: trio.variation,
    excludeWarnings: trio.excludeWarnings || [],
  });
  if (result.isErr()) {
    return null;
  }
  return removeStaging(result.value);
}

const estimateSuccessRate = (
  state: PenroseState,
  sampler: () => string,
  maxSamplesPerDim: number,
  maxMs: number
): number => {
  const startTime = self.performance.now();

  const optimizer = new ExteriorPointOptimizer(new LBGFSOptimizer());

  let successes = 0;
  let numSamples = 0;
  const maxSamples = maxSamplesPerDim * state.inputs.length;
  for (let i = 0; i < maxSamples; i++) {
    numSamples++;

    optimizer.reset();

    state.variation = sampler();
    state = resample(state);

    let result: OptimizerResult;
    let error = false;
    do {
      try {
        result = optimizer.step(state);
      } catch (e) {
        error = true;
        break;
      }
    } while (result.tag === "Unconverged");

    if (error) {
      continue;
    }

    if (result.tag === "Failed") {
      console.log(`Optimization failed on iteration ${i}`);
      continue;
    }

    const constraintSum = result.outputs.constraints
      .map(c => Math.max(0, c) * Math.max(0, c))
      .reduce((acc, p) => acc + p, 0);

    if (constraintSum < 1e-1) {
      successes++;
    }

    const elapsedTime = self.performance.now() - startTime;
    if (elapsedTime > maxMs) {
      console.log(`Stopping after ${numSamples} samples due to time limit.`);
      break;
    }
  }

  return successes / numSamples;
}

const computeStatistics = async () => {
  let nextVariation = 0;
  const sampler = () => `${nextVariation++}`;

  for (const [name, trio] of namesAndTrios) {
    const state = await compileTrio(trio);
    if (!state) {
      console.error(`Failed to compile trio for ${name}`);
      continue;
    }

    const explorationInfo = computeDiagramExploration(
      state,
      sampler,
      maxSamplesPerDim,
      maxMs
    );

    // save explorationInfo to output directory
    const outputFile = `${outputDir}/${name}.json`;
    const outputFileDir = outputFile.split("/").slice(0, -1).join("/");
    fs.mkdirSync(outputFileDir, { recursive: true });
    fs.writeFileSync(outputFile, JSON.stringify(explorationInfo, null, 2));

    console.log(
      `Saved exploration info for ${name} to ${outputFile}`,
    );
  }
};

const computeSuccessRates = async () => {
  let nextVariation = 0;
  const sampler = () => `${nextVariation++}`;

  const successRates: Record<string, number> = {};

  for (const [name, trio] of namesAndTrios) {
    const state = await compileTrio(trio);
    if (!state) {
      console.error(`Failed to compile trio for ${name}`);
      continue;
    }

    const successRate = estimateSuccessRate(state, sampler, maxSamplesPerDim, maxMs);
    successRates[name] = successRate;
    console.log(
      `Success rate for ${name}: ${successRate.toFixed(3)}`,
    );
  }

  // save success rates to output directory
  const outputFile = `${outputDir}/success-rates.json`;
  fs.mkdirSync(outputDir, { recursive: true });
  fs.writeFileSync(outputFile, JSON.stringify(successRates, null, 2));
}

switch (mode) {
  case "statistics":
    await computeStatistics();
    break;

  case "success-rate":
    await computeSuccessRates();
    break;
}

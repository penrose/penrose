import "global-jsdom/register";

import { entries, Trio } from "@penrose/examples";
import fs from "fs";
import { compileTrio } from "../utils.js";
import { computeDiagramExploration } from "./statistics.js";
import { estimateSuccessRates } from "./success-rate.js";

const args = process.argv.slice(2);
const mode = args[0];
const outputDir = args.length > 1 ? args[1] : "collect-output";

const maxSamplesPerDim = 50;
const totalMinutes = 30;
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
      maxMs,
    );

    // save explorationInfo to output directory
    const outputFile = `${outputDir}/${name}.json`;
    const outputFileDir = outputFile.split("/").slice(0, -1).join("/");
    fs.mkdirSync(outputFileDir, { recursive: true });
    fs.writeFileSync(outputFile, JSON.stringify(explorationInfo, null, 2));

    console.log(`Saved exploration info for ${name} to ${outputFile}`);
  }
};

const computeSuccessRates = async () => {
  const successRates = await estimateSuccessRates(
    namesAndTrios,
    maxSamplesPerDim,
    maxMs,
  );

  // save success rates to output directory
  const outputFile = `${outputDir}/success-rates.json`;
  fs.mkdirSync(outputDir, { recursive: true });
  fs.writeFileSync(
    outputFile,
    JSON.stringify(Object.fromEntries(successRates), null, 2),
  );
};

switch (mode) {
  case "statistics":
    await computeStatistics();
    break;

  case "success-rate":
    await computeSuccessRates();
    break;
}

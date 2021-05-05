require("global-jsdom/register");
import {
  compileDomain,
  compileSubstance,
  prettySubstance,
  showError,
  Synthesizer,
  SynthesizerSetting,
} from "@penrose/core";
import { SubProg } from "@penrose/core/build/dist/types/substance";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "fs";
import * as neodoc from "neodoc";
import { join } from "path";
// const chalk = require("chalk");

// process.env.NODE_ENV = "production";

const USAGE = `
Penrose Synthesizer.

Usage:
  penrose-synthesizer <domain> [options]

Options:
  --substance=SUB      A Substance program that will be included in every synthesized program     
  --style=STY          A Style program that the synthesized programs work with
  --path=PATH          Output path for the generated programs (Trailing slash not needed)
                       [default: synthesized-progs]
  --num-programs=NUM   The number of programs to generate 
                       [default: 1]
  --synth-setting=SET  A JSON file containing parameters for the synthesizer
`;

const defaultSetting: SynthesizerSetting = {
  mutationCount: [1, 4],
  argOption: "existing",
  weights: {
    type: 0.1,
    predicate: 0.3,
    constructor: 0.0,
  },
  add: {
    type: "*",
    // type: [],
    function: [],
    // constructor: [],
    constructor: "*",
    // predicate: ["Equal"],
    predicate: [],
  },
  delete: {
    type: [],
    function: [],
    constructor: [],
    // predicate: ["IsSubset"],
    predicate: [],
  },
};

const writePrograms = (
  progs: SubProg[],
  domainSrc: string,
  prefix: string
): void => {
  // make the directory if it doesn't exist
  if (!existsSync(prefix)) {
    mkdirSync(prefix, { recursive: true });
  }
  for (let i = 0; i < progs.length; i++) {
    const fileName = `prog-${i}.sub`;
    const filePath = join(prefix, fileName);
    writeFileSync(filePath, prettySubstance(progs[i]));
  }
};

// Main function

(async () => {
  // Process command-line arguments
  const args = neodoc.run(USAGE, { smartOptions: true });

  // Determine the output file path
  const substancePath = args["--substance"];
  const settingPath = args["--synth-setting"];
  const outputPath = args["--path"];
  const domainPath = args["<domain>"];
  const numPrograms = +args["--num-programs"]; // NOTE: convert to number
  const domainSrc = readFileSync(domainPath, "utf8").toString();
  const envOrError = compileDomain(domainSrc);

  // initialize synthesizer

  if (envOrError.isOk()) {
    const env = envOrError.value;
    let subResult;
    if (substancePath) {
      const substanceSrc = readFileSync(substancePath, "utf8").toString();
      const subRes = compileSubstance(substanceSrc, env);
      if (subRes.isOk()) {
        subResult = subRes.value;
      } else {
        console.log(
          `Error when compiling the template Substance program: ${showError(
            subRes.error
          )}`
        );
      }
    }

    const synth = new Synthesizer(env, defaultSetting, subResult);
    const progs = synth.generateSubstances(numPrograms);

    // write progs
    writePrograms(progs, domainSrc, outputPath);

    // progs.map((prog) => console.log(prettySubstance(prog) + "\n-------"));
  }
})();

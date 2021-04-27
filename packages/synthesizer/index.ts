require("global-jsdom/register");
import { readFileSync } from "fs";
import * as neodoc from "neodoc";
import {
  compileDomain,
  prettySubstance,
  showError,
  Synthesizer,
} from "@penrose/core";
// const chalk = require("chalk");

const USAGE = `
Penrose Synthesizer.

Usage:
  penrose-synthesizer <domain> [options]

Options:
  --substance=SUB      A Substance program that will be included in every synthesized program     
  --style=STY          A Style program that the synthesized programs work with
  --spec=SPEC          A domain program used as a specification for synthesis 
  --path=PATH          Output path for the generated programs (Trailing slash not needed)
                       [default: synthesized-progs]
  --num-programs=NUM   The number of programs to generate 
                       [default: 1]
  --synth-setting=SET  A JSON file containing parameters for the synthesizer
`;

const defaultSetting = {
  lengthRange: [1, 10],
  argOption: "mixed",
  weights: {
    type: 0.1,
    predicate: 0.3,
    constructor: 0.0,
  },
};

// Main function

(async () => {
  // Process command-line arguments
  const args = neodoc.run(USAGE, { smartOptions: true });

  // Determine the output file path
  const substance = args["--substance"];
  const settingPath = args["--synth-setting"];
  const domainPath = args["<domain>"];
  const numPrograms = +args["--num-programs"]; // NOTE: convert to number
  const domainSrc = readFileSync(domainPath, "utf8").toString();
  const envOrError = compileDomain(domainSrc);

  // initialize synthesizer

  // process spec
  let specEnv = undefined;
  const specPath = args["--spec"];
  if (specPath) {
    const specSrc = readFileSync(specPath, "utf8").toString();
    const specOrError = compileDomain(specSrc);
    if (specOrError.isOk()) {
      specEnv = specOrError.value;
    } else {
      console.error(
        "Error when compiling the specification Domain program:\n" +
          showError(specOrError.error)
      );
    }
  }

  if (envOrError.isOk()) {
    const env = envOrError.value;
    const synth = new Synthesizer(env, defaultSetting, specEnv);
    const progs = synth.generateSubstances(numPrograms);
    progs.map((prog) => console.log(prettySubstance(prog) + "\n-------"));
  }
})();

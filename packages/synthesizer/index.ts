require("global-jsdom/register");
import { readFileSync } from "fs";
import * as neodoc from "neodoc";
import {
  compileDomain,
  prettySubstance,
  synthesizePrograms,
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

// Main function

(async () => {
  // Process command-line arguments
  const args = neodoc.run(USAGE, { smartOptions: true });

  // Determine the output file path
  const substance = args["--substance"];
  const domainPath = args["<domain>"];
  const numPrograms = +args["--num-programs"]; // NOTE: convert to number
  const domainSrc = readFileSync(domainPath, "utf8").toString();
  const envOrError = compileDomain(domainSrc);
  if (envOrError.isOk()) {
    const env = envOrError.value;
    const progs = synthesizePrograms(env, numPrograms);
    progs.map((prog) => console.log(prettySubstance(prog) + "\n-------"));
  }
})();

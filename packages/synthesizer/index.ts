require("global-jsdom/register");
import {
  Registry,
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
  --registry=REGISTRY  Path for the generated registry
                       [default: registry.json]
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
    // type: "*",
    type: [],
    function: [],
    constructor: [],
    // constructor: "*",
    predicate: ["Equal"],
    // predicate: [],
  },
  delete: {
    type: [],
    function: [],
    constructor: [],
    predicate: ["IsSubset"],
    // predicate: [],
  },
};

const writePrograms = (
  progs: SubProg[],
  domainSrc: string,
  prefix: string,
  styleSrc?: string,
  registryPath?: string
): void => {
  // make the directory if it doesn't exist
  const substances = {};
  const trios = [];
  const styID = "style";
  const domainID = "domain";
  const stylePath = "style.sty";
  const domainPath = "domain.dsl";
  const style = {
    name: styID,
    URI: stylePath,
  };
  const domain = {
    name: domainID,
    URI: domainPath,
  };
  if (!existsSync(prefix)) {
    mkdirSync(prefix, { recursive: true });
  }
  for (let i = 0; i < progs.length; i++) {
    const subID = `prog-${i}`;
    const fileName = `${subID}.sub`;
    const subPath = join(prefix, fileName);
    writeFileSync(subPath, prettySubstance(progs[i]));
    substances[subID] = { name: subID, URI: fileName };
    trios.push({ substance: subID, style: styID, domain: domainID });
  }
  // TODO: change name style and domain
  const registry: Registry = {
    substances,
    styles: {
      [styID]: style,
    },
    domains: {
      [domainID]: domain,
    },
    trios,
  };
  if (stylePath && registryPath) {
    writeFileSync(join(prefix, registryPath), JSON.stringify(registry));
    writeFileSync(join(prefix, domainPath), domainSrc);
    writeFileSync(join(prefix, stylePath), styleSrc);
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
  const stylePath = args["--style"];
  const numPrograms = +args["--num-programs"]; // NOTE: convert to number
  const domainSrc = readFileSync(domainPath, "utf8").toString();
  const styleSrc = readFileSync(stylePath, "utf8").toString();
  const envOrError = compileDomain(domainSrc);
  const registryPath = args["--registry"];

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
    if (stylePath) {
      writePrograms(progs, domainSrc, outputPath, styleSrc, registryPath);
    } else {
      writePrograms(progs, domainPath, outputPath);
    }

    // progs.map((prog) => console.log(prettySubstance(prog) + "\n-------"));
  }
})();

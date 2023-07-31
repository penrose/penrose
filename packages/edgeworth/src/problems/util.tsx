import { MultipleChoiceProblem } from "@penrose/components";
import {
  compileDomain,
  compileSubstance,
  prettySubstance,
  showError,
} from "@penrose/core";
import { shuffle } from "lodash";
import { Preset } from "../examples.js";
import {
  SynthesizedSubstance,
  Synthesizer,
  SynthesizerSetting,
} from "../synthesis/Synthesizer.js";

const generateProgs = (
  setting: SynthesizerSetting,
  seed: string,
  numPrograms: number,
  domain: string,
  substance: string,
) => {
  const envOrError = compileDomain(domain);
  // initialize synthesizer
  if (envOrError.isOk()) {
    const env = envOrError.value;
    let subResult;
    if (substance.length > 0) {
      const subRes = compileSubstance(substance, env);
      if (subRes.isOk()) {
        subResult = subRes.value;
      } else {
        console.log(
          `Error when compiling the template Substance program: ${showError(
            subRes.error,
          )}`,
        );
      }
    }
    const synth = new Synthesizer(env, setting, subResult, seed);
    let progs = synth.generateSubstances(numPrograms);
    const template = synth.getTemplate();
    return [{ prog: template, ops: [] } as SynthesizedSubstance, ...progs];
  }
};

export const multipleChoiceProblem = (
  preset: Preset,
  seed: string,
  numPrograms: number,
  answer: {
    correct: number[];
    incorrect: number[];
  },
) => {
  const { prompt, substance, style, domain, setting } = preset;
  const progs = generateProgs(setting, seed, numPrograms, domain, substance)!;
  // `answer` include indices into some of the generated diagrams (i.e. `progs`). `options` are these diagrams with the correctness flag attached.
  const options = progs.reduce(
    (
      problems: {
        style: string;
        domain: string;
        substance: string;
        variation: string;
        answer: boolean;
      }[],
      p: SynthesizedSubstance,
      i: number,
    ) => {
      const substance = prettySubstance(p.prog);
      if (answer.correct.includes(i)) {
        return [
          ...problems,
          { substance, style, domain, variation: `${i}`, answer: true },
        ];
      } else if (answer.incorrect.includes(i)) {
        return [
          ...problems,
          {
            substance,
            style,
            domain,
            variation: `${i}`,
            answer: false,
          },
        ];
      } else return problems;
    },
    [],
  );

  return (
    <MultipleChoiceProblem
      diagrams={shuffle(options)}
      correctIndices={answer.correct}
      prompt={prompt}
    ></MultipleChoiceProblem>
  );
};

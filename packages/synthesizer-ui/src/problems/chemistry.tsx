import { MultipleChoiceProblem } from "@penrose/components";
import {
  compileDomain,
  compileSubstance,
  prettySubstance,
  showError,
  SynthesizedSubstance,
  Synthesizer,
  SynthesizerSetting,
} from "@penrose/core";
import { Preset, presets } from "../examples";

const generateProgs = (
  setting: SynthesizerSetting,
  seed: string,
  numPrograms: number,
  domain: string,
  substance: string
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
            subRes.error
          )}`
        );
      }
    }
    const synth = new Synthesizer(env, setting, subResult, seed);
    let progs = synth.generateSubstances(numPrograms);
    const template = synth.getTemplate();
    return [{ prog: template, ops: [] } as SynthesizedSubstance, ...progs];
  }
};

const assembleProblem = (
  preset: Preset,
  seed: string,
  numPrograms: number,
  answer: {
    correct: number[];
    incorrect: number[];
  }
) => {
  const { prompt, substance, style, domain, setting } = preset;
  const progs = generateProgs(setting, seed, numPrograms, domain, substance)!;
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
      i: number
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
    []
  );

  return (
    <MultipleChoiceProblem
      diagrams={options}
      correctIndices={answer.correct}
      prompt={prompt}
    ></MultipleChoiceProblem>
  );
};

export default function () {
  const problems = [
    assembleProblem(presets["lewis_0"], "test0", 10, {
      correct: [0],
      incorrect: [1, 4, 5],
    }),
    assembleProblem(presets["lewis_1"], "test0", 10, {
      correct: [0],
      incorrect: [1, 2, 3],
    }),
    assembleProblem(presets["lewis_2"], "test0", 10, {
      correct: [0],
      incorrect: [1, 2, 3],
    }),
    assembleProblem(presets["lewis_3"], "test0", 10, {
      correct: [0],
      incorrect: [1, 2, 3],
    }),
    assembleProblem(presets["lewis_4"], "test0", 10, {
      correct: [0],
      incorrect: [1, 2, 3],
    }),
    assembleProblem(presets["lewis_5"], "test0", 10, {
      correct: [0],
      incorrect: [1, 2, 3],
    }),
    assembleProblem(presets["lewis_6"], "test0", 10, {
      correct: [0],
      incorrect: [1, 2, 3],
    }),
  ];
  return <div>{problems}</div>;
}

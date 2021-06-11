import { compileDomain } from "compiler/Domain";
import {
  compileSubstance,
  prettyStmt,
  prettySubstance,
} from "compiler/Substance";
import { Env } from "types/domain";
import { Decl } from "types/substance";
import { Synthesizer, SynthesizerSetting } from "./Synthesizer";

const defaultSetting: SynthesizerSetting = {
  mutationCount: [1, 1],
  argOption: "existing",
  argReuse: "distinct",
  weights: {
    type: 0.1,
    predicate: 0.3,
    constructor: 0.0,
  },
  add: {
    type: [],
    function: [],
    constructor: [],
    predicate: [],
  },
  delete: {
    type: [],
    function: [],
    constructor: [],
    predicate: [],
  },
  edit: {
    type: [],
    function: [],
    constructor: [],
    predicate: [],
  },
};

const initSynth = (
  substance: string,
  setting: SynthesizerSetting
): Synthesizer => {
  let subResult;
  const subRes = compileSubstance(substance, env);
  if (subRes.isOk()) {
    subResult = subRes.value;
  }
  const synth = new Synthesizer(env, setting, subResult);
  synth.cxt.loadTemplate();
  return synth;
};

const domain = `type Set
function Intersection : Set a * Set b -> Set
function Subset : Set a * Set b -> Set`;
const env: Env = compileDomain(domain).unsafelyUnwrap();

describe("Synthesizer Operations", () => {
  test("cascading delete", () => {
    const original = `Set A
Set B
Set D
Set C := Subset(B, A)
Set E := Intersection(D,C)`;
    const expected = `Set B
Set D`;
    const synth = initSynth(original, {
      ...defaultSetting,
      delete: {
        ...defaultSetting.delete,
        type: ["Set"],
      },
    });
    synth.cxt.loadTemplate();
    const toDelete = synth.cxt.prog.statements[0] as Decl;
    expect("Set A").toEqual(prettyStmt(toDelete));
    synth.cascadingDelete(toDelete);
    const newAST = synth.cxt.prog;
    expect(prettySubstance(newAST)).toEqual(expected);
  });
});

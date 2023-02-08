import { cascadingDelete } from "../analysis/SubstanceAnalysis";
import { compileDomain } from "../compiler/Domain";
import {
  compileSubstance,
  prettyStmt,
  prettySubstance,
} from "../compiler/Substance";
import { A } from "../types/ast";
import { Env } from "../types/domain";
import { Decl, SubStmt } from "../types/substance";
import { Delete, executeMutations, removeStmtCtx } from "./Mutation";
import { initContext, Synthesizer, SynthesizerSetting } from "./Synthesizer";

const defaultSetting: SynthesizerSetting = {
  mutationCount: [1, 1],
  argOption: "existing",
  argReuse: "distinct",
  weights: {
    type: 0.1,
    predicate: 0.3,
    constructor: 0.0,
  },
  opWeights: {
    add: 0.3333,
    delete: 0.3333,
    edit: 0.3333,
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
  const synth = new Synthesizer(env, setting, subResult, "seed");
  return synth;
};

const domain = `type Set
function Intersection(Set a, Set b) -> Set
function Subset(Set a, Set b) -> Set`;
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
    const ctx = initContext(
      synth.env,
      defaultSetting.argOption,
      defaultSetting.argReuse,
      "test0"
    );
    const toDelete = synth.currentProg.statements[0] as Decl<A>;
    expect("Set A").toEqual(prettyStmt(toDelete));
    const cascadedStmts: SubStmt<A>[] = cascadingDelete(
      toDelete,
      synth.currentProg
    );
    const ops: Delete[] = cascadedStmts.map((stmt) => ({
      tag: "Delete",
      stmt,
      mutate: removeStmtCtx,
    }));

    const { res: newAST } = executeMutations(ops, synth.template, ctx);
    expect(prettySubstance(newAST)).toEqual(expected);
  });
});

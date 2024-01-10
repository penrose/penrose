// @vitest-environment jsdom

import { compileDomain, compileSubstance } from "@penrose/core";
import {
  initSubstanceEnv,
  prettyStmt,
  prettySubstance,
} from "@penrose/core/dist/compiler/Substance";
import { A } from "@penrose/core/dist/types/ast";
import { DomainEnv } from "@penrose/core/dist/types/domain";
import { CompiledSubStmt, Decl } from "@penrose/core/dist/types/substance";
import { describe, expect, test } from "vitest";
import { cascadingDelete } from "../analysis/SubstanceAnalysis.js";
import { Delete, executeMutations, removeStmtCtx } from "./Mutation.js";
import { Synthesizer, SynthesizerSetting, initContext } from "./Synthesizer.js";

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
  setting: SynthesizerSetting,
): Synthesizer => {
  let subEnv;
  const subRes = compileSubstance(substance, domEnv);
  if (subRes.isOk()) {
    subEnv = subRes.value;
  }
  const synth = new Synthesizer(
    domEnv,
    initSubstanceEnv(),
    setting,
    subEnv === undefined ? undefined : [subEnv, domEnv],
    "seed",
  );
  return synth;
};

const domain = `type Set
function Intersection(Set a, Set b) -> Set
function Subset(Set a, Set b) -> Set`;
const domEnv: DomainEnv = compileDomain(domain).unsafelyUnwrap();

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
      synth.domEnv,
      synth.subEnv,
      defaultSetting.argOption,
      defaultSetting.argReuse,
      "test0",
    );
    const toDelete = synth.currentProg.statements[0] as Decl<A>;
    expect("Set A").toEqual(prettyStmt(toDelete));
    const cascadedStmts: CompiledSubStmt<A>[] = cascadingDelete(
      toDelete,
      synth.currentProg,
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

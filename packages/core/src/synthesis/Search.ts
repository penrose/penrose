import { groupBy } from "lodash";
import {
  Mutation,
  SynthesizedSubstance,
  SynthesizerSetting,
} from "synthesis/Synthesizer";
import { SubStmt } from "types/substance";

interface TaggedMutationSet {
  tag: Mutation["tag"];
  stmt: SubStmt[];
}

const synthesizeConfig = (
  examples: SynthesizedSubstance[]
): SynthesizerSetting => {
  const ops: Mutation[] = examples
    .map((ex: SynthesizedSubstance) => ex.ops)
    .flat();
  const taggedMutations: TaggedMutationSet[] = groupBy(ops, "tag");

  return {};
};

const editedStmt = (mutation: Mutation): SubStmt => {
  switch (mutation.tag) {
    case "Add":
    case "Delete":
    case "Swap":
    case "ReplaceName":
      return mutation.stmt;
    case "Replace":
      return mutation.old;
  }
};

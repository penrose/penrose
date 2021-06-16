import { prettyStmt } from "compiler/Substance";
import _, { flow, groupBy, map } from "lodash";
import {
  Mutation,
  SynthesizedSubstance,
  SynthesizerSetting,
} from "synthesis/Synthesizer";
import { SubStmt } from "types/substance";

interface TaggedMutationSet {
  tag: Mutation["tag"];
  stmts: SubStmt[];
}

export const synthesizeConfig = (examples: SynthesizedSubstance[]) => {
  // ): SynthesizerSetting => {
  const ops: Mutation[] = examples
    .map((ex: SynthesizedSubstance) => ex.ops)
    .flat();

  const grouped = groupBy(ops, "tag");

  const taggedMutations: TaggedMutationSet[] = map(
    grouped,
    (value: Mutation[], key: Mutation["tag"]): TaggedMutationSet => {
      const set: TaggedMutationSet = {
        tag: key,
        stmts: value.map(editedStmt),
      };
      return set;
    }
  ) as any; // TODO: resolve types: why is it `boolean[]`?

  console.log(showMutationSets(taggedMutations));

  // return {};
};

const showMutationSets = (sets: TaggedMutationSet[]): string =>
  sets.map(showMutationSet).join("\n\n");

const showMutationSet = (set: TaggedMutationSet): string =>
  `${set.tag}:\n${set.stmts.map(prettyStmt).join("\n")}`;

const editedStmt = (mutation: Mutation): SubStmt => {
  switch (mutation.tag) {
    case "Replace":
      return mutation.old;
    case "Add":
    case "Delete":
    case "Swap":
    case "ReplaceName":
    case "TypeChange":
      return mutation.stmt;
  }
};

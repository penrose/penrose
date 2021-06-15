import { prettyStmt } from "compiler/Substance";
import _, { groupBy } from "lodash";
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
  const taggedMutations: any = _.chain(ops)
    .groupBy("tag")
    .map((value: Mutation[], key: Mutation["tag"]) => {
      return {
        tag: key,
        stmts: value.map(editedStmt),
      };
    })
    .value();
  console.log(showMutationSets(taggedMutations as any));

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

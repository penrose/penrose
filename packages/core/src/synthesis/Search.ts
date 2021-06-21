import { prettyStmt } from "compiler/Substance";
import { groupBy, map } from "lodash";
import { getDiff, rdiffResult } from "recursive-diff";
import { Mutation, SynthesizedSubstance } from "synthesis/Synthesizer";
import { metaProps } from "types/ast";
import { SubProg, SubStmt } from "types/substance";

//#region Generalized edits

type Edit = Mutation;

const generalizedEdits = (
  original: SubProg,
  editedProgs: SubProg[],
  mode: "exact"
): Edit => {
  switch (mode) {
    case "exact":
      return {} as any; // COMBAK: complete the function
  }
};

export const diffSubprogs = (left: SubProg, right: SubProg): rdiffResult[] => {
  const diffs: rdiffResult[] = getDiff(left, right);
  // remove all diffs related to meta-properties
  const concreteDiffs: rdiffResult[] = diffs.filter(
    (diff: rdiffResult) =>
      !diff.path.some((key: string | number) =>
        metaProps.includes(key.toString())
      )
  );
  return concreteDiffs;
};

//#endregion

//#region Specification synthesis
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

  // console.log(showMutationSets(taggedMutations));

  // TODO: finish this function
  return {} as any;
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

//#endregion

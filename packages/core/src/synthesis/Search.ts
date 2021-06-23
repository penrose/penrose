import { getStmt, sortStmts } from "analysis/SubstanceAnalysis";
import { prettyStmt } from "compiler/Substance";
import { cloneDeep, get, groupBy, map } from "lodash";
import { applyDiff, getDiff, rdiffResult } from "recursive-diff";
import { Mutation, SynthesizedSubstance } from "synthesis/Synthesizer";
import { ASTNode, Identifier, metaProps } from "types/ast";
import { SubProg, SubStmt } from "types/substance";

//#region Generalized edits

type Edit = Mutation;
type DiffType = ASTNode["tag"];

export interface StmtDiff {
  diff: rdiffResult;
  stmt: SubStmt;
  diffType: DiffType;
  originalValue: any;
}

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

/**
 * Compute the exact diffs between two Substance ASTs.
 *
 * @param left the original Substance program
 * @param right the changed Substance program
 * @returns a list of diffs
 */
export const diffSubProgs = (left: SubProg, right: SubProg): rdiffResult[] => {
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

/**
 * Compute statement-insensitive diffs between two Substance ASTs.
 *
 * @param left the original Substance program
 * @param right the changed Substance program
 * @returns a list of diffs tagged with the original statement
 */
export const diffSubStmts = (left: SubProg, right: SubProg): StmtDiff[] => {
  // normalize the statement orderings of both ASTs first
  const [leftSorted, rightSorted] = [sortStmts(left), sortStmts(right)];
  // compute the exact diffs between two normalized ASTs
  const exactDiffs: rdiffResult[] = diffSubProgs(leftSorted, rightSorted);
  // tag the diffs with
  return exactDiffs.map((d) => toStmtDiff(d, leftSorted));
};

export const toStmtDiff = (diff: rdiffResult, ast: SubProg): StmtDiff => {
  const [, stmtIndex, ...path] = diff.path;
  // TODO: encode the paths to AST in a more principled way
  const stmt = getStmt(ast, stmtIndex as number);
  const originalValue = get(stmt, path);
  const stmtDiff = {
    ...diff,
    path,
  };
  return {
    diff: stmtDiff,
    stmt,
    diffType: diffType(stmt, stmtDiff),
    originalValue,
  };
};

/**
 * Infer the diff type from the AST-level diff by looking up the deepest tagged AST node in the diff path.
 *
 * @param node the top-level node changed
 * @param diff the diff defined for the node
 * @returns
 */
const diffType = (node: ASTNode, diff: rdiffResult): ASTNode["tag"] => {
  let tag = undefined;
  let currNode: ASTNode = node;
  for (const prop of diff.path) {
    currNode = currNode[prop];
    if (currNode.tag) {
      tag = currNode.tag;
    }
  }
  if (!tag) throw new Error(`unknown diff type in ${diff.path}`);
  else return tag;
};

export const showStmtDiff = (d: StmtDiff): string =>
  `Changed ${prettyStmt(d.stmt)} (${d.diffType}): ${d.originalValue} (${
    d.diff.path
  }) -> ${d.diff.val}`;

export const applyStmtDiff = (prog: SubProg, stmtDiff: StmtDiff): SubProg => {
  const { diff, stmt } = stmtDiff;
  return {
    ...prog,
    statements: prog.statements.map((s: SubStmt) => {
      if (s === stmt) {
        const res: SubStmt = applyDiff(cloneDeep(s), [diff]);
        return res;
      } else return s;
    }),
  };
};

export const swapDiffID = (d: StmtDiff, id: Identifier): StmtDiff => {
  return {
    ...d,
    diff: {
      ...d.diff,
      val: id.value,
    },
  };
};

/**
 * Apply a set of statement diffs on a Substance program.
 * NOTE: instead of sequencially applying each diff, we find all applicable diffs and apply them in a batch for each Substance statement
 *
 * @param prog the origianl Substance program
 * @param diffs a set of diffs to apply
 * @returns
 */
export const applyStmtDiffs = (
  prog: SubProg,
  diffs: StmtDiff[],
  generalize?: (originalDiff: StmtDiff) => StmtDiff
): SubProg => ({
  ...prog,
  statements: prog.statements.map((stmt: SubStmt) =>
    applyDiff(stmt, findDiffs(stmt, diffs))
  ),
});

export const findDiffs = (stmt: SubStmt, diffs: StmtDiff[]): rdiffResult[] =>
  diffs.filter((d) => d.stmt === stmt).map((d) => d.diff);

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

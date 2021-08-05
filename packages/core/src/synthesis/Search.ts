import {
  cleanNode,
  getStmt,
  intersection,
  nodesEqual,
  sortStmts,
  subProg,
} from "analysis/SubstanceAnalysis";
import { prettyStmt, prettySubstance } from "compiler/Substance";
import { cloneDeep, difference, get, intersectionWith, sortBy } from "lodash";
import { applyDiff, getDiff, rdiffResult } from "recursive-diff";
import { ASTNode, Identifier, metaProps } from "types/ast";
import { Env } from "types/domain";
import { SubProg, SubStmt } from "types/substance";
import {
  Add,
  addMutation,
  Delete,
  deleteMutation,
  enumerateMutations,
  executeMutation,
  Mutation,
  MutationGroup,
  showMutations,
  Update,
} from "./Mutation";
import { initContext } from "./Synthesizer";

//#region Fine-grained diffs

type DiffType = ASTNode["tag"];

export interface StmtDiff {
  diff: rdiffResult;
  stmt: SubStmt;
  diffType?: DiffType;
  originalValue: any;
}

export type SubDiff = UpdateDiff | AddDiff | DeleteDiff;
export interface DiffSet {
  add: AddDiff[];
  delete: DeleteDiff[];
  update: UpdateDiff[];
}
export interface UpdateDiff {
  diffType: "Update";
  source: SubStmt;
  result: SubStmt;
  rawDiff: rdiffResult[];
  stmtDiff: StmtDiff[];
}

export interface AddDiff {
  diffType: "Add";
  source: SubStmt;
}
export interface DeleteDiff {
  diffType: "Delete";
  source: SubStmt;
}

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
  return exactDiffs.map((d) => toStmtDiff(d, leftSorted));
};

/**
 * Determine if two Substance AST nodes are similar. The metric is whether the nodes have common descendents or are equal themselves.
 *
 * @param left a node in the Substance AST
 * @param right a node in the Substance AST
 * @returns if the nodes have common descendents
 */
export const similarNodes = (left: ASTNode, right: ASTNode): boolean => {
  const equalNodes = nodesEqual(left, right);
  const similarChildren = intersectionWith(
    left.children,
    right.children,
    similarNodes
  );

  const similarLeft = intersectionWith([left], right.children, similarNodes);
  const similarRight = intersectionWith(left.children, [right], similarNodes);

  // console.log(
  //   prettySubNode(left as any),
  //   prettySubNode(right as any),
  //   equalNodes,
  //   similarChildren.map((n) => prettySubNode(n as any)),
  //   similarLeft.map((n) => prettySubNode(n as any)),
  //   similarRight.map((n) => prettySubNode(n as any))
  // );
  // console.log(left, right);

  return (
    equalNodes ||
    similarChildren.length > 0 ||
    similarLeft.length > 0 ||
    similarRight.length > 0
  );
};

interface SimilarMapping {
  source: SubStmt;
  similarStmts: SubStmt[];
}

export const subProgDiffs = (left: SubProg, right: SubProg): DiffSet => {
  const commonStmts = intersection(left, right);
  const leftFiltered = left.statements.filter((a) => {
    return intersectionWith(commonStmts, [a], nodesEqual).length === 0;
  });
  const rightFiltered = right.statements.filter((a) => {
    return intersectionWith(commonStmts, [a], nodesEqual).length === 0;
  });
  const similarMap = similarMappings(leftFiltered, rightFiltered);

  const update: UpdateDiff[] = updateDiffs(similarMap);
  const updatedSource: SubStmt[] = update.map((d) => d.source);
  const updatedResult: SubStmt[] = update.map((d) => d.result);
  // console.log(rightFiltered.map(prettyStmt).join("\n"));
  // console.log(updatedResult.map(prettyStmt).join("\n"));

  const deleted: DeleteDiff[] = leftFiltered
    .filter(
      (s) => intersectionWith([s], updatedSource, nodesEqual).length === 0
    )
    .map((s) => ({ diffType: "Delete", source: s }));
  const add: AddDiff[] = rightFiltered
    .filter(
      (s) => intersectionWith([s], updatedResult, nodesEqual).length === 0
    )
    .map((s) => ({ diffType: "Add", source: s }));
  return { update, add, delete: deleted };
};

export const showSubDiff = (d: SubDiff) => {
  switch (d.diffType) {
    case "Add":
    case "Delete":
      return `${d.diffType}: ${prettyStmt(d.source)}`;
    case "Update":
      return `${d.diffType}: ${prettyStmt(d.source)} -> ${prettyStmt(
        d.result
      )}\n\t${d.stmtDiff.map(showStmtDiff).join("\n\t")}`;
  }
};

export const showDiffset = (d: DiffSet): string =>
  [...d.add, ...d.delete, ...d.update].map(showSubDiff).join("\n");

export const updateDiffs = (mappings: SimilarMapping[]): UpdateDiff[] => {
  const picked = [];
  const diffs = [];
  for (const m of mappings) {
    const diff = toUpdateDiff(picked, m);
    if (diff) {
      diffs.push(diff);
      picked.push(diff.result);
    }
  }
  return diffs;
};

const toUpdateDiff = (
  picked: SubStmt[],
  { similarStmts, source }: SimilarMapping
): UpdateDiff | undefined => {
  const result = difference(similarStmts, picked)[0]; // TODO: insert ranking algorithm here
  if (result === undefined) return undefined;
  const rawDiffs = getDiff(cleanNode(source), cleanNode(result));
  return {
    diffType: "Update",
    source,
    result,
    rawDiff: rawDiffs,
    stmtDiff: rawDiffs.map((d) => rawToStmtDiff(d, source)),
  };
};

/**
 * For each statement in `leftSet`, find out similar statements in `rightSet`.
 *
 * @param leftSet the first set of statements
 * @param rightSet the second set of statements
 * @returns
 */
export const similarMappings = (
  leftSet: SubStmt[],
  rightSet: SubStmt[]
): SimilarMapping[] => {
  const mappings: SimilarMapping[] = [];
  for (const stmt of leftSet) {
    const similarSet: SubStmt[] = rightSet.filter((s) => similarNodes(stmt, s));
    if (similarSet.length > 0)
      mappings.push({
        source: stmt,
        // HACK: for consistent ordering
        similarStmts: sortBy(similarSet, prettyStmt),
      });
  }
  return mappings;
};

export const rawToStmtDiff = (diff: rdiffResult, source: SubStmt): StmtDiff => {
  const { path } = diff;
  const originalValue = get(source, path);
  const stmtDiff = {
    ...diff,
    path,
  };
  return {
    diff,
    stmt: source,
    // diffType: diffType(source, stmtDiff),
    originalValue,
  };
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
const diffType = (node: ASTNode, diff: rdiffResult): DiffType => {
  let tag = undefined;
  let currNode: ASTNode = node;
  for (const prop of diff.path) {
    currNode = currNode[prop];
    if (currNode.tag) {
      tag = currNode.tag;
    }
  }
  if (!tag) return diff.path.join(".");
  else return tag;
};

export const showStmtDiff = (d: StmtDiff): string =>
  `Changed ${prettyStmt(d.stmt)} ${d.diffType ? `(${d.diffType})` : ""}: ${
    d.originalValue
  } (${d.diff.path}) -> ${d.diff.val}`;

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

//#region Single-mutation search

const cartesianProduct = <T>(...sets: T[][]) =>
  sets.reduce<T[][]>(
    (accSets, set) =>
      accSets.flatMap((accSet) => set.map((value) => [...accSet, value])),
    [[]]
  );

/**
 * Given two Substance programs, find possible mutation paths that transform from `src` to `dest`.
 *
 * @param src The source Substance program
 * @param dest The changed Substance program
 * @param srcEnv The environment for the source Substance program
 */
export const findMutationPaths = (
  src: SubProg,
  dest: SubProg,
  srcEnv: Env
): MutationGroup[] => {
  const diffs: DiffSet = subProgDiffs(src, dest);
  // pack add and delete mutations
  const addMutations: Add[] = diffs.add.map((a) => addMutation(a.source));
  const deleteMutations: Delete[] = diffs.delete.map((a) =>
    deleteMutation(a.source)
  );
  // find all possible updates for each statement in the update set
  const matchingUpdates: MutationGroup[] = diffs.update.map((d) => {
    const mutations = enumerateMutations(d.source, srcEnv);
    d.source;
    const ctx = initContext(srcEnv);
    const matchedMutations = mutations.filter((m) => {
      // HACK: assumes each update pair is connected by only one mutation. Therefore packing the source and result stmts into individual programs
      const prog1 = subProg([d.source]);
      const prog2 = subProg([d.result]);
      const { res: mutatedAST } = executeMutation(m, prog1, ctx);
      return (
        prettySubstance(sortStmts(prog2)) ===
        prettySubstance(sortStmts(mutatedAST))
      );
    });
    return matchedMutations;
  });

  // any combination of candidate mutations for each stmt will be a valid mutation group
  const updateGroups: MutationGroup[] = cartesianProduct(...matchingUpdates);
  // for each update group, combine it with add and delete to get each of the candidate mutation group
  if (updateGroups.length > 0) {
    return updateGroups.map((updateGroup: MutationGroup) => [
      ...updateGroup,
      ...addMutations,
      ...deleteMutations,
    ]);
  } else return [[...addMutations, ...deleteMutations]];
};

//#endregion

//#region Specification synthesis
// interface TaggedMutationSet {
//   tag: Mutation["tag"];
//   stmts: SubStmt[];
// }

// export const synthesizeConfig = (examples: SynthesizedSubstance[]) => {
//   // ): SynthesizerSetting => {
//   const ops: Mutation[] = examples
//     .map((ex: SynthesizedSubstance) => ex.ops)
//     .flat();

//   const grouped = groupBy(ops, "tag");

//   const taggedMutations: TaggedMutationSet[] = map(
//     grouped,
//     (value: Mutation[], key: Mutation["tag"]): TaggedMutationSet => {
//       const set: TaggedMutationSet = {
//         tag: key,
//         stmts: value.map(editedStmt),
//       };
//       return set;
//     }
//   ) as any; // TODO: resolve types: why is it `boolean[]`?

//   // console.log(showMutationSets(taggedMutations));

//   // TODO: finish this function
//   return {} as any;
// };

// const showMutationSets = (sets: TaggedMutationSet[]): string =>
//   sets.map(showMutationSet).join("\n\n");

// const showMutationSet = (set: TaggedMutationSet): string =>
//   `${set.tag}:\n${set.stmts.map(prettyStmt).join("\n")}`;

// const editedStmt = (mutation: Mutation): SubStmt => {
//   switch (mutation.tag) {
//     case "Replace":
//       return mutation.old;
//     case "Add":
//     case "Delete":
//     // case "Swap":
//     // case "ReplaceName":
//     case "TypeChange":
//       return mutation.stmt;
//   }
// };

//#endregion

import {
  prettyStmt,
  prettySubstance,
} from "@penrose/core/dist/compiler/Substance";
import {
  A,
  AbstractNode,
  Identifier,
  metaProps,
} from "@penrose/core/dist/types/ast";
import { Env, Type } from "@penrose/core/dist/types/domain";
import {
  LabelOption,
  SubExpr,
  SubProg,
  SubStmt,
} from "@penrose/core/dist/types/substance";
import _ from "lodash";
import rdiff from "recursive-diff";
import {
  cleanNode,
  findTypes,
  getStmt,
  intersection,
  mergeKindMaps,
  nodesEqual,
  sortStmts,
  subProg,
} from "../analysis/SubstanceAnalysis.js";
import {
  Add,
  Delete,
  Mutation,
  MutationGroup,
  addMutation,
  deleteMutation,
  enumerateProgMutations,
  enumerateStmtMutations,
  executeMutation,
} from "./Mutation.js";
import {
  SynthesisContext,
  WithContext,
  filterContext,
  initContext,
} from "./Synthesizer.js";

//#region Fine-grained diffs

type DiffType = AbstractNode["tag"];

export interface StmtDiff {
  diff: rdiff.rdiffResult;
  stmt: SubStmt<A>;
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
  source: SubStmt<A>;
  result: SubStmt<A>;
  rawDiff: rdiff.rdiffResult[];
  stmtDiff: StmtDiff[];
}

export interface AddDiff {
  diffType: "Add";
  source: SubStmt<A>;
}
export interface DeleteDiff {
  diffType: "Delete";
  source: SubStmt<A>;
}

/**
 * Compute the exact diffs between two Substance ASTs.
 *
 * @param left the original Substance program
 * @param right the changed Substance program
 * @returns a list of diffs
 */
export const diffSubProgs = (
  left: SubProg<A>,
  right: SubProg<A>,
): rdiff.rdiffResult[] => {
  const diffs: rdiff.rdiffResult[] = rdiff.getDiff(left, right);
  // remove all diffs related to meta-properties
  const concreteDiffs: rdiff.rdiffResult[] = diffs.filter(
    (diff: rdiff.rdiffResult) =>
      !diff.path.some((key: string | number) =>
        metaProps.includes(key.toString()),
      ),
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
export const diffSubStmts = (
  left: SubProg<A>,
  right: SubProg<A>,
): StmtDiff[] => {
  // normalize the statement orderings of both ASTs first
  const [leftSorted, rightSorted] = [sortStmts(left), sortStmts(right)];
  // compute the exact diffs between two normalized ASTs
  const exactDiffs: rdiff.rdiffResult[] = diffSubProgs(leftSorted, rightSorted);
  return exactDiffs.map((d) => toStmtDiff(d, leftSorted));
};

export type SubNode<T> =
  | LabelOption<T>
  | SubExpr<T>
  | SubProg<T>
  | SubStmt<T>
  | Type<T>;

const children = <T>(node: SubNode<T>): SubNode<T>[] => {
  switch (node.tag) {
    case "ApplyConstructor":
    case "ApplyFunction":
    case "ApplyPredicate":
    case "Func":
    case "TypeConstructor": {
      return [node.name, ...node.args];
    }
    case "AutoLabel": {
      return [node.option];
    }
    case "Bind": {
      return [node.variable, node.expr];
    }
    case "Decl": {
      return [node.type, node.name];
    }
    case "Deconstructor": {
      return [node.variable, node.field];
    }
    case "DefaultLabels":
    case "Identifier":
    case "Prop":
    case "StringLit": {
      return [];
    }
    case "EqualExprs":
    case "EqualPredicates": {
      return [node.left, node.right];
    }
    case "LabelDecl": {
      return [node.variable, node.label];
    }
    case "LabelIDs": {
      return [...node.variables];
    }
    case "NoLabel": {
      return [...node.args];
    }
    case "SubProg": {
      return [...node.statements];
    }
    case "TypeVar": {
      return [node.name];
    }
  }
};

/**
 * Determine if two Substance AST nodes are similar. The metric is whether the nodes have common descendents or are equal themselves.
 *
 * @param left a node in the Substance AST
 * @param right a node in the Substance AST
 * @returns if the nodes have common descendents
 */
export const similarNodes = (left: SubNode<A>, right: SubNode<A>): boolean => {
  const equalNodes = nodesEqual(left, right);
  const similarChildren = _.intersectionWith(
    children(left),
    children(right),
    similarNodes,
  );

  const similarLeft = _.intersectionWith([left], children(right), similarNodes);
  const similarRight = _.intersectionWith(
    children(left),
    [right],
    similarNodes,
  );

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
  source: SubStmt<A>;
  similarStmts: SubStmt<A>[];
}

export const subProgDiffs = (left: SubProg<A>, right: SubProg<A>): DiffSet => {
  const commonStmts = intersection(left, right);
  const leftFiltered = left.statements.filter((a) => {
    return _.intersectionWith(commonStmts, [a], nodesEqual).length === 0;
  });
  const rightFiltered = right.statements.filter((a) => {
    return _.intersectionWith(commonStmts, [a], nodesEqual).length === 0;
  });
  const similarMap = similarMappings(leftFiltered, rightFiltered);

  const update: UpdateDiff[] = updateDiffs(similarMap);
  const updatedSource: SubStmt<A>[] = update.map((d) => d.source);
  const updatedResult: SubStmt<A>[] = update.map((d) => d.result);
  // console.log(rightFiltered.map(prettyStmt).join("\n"));
  // console.log(updatedResult.map(prettyStmt).join("\n"));

  const deleted: DeleteDiff[] = leftFiltered
    .filter(
      (s) => _.intersectionWith([s], updatedSource, nodesEqual).length === 0,
    )
    .map((s) => ({ diffType: "Delete", source: s }));
  const add: AddDiff[] = rightFiltered
    .filter(
      (s) => _.intersectionWith([s], updatedResult, nodesEqual).length === 0,
    )
    .map((s) => ({ diffType: "Add", source: s }));
  return { update, add, delete: deleted };
};

export const showSubDiff = (d: SubDiff): string => {
  switch (d.diffType) {
    case "Add":
    case "Delete":
      return `${d.diffType}: ${prettyStmt(d.source)}`;
    case "Update":
      return `${d.diffType}: ${prettyStmt(d.source)} -> ${prettyStmt(
        d.result,
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
  picked: SubStmt<A>[],
  { similarStmts, source }: SimilarMapping,
): UpdateDiff | undefined => {
  const result = _.difference(similarStmts, picked)[0]; // TODO: insert ranking algorithm here
  if (result === undefined) return undefined;
  const rawDiffs = rdiff.getDiff(cleanNode(source), cleanNode(result));
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
  leftSet: SubStmt<A>[],
  rightSet: SubStmt<A>[],
): SimilarMapping[] => {
  const mappings: SimilarMapping[] = [];
  for (const stmt of leftSet) {
    const similarSet: SubStmt<A>[] = rightSet.filter((s) =>
      similarNodes(stmt, s),
    );
    if (similarSet.length > 0)
      mappings.push({
        source: stmt,
        // HACK: for consistent ordering
        similarStmts: _.sortBy(similarSet, prettyStmt),
      });
  }
  return mappings;
};

export const rawToStmtDiff = (
  diff: rdiff.rdiffResult,
  source: SubStmt<A>,
): StmtDiff => {
  const { path } = diff;
  const originalValue = _.get(source, path);
  return {
    diff,
    stmt: source,
    // diffType: diffType(source, stmtDiff),
    originalValue,
  };
};
export const toStmtDiff = (
  diff: rdiff.rdiffResult,
  ast: SubProg<A>,
): StmtDiff => {
  const [, stmtIndex, ...path] = diff.path;
  // TODO: encode the paths to AST in a more principled way
  const stmt = getStmt(ast, stmtIndex as number);
  const originalValue = _.get(stmt, path);
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
const diffType = (node: AbstractNode, diff: rdiff.rdiffResult): DiffType => {
  let tag = undefined;
  let currNode: any = node;
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

export const applyStmtDiff = (
  prog: SubProg<A>,
  stmtDiff: StmtDiff,
): SubProg<A> => {
  const { diff, stmt } = stmtDiff;
  return {
    ...prog,
    statements: prog.statements.map((s: SubStmt<A>) => {
      if (s === stmt) {
        const res: SubStmt<A> = rdiff.applyDiff(_.cloneDeep(s), [diff]);
        return res;
      } else return s;
    }),
  };
};

export const swapDiffID = (d: StmtDiff, id: Identifier<A>): StmtDiff => {
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
  prog: SubProg<A>,
  diffs: StmtDiff[],
): SubProg<A> => ({
  ...prog,
  statements: prog.statements.map((stmt: SubStmt<A>) =>
    rdiff.applyDiff(stmt, findDiffs(stmt, diffs)),
  ),
});

export const findDiffs = (
  stmt: SubStmt<A>,
  diffs: StmtDiff[],
): rdiff.rdiffResult[] =>
  diffs.filter((d) => d.stmt === stmt).map((d) => d.diff);

//#endregion

//#region Single-mutation search

export const cartesianProduct = <T>(...sets: T[][]): T[][] =>
  sets.reduce<T[][]>(
    (accSets, set) =>
      accSets.flatMap((accSet) => set.map((value) => [...accSet, value])),
    [[]],
  );

/**
 * Given two Substance programs, find possible mutation paths that transform from `src` to `dest`.
 *
 * @param src The source Substance program
 * @param dest The changed Substance program
 * @param srcEnv The environment for the source Substance program
 */
export const findMutationPaths = (
  src: SubProg<A>,
  dest: SubProg<A>,
  srcEnv: Env,
): MutationGroup[] => {
  const diffs: DiffSet = subProgDiffs(src, dest);
  // pack add and delete mutations
  const addMutations: Add[] = diffs.add.map((a) => addMutation(a.source));
  const deleteMutations: Delete[] = diffs.delete.map((a) =>
    deleteMutation(a.source),
  );
  // find all possible updates for each statement in the update set
  const matchingUpdates: MutationGroup[] = diffs.update.map((d) => {
    // COMBAK: check random seed
    const cxt = initContext(
      srcEnv,
      "existing",
      "distinct",
      "findMutationPaths",
    );
    const mutations = enumerateStmtMutations(d.source, src, cxt);
    d.source;
    const matchedMutations = mutations.filter((m) => {
      // HACK: assumes each update pair is connected by only one mutation. Therefore packing the source and result stmts into individual programs
      const prog1 = subProg([d.source]);
      const prog2 = subProg([d.result]);
      const { res: mutatedAST } = executeMutation(m, prog1, cxt);
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

export const enumerateAllPaths = (
  src: SubProg<A>,
  dest: SubProg<A>,
  srcEnv: Env,
): MutationGroup[] => {
  const diffs: DiffSet = subProgDiffs(src, dest);
  // pack add and delete mutations
  const addMutations: Add[] = diffs.add.map((a) => addMutation(a.source));
  const deleteMutations: Delete[] = diffs.delete.map((a) =>
    deleteMutation(a.source),
  );
  // find all possible updates for each statement in the update set
  // COMBAK: check random seed
  const cxt = initContext(srcEnv, "existing", "distinct", "enumerateAllPaths");
  const possibleUpdates: MutationGroup[] = diffs.update.map((d) =>
    enumerateStmtMutations(d.source, src, cxt),
  );

  // any combination of candidate mutations for each stmt will be a valid mutation group
  const updateGroups: MutationGroup[] = cartesianProduct(...possibleUpdates);
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

//#region Enumerative search with observational equivalence

interface MutatedSubProg {
  prog: SubProg<A>;
  cxt: SynthesisContext;
  mutations: Mutation[];
}

/**
 * @param srcProg the source Substance program
 * @param destStmt the result Substance program
 * @param initCxt the current synthesis context
 * @param maxDepth maximum number of mutations per path
 * @returns a list of mutation paths that tranform from `srcStmt` to `destStmt`, up to `maxDepth`
 */
export const enumerateMutationPaths = (
  srcProg: SubProg<A>,
  destProg: SubProg<A>,
  initCxt: SynthesisContext,
  maxDepth: number,
): MutatedSubProg[] => {
  // optimization: pre-filter the environment by the source and target program
  // NOTE: we need both the source and target programs because the synthesizer config includes __both__ types to match and candidate types
  const srcAndDestTypes = mergeKindMaps(
    findTypes(srcProg),
    findTypes(destProg),
  );
  const filteredCxt = filterContext(initCxt, srcAndDestTypes);
  // console.log(showEnv(initCxt.env));
  // console.log(showEnv(filteredCxt.env));
  const startProg: MutatedSubProg = {
    prog: srcProg,
    mutations: [],
    // cxt: initCxt,
    cxt: filteredCxt,
  };
  // a list of _unique_ Substance programs so far
  let candidates: MutatedSubProg[] = [startProg];
  // all correct paths up to `maxDepth`
  let matchedCandidates: MutatedSubProg[] = [];
  let currentDepth = 0;
  while (currentDepth < maxDepth) {
    // grow all candidates by one
    candidates = candidates
      .map(
        ({ prog: progToGrow, mutations: pathToGrow, cxt }: MutatedSubProg) => {
          // enumerate all mutations for this candidate
          const possibleMutations: Mutation[] = enumerateProgMutations(
            progToGrow,
            cxt,
          );
          // execute all mutations and get resulting programs
          const resultProgs: WithContext<SubProg<A>>[] = possibleMutations.map(
            (m) => executeMutation(m, progToGrow, cxt),
          );
          // pack the resulting programs with their updated mutation path
          return _.zipWith(
            resultProgs,
            possibleMutations,
            (p, mutation): MutatedSubProg => ({
              prog: p.res,
              cxt: p.ctx,
              mutations: [...pathToGrow, mutation],
            }),
          );
        },
      )
      .flat();
    // "Observational equivalence": remove all candidates that lead to the same output
    candidates = _.uniqBy(candidates, (c) => prettySubstance(c.prog));
    // HACK: using string-based check instead of AST check
    const matches = candidates.filter(
      (p) => prettySubstance(p.prog) === prettySubstance(destProg),
    );
    // collect all the correct paths at this depth
    if (matches.length > 0) {
      matchedCandidates = [...matchedCandidates, ...matches];
    }
    currentDepth++;
  }
  return matchedCandidates;
};

//#endregion

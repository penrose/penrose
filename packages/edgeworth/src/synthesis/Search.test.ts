import { compileDomain } from "@penrose/core/dist/compiler/Domain";
import {
  compileSubstance,
  prettyStmt,
  prettySubstance,
} from "@penrose/core/dist/compiler/Substance";
import { A } from "@penrose/core/dist/types/ast";
import { SubProg, SubRes } from "@penrose/core/dist/types/substance";
import { showError } from "@penrose/core/dist/utils/Error";
import _ from "lodash";
import pc from "pandemonium/choice";
import rdiff from "recursive-diff";
import seedrandom from "seedrandom";
import { sortStmts, typeOf } from "../analysis/SubstanceAnalysis";
import {
  enumerateStmtMutations,
  executeMutation,
  executeMutations,
  showMutations,
} from "./Mutation";
import {
  DiffSet,
  StmtDiff,
  applyStmtDiffs,
  diffSubProgs,
  diffSubStmts,
  enumerateMutationPaths,
  findMutationPaths,
  showStmtDiff,
  showSubDiff,
  subProgDiffs,
  swapDiffID,
} from "./Search";
import { initContext } from "./Synthesizer";

const RNG = seedrandom("seed5");
const choice: <T>(array: Array<T>) => T = pc.createChoice(RNG);

const domainSrc = `
type Set
type Point
type Map

constructor Singleton(Point p) -> Set

function Intersection(Set a, Set b) -> Set
function Union(Set a, Set b) -> Set
function Subtraction(Set a, Set b) -> Set
function CartesianProduct(Set a, Set b) -> Set
function Difference(Set a, Set b) -> Set
function Subset(Set a, Set b) -> Set
function AddPoint(Point p, Set s1) -> Set

predicate Not(Prop p1)
predicate From(Map f, Set domain, Set codomain)
predicate Empty(Set s)
predicate Intersecting(Set s1, Set s2)
predicate IsSubset(Set s1, Set s2)
predicate Equal(Set s1, Set s2)
predicate PointIn(Set s, Point p)
predicate In(Point p, Set s)
predicate Injection(Map m)
predicate Surjection(Map m)
predicate Bijection(Map m)
predicate PairIn(Point, Point, Map)
`;

const getSubRes = (domainSrc: string, substanceSrc: string): SubRes => {
  const envOrError = compileDomain(domainSrc);
  if (envOrError.isOk()) {
    const env = envOrError.value;
    const subRes = compileSubstance(substanceSrc, env);
    if (subRes.isOk()) {
      const subResult = subRes.value;
      return subResult;
    } else {
      throw new Error(
        `Error when compiling the Substance program: ${showError(subRes.error)}`
      );
    }
  } else {
    throw new Error(
      `Error when compiling the Domain program:\n${showError(envOrError.error)}`
    );
  }
};
describe("AST diff tests", () => {
  test("Compute AST diff based on tree sets", () => {
    const original = `
    Set A, B, C, D, E
    D := Union(A, B)
    IsSubset(B, A)
    IsSubset(C, A)
    Equal(E, E)
    `;
    const edited = `
    Set A, B, C, D, E
    IsSubset(B, A)
    IsSubset(D, A)
    E := Union(A, B)
    `;
    const res1: SubRes = getSubRes(domainSrc, original);
    const ast1: SubProg<A> = res1[0].ast;
    const ast2: SubProg<A> = getSubRes(domainSrc, edited)[0].ast;
    const d: DiffSet = subProgDiffs(ast1, ast2);
    expect([...d.add, ...d.delete, ...d.update].map(showSubDiff)).toEqual([
      "Delete: Equal(E, E)",
      "Update: D := Union(A, B) -> E := Union(A, B)\n\tChanged D := Union(A, B) : D (variable,value) -> E",
      "Update: IsSubset(C, A) -> IsSubset(D, A)\n\tChanged IsSubset(C, A) : C (args,0,value) -> D",
    ]);
  });

  test("applying AST diff with id swap", () => {
    const prog1 = `
    Set A, B, C, D, E, F, Z
    IsSubset(B, A)
    IsSubset(C, A)
    -- D := Union(A, B)
    Z := Union(A, B) -- This will mess up the ordering
    `;
    const prog2 = `
    Set A, B, C, D, E, F, Z
    IsSubset(B, A)
    IsSubset(D, A)
    F := Union(A, B)
    `;
    const res1: SubRes = getSubRes(domainSrc, prog1);
    const ast1: SubProg<A> = res1[0].ast;
    const ast2: SubProg<A> = getSubRes(domainSrc, prog2)[0].ast;
    const diffs: StmtDiff[] = diffSubStmts(ast1, ast2);
    expect(diffs).toHaveLength(2);
    expect(diffs.map(showStmtDiff)).toEqual([
      "Changed IsSubset(C, A) (Identifier): C (args,0,value) -> D",
      "Changed Z := Union(A, B) (Identifier): Z (variable,value) -> F",
    ]);
    const env = res1[1];
    const ids = env.varIDs;
    const swappedDiffs: StmtDiff[] = diffs.map((d: StmtDiff) => {
      if (d.diffType === "Identifier") {
        const matchingIDs = ids.filter(
          (id) => typeOf(id.value, env) === typeOf(d.diff.val, env)
        );
        const choices = matchingIDs.filter(
          (id) => ![d.diff.val, d.originalValue].includes(id.value)
        );
        return swapDiffID(d, choice(choices));
      } else return d;
    });
    const original = prettySubstance(ast1);
    // TODO: check if applystmtdiffs mutates the AST
    const res = applyStmtDiffs(ast1, swappedDiffs);
    expect(prettySubstance(res)).not.toEqual(original);
    // TODO: add more detailed assertions
  });

  test("applying AST diff regardless of stmt ordering", () => {
    const prog1 = `
    Set A, B, C
    IsSubset(A,B)
    IsSubset(C, A)
    `;
    const prog2 = `
    Set A, B, C
    IsSubset(C,A)
    IsSubset(B, A)
    `;
    const ast1: SubProg<A> = getSubRes(domainSrc, prog1)[0].ast;
    const ast2: SubProg<A> = getSubRes(domainSrc, prog2)[0].ast;
    const diffs: StmtDiff[] = diffSubStmts(ast1, ast2);
    // the ASTs have normalized ordering, so there should be only two diffs
    expect(diffs).toHaveLength(2);
    // console.log(diffs.map(showStmtDiff).join("\n"));

    // apply the stmt diffs, grouped by target statements
    const ast2From1 = applyStmtDiffs(ast1, diffs);
    // the result should be semantically equivalent to the second program
    expect(prettySubstance(sortStmts(ast2From1))).toEqual(
      prettySubstance(sortStmts(ast2))
    );
    // ...but different in the source because the diff is applied to the 2nd statement
    expect(prettySubstance(ast2From1)).not.toEqual(prettySubstance(ast2));
  });
  test("applying exact AST diff", () => {
    const prog1 = `
    Set A, B, C
    IsSubset(C, A)
    IsSubset(A,B)
    `;
    const prog2 = `
    Set A, B, C
    IsSubset(C,A)
    IsSubset(B, A)
    `;
    const ast1: SubProg<A> = getSubRes(domainSrc, prog1)[0].ast;
    const ast2: SubProg<A> = getSubRes(domainSrc, prog2)[0].ast;
    const diffs: rdiff.rdiffResult[] = diffSubProgs(ast1, ast2);
    // because we filtered out all the noisy diffs, the exact formatting should not matter much
    expect(diffs).toHaveLength(2);
    // NOTE: `applyDiff` actually mutates the object :(
    const ast2From1 = rdiff.applyDiff(_.cloneDeep(ast1), diffs);
    // the exact objects won't be equal, because the ASTs don't have the same positional info
    expect(ast2From1).not.toEqual(ast2);
    // ...but the ASTs are actually the same, which can be shown by checking the trees
    expect(prettySubstance(ast2From1)).toEqual(prettySubstance(ast2));
    expect(prettySubstance(ast2From1)).not.toEqual(prettySubstance(ast1));
  });
});

describe("Mutation recognition tests", () => {
  test("recognizing swap mutation - auto", () => {
    const prog1 = `
    Set A, B, C
    IsSubset(A,B)
    IsSubset(C, A)
    `;
    const prog2 = `
    Set A, B, C
    IsSubset(C,A)
    IsSubset(B, A)
    `;
    const [subEnv, env] = getSubRes(domainSrc, prog1);
    const ast1: SubProg<A> = subEnv.ast;
    const ast2: SubProg<A> = getSubRes(domainSrc, prog2)[0].ast;
    const mutationGroups = findMutationPaths(ast1, ast2, env);
    expect(mutationGroups.map(showMutations)).toContain(
      "Swap arguments 0 and 1 of IsSubset(A, B)"
    );
  });
  test("recognizing swap mutation - stepwise", () => {
    const prog1 = `
    Set A, B, C
    IsSubset(A,B)
    IsSubset(C, A)
    `;
    const prog2 = `
    Set A, B, C
    IsSubset(C,A)
    IsSubset(B, A)
    `;
    const [subEnv, env] = getSubRes(domainSrc, prog1);
    const ast1: SubProg<A> = subEnv.ast;
    const ast2: SubProg<A> = getSubRes(domainSrc, prog2)[0].ast;
    // find diffs between ASTs, which should only have updates
    const diffs: DiffSet = subProgDiffs(ast1, ast2);
    expect(diffs.add).toHaveLength(0);
    expect(diffs.delete).toHaveLength(0);
    expect(diffs.update).toHaveLength(1);
    expect(prettyStmt(diffs.update[0].source)).toEqual("IsSubset(A, B)");
    expect(prettyStmt(diffs.update[0].result)).toEqual("IsSubset(B, A)");
    // get all the updated statements from ast1. There should be only one statement changed
    const fromSet = diffs.update.map((d) => d.source);
    expect(fromSet).toHaveLength(1);
    // enumerate all mutations for the statement
    const swappedPred = fromSet[0];
    const ctx = initContext(env, "existing", "distinct", "test0");
    const mutations = enumerateStmtMutations(swappedPred, ast1, ctx);
    // apply each mutation and see how many of them match with the result
    const matchedMutations = mutations.filter((m) => {
      const { res: mutatedAST } = executeMutation(m, ast1, ctx);
      return (
        prettySubstance(sortStmts(ast2)) ===
        prettySubstance(sortStmts(mutatedAST))
      );
    });
    expect(matchedMutations).toHaveLength(1);
    expect(matchedMutations[0].tag).toEqual("SwapStmtArgs");
  });
  test("recognizing swap mutation with noise - auto", () => {
    const prog1 = `
    Set A, B, C
    IsSubset(A,B)
    IsSubset(C, A)
    `;
    const prog2 = `
    Set A, B, C, D, E
    IsSubset(B, A)
    Equal(D, E)
    `;
    const [subEnv, env] = getSubRes(domainSrc, prog1);
    const ast1: SubProg<A> = subEnv.ast;
    const ast2: SubProg<A> = getSubRes(domainSrc, prog2)[0].ast;
    const mutationGroups = findMutationPaths(ast1, ast2, env);
    // since there's only one possible update, there should be only one path
    expect(mutationGroups).toHaveLength(1);
    // the path should have two adds, one delete, and an update
    expect(mutationGroups[0].filter((m) => m.tag === "Add")).toHaveLength(3);
    expect(mutationGroups[0].filter((m) => m.tag === "Delete")).toHaveLength(1);
    expect(
      mutationGroups[0].filter((m) => m.tag === "SwapStmtArgs")
    ).toHaveLength(1);
    const { res: ast2from1 } = executeMutations(
      mutationGroups[0],
      ast1,
      initContext(env, "existing", "distinct", "test1")
    );
    expect(prettySubstance(sortStmts(ast2from1))).toEqual(
      prettySubstance(sortStmts(ast2))
    );
  });
  test("recognizing multiple mutations on multiple stmts", () => {
    const prog1 = `
    Set A, B, C
    IsSubset(A,B)
    C := Intersection(A, B)
    `;
    const prog2 = `
    Set A, B, C
    Equal(B, A)
    Intersecting(A, B)
    `;
    const [subEnv, env] = getSubRes(domainSrc, prog1);
    const ast1: SubProg<A> = subEnv.ast;
    const ast2: SubProg<A> = getSubRes(domainSrc, prog2)[0].ast;
    const ctx = initContext(env, "existing", "distinct", "test2");
    const paths = enumerateMutationPaths(ast1, ast2, ctx, 5);
    const shortestPath = _.minBy(paths, (p) => p.mutations.length);
    expect(shortestPath?.mutations).toHaveLength(3);
    expect(shortestPath?.mutations.map((m) => m.tag)).toContain("SwapStmtArgs");
    expect(shortestPath?.mutations.map((m) => m.tag)).toContain(
      "ChangeExprType"
    );
    expect(shortestPath?.mutations.map((m) => m.tag)).toContain(
      "ReplaceStmtName"
    );

    // console.log(
    //   paths.map((p) => [prettySubstance(p.prog), showMutations(p.mutations)])
    // );
  });
  test("recognizing multiple mutations on one stmt", () => {
    const prog1 = `
    Set A, B, C
    IsSubset(A,B)
    `;
    const prog2 = `
    Set A, B, C
    Equal(B, A)
    `;
    const [subEnv, env] = getSubRes(domainSrc, prog1);
    const ast1: SubProg<A> = subEnv.ast;
    const ast2: SubProg<A> = getSubRes(domainSrc, prog2)[0].ast;
    const ctx = initContext(env, "existing", "distinct", "test3");
    const paths = enumerateMutationPaths(ast1, ast2, ctx, 10);
    const twoStepPaths = paths.filter((p) => p.mutations.length === 2);
    // since we use observational equivalence, there should be only one path
    expect(twoStepPaths).toHaveLength(1);
    // the output should be the same as the
    expect(prettySubstance(twoStepPaths[0].prog)).toEqual(
      prettySubstance(ast2)
    );
    // there should be at least a swap operation
    expect(twoStepPaths[0].mutations.map((m) => m.tag)).toContain(
      "SwapStmtArgs"
    );
    // console.log(
    //   paths.map((p) => [prettySubstance(p.prog), showMutations(p.mutations)])
    // );
  });
});

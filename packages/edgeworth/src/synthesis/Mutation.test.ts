// @vitest-environment jsdom

import { compileDomain } from "@penrose/core";
import {
  compileSubstance,
  prettyStmt,
} from "@penrose/core/dist/compiler/Substance";
import { DomainEnv } from "@penrose/core/dist/types/domain";
import { SubstanceEnv } from "@penrose/core/dist/types/substance";
import { showError } from "@penrose/core/dist/utils/Error";
import { describe, expect, test } from "vitest";
import { enumerateStmtMutations } from "./Mutation.js";
import { initContext } from "./Synthesizer.js";

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

predicate From(Map f, Set domain, Set codomain)
predicate Empty(Set s)
predicate Intersecting(Set s1, Set s2)
predicate IsSubset(Set s1, Set s2)
predicate Equal(Set s1, Set s2)
predicate Equal3(Set s1, Set s2, Set s3)
predicate PointIn(Set s, Point p)
predicate In(Point p, Set s)
predicate Injection(Map m)
predicate Surjection(Map m)
predicate Bijection(Map m)
predicate PairIn(Point, Point, Map)
`;

const getSubRes = (
  domainSrc: string,
  substanceSrc: string,
): [SubstanceEnv, DomainEnv] => {
  const envOrError = compileDomain(domainSrc);
  if (envOrError.isOk()) {
    const domEnv = envOrError.value;
    const subRes = compileSubstance(substanceSrc, domEnv);
    if (subRes.isOk()) {
      const subEnv = subRes.value;
      return [subEnv, domEnv];
    } else {
      throw new Error(
        `Error when compiling the Substance program: ${showError(
          subRes.error,
        )}`,
      );
    }
  } else {
    throw new Error(
      `Error when compiling the Domain program:\n${showError(
        envOrError.error,
      )}`,
    );
  }
};

describe("Mutation enumeration", () => {
  test("enumerate all edit mutations", () => {
    const subSrc = `
    Set A, B, C
    Equal3(A, B, C)
    IsSubset(B, C)
    `;
    const [subEnv, env] = getSubRes(domainSrc, subSrc);
    const pred1 = subEnv.ast.statements[3];
    expect(prettyStmt(pred1)).toEqual("Equal3(A, B, C)");
    const cxt = initContext(env, subEnv, "existing", "distinct", "enumTest");
    const mutations1 = enumerateStmtMutations(pred1, subEnv.ast, cxt);
    // Equal3 should only have 3 swap mutations
    expect(
      mutations1.map((m) => m.tag).every((t) => t === "SwapStmtArgs"),
    ).toEqual(true);
    expect(mutations1).toHaveLength(3);
    // IsSubset will have one swap, two replace names, and a bumch of changetypes
    // const pred2 = subEnv.ast.statements[4];
    // expect(prettyStmt(pred2)).toEqual("IsSubset(B, C)");
    // const mutations2 = enumerateMutations(pred2, subEnv.ast, cxt);
  });
});

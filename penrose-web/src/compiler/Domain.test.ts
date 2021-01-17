import * as nearley from "nearley";
import grammar from "parser/DomainParser";
import * as path from "path";
import * as fs from "fs";
import { checkDomain, CheckerResult, isSubtypeOf } from "compiler/Domain";
import { showDomainErr } from "utils/Error";
import { compile } from "moo";
import { type } from "os";

const compileDomain = (prog: string) => {
  const { results } = parser.feed(prog);
  return checkDomain(results[0]);
};

const contextHas = (
  res: CheckerResult,
  expectedTypes: string[],
  expectedConstructors: string[],
  expectedFunctions: string[],
  expectedPredicates: string[]
) => {
  if (res.isOk()) {
    const { types, typeVars, constructors, functions, predicates } = res.value;
    expect(typeVars.size).toBe(0);
    expectedTypes.map((t) => expect(types.has(t)).toBe(true));
    expectedConstructors.map((c) => expect(constructors.has(c)).toBe(true));
    expectedFunctions.map((f) => expect(functions.has(f)).toBe(true));
    expectedPredicates.map((p) => expect(predicates.has(p)).toBe(true));
  } else {
    fail(showDomainErr(res.error));
  }
};

let parser: nearley.Parser;
beforeEach(() => {
  // NOTE: Neither `feed` nor `finish` will reset the parser state. Therefore recompiling before each unit test
  parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
});

describe("Common", () => {
  test("finding subtypes", () => {
    const prog = `
  type A
  type B
  type C
  type D
  type E
  C <: B
  B <: A
  value varA: A
  value varB: B
  value varC: C
 `;
    const res = compileDomain(prog);
    if (res.isOk()) {
      const env = res.value;
      const typeA = env.preludeValues.get("varA")!;
      const typeB = env.preludeValues.get("varB")!;
      const typeC = env.preludeValues.get("varC")!;
      expect(isSubtypeOf(typeB, typeA, env)).toBe(true);
      expect(isSubtypeOf(typeC, typeB, env)).toBe(true);
      expect(isSubtypeOf(typeC, typeA, env)).toBe(true);
      expect(isSubtypeOf(typeA, typeA, env)).toBe(true);
      expect(isSubtypeOf(typeB, typeB, env)).toBe(true);
      expect(isSubtypeOf(typeA, typeC, env)).toBe(false);
      expect(isSubtypeOf(typeA, typeB, env)).toBe(false);
    } else {
      fail(showDomainErr(res.error));
    }
  });
});

describe("Statements", () => {
  test("type and constructor decl", () => {
    const prog = `
type List ('T)
type Real
type Interval
type OpenInterval
type ClosedInterval
constructor Cons ['X] : 'X head * List('X) tail -> List('X)
constructor Nil['X] -> List('X)
constructor CreateInterval: Real left * Real right -> Interval
constructor CreateOpenInterval: Real left * Real right -> OpenInterval
constructor CreateClosedInterval: Real left * Real right -> ClosedInterval
    `;
    const res = compileDomain(prog);
    const types = [
      "String",
      "List",
      "Real",
      "Interval",
      "OpenInterval",
      "ClosedInterval",
    ];
    const constructors = [
      "Cons",
      "Nil",
      "CreateInterval",
      "CreateOpenInterval",
      "CreateClosedInterval",
    ];
    contextHas(res, types, constructors, [], []);
  });
  test("predicate decl", () => {
    const prog = `
type Map
type Set
type Point
predicate Not : Prop p1
predicate From : Map f * Set domain * Set codomain
predicate Empty : Set s
predicate Intersecting : Set s1 * Set s2
predicate IsSubset : Set s1 * Set s2
predicate PointIn : Set s * Point p
predicate In : Point p * Set s
predicate Injection : Map m
predicate Surjection : Map m
predicate Bijection : Map m
predicate PairIn : Point * Point * Map
    `;
    const res = compileDomain(prog);
    const types = ["Map", "Set", "Point"];
    const predicates = [
      "Not",
      "From",
      "Empty",
      "Intersecting",
      "IsSubset",
      "PointIn",
      "In",
      "Injection",
      "Surjection",
      "Bijection",
      "PairIn",
    ];
    contextHas(res, types, [], [], predicates);
  });
});

describe("Errors", () => {
  const expectErrorOf = (prog: string, errorType: string) => {
    const result = compileDomain(prog);
    if (result.isErr()) {
      console.log(showDomainErr(result.error));
      expect(result.error.tag).toBe(errorType);
    } else {
      fail(`Error ${errorType} was suppoed to occur.`);
    }
  };
  test("Duplicate names", () => {
    const prog = `
type Set 
type Point 
type Set
    `;
    expectErrorOf(prog, "DuplicateName");
  });
  test("Type not found", () => {
    const prog = `
constructor Cons ['X] : 'X head * List('X) tail -> List('X)
    `;
    expectErrorOf(prog, "TypeNotFound");
  });
  test("type var not found", () => {
    const prog = `
  constructor Cons ['X] : 'Z head * List('Y) tail -> List('X)
    `;
    expectErrorOf(prog, "TypeVarNotFound");
  });
  test("prop in subtyping relation", () => {
    const prog = `
  type Set
  Prop <: Set
    `;
    expectErrorOf(prog, "NotTypeConsInSubtype");
  });
  test("type var in subtyping relation", () => {
    const prog = `
  'T <: 'V
    `;
    expectErrorOf(prog, "NotTypeConsInSubtype");
  });
  test("subtype cycle", () => {
    const prog = `
  type A
  type B
  type C
  type D
  type E
  C <: B
  B <: A
  A <: C
  D <: C
  E <: D
  D <: E
    `;
    expectErrorOf(prog, "CyclicSubtypes");
  });
});

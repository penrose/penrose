import * as nearley from "nearley";
import grammar from "parser/DomainParser";
import * as path from "path";
import * as fs from "fs";
import {
  checkDomain,
  errorString,
  DomainEnv,
  CheckerResult,
} from "compiler/Domain";
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
    fail(errorString(res.error));
  }
};

let parser: nearley.Parser;
beforeEach(() => {
  // NOTE: Neither `feed` nor `finish` will reset the parser state. Therefore recompiling before each unit test
  parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
});

describe("Common", () => {
  test("empty program", () => {
    const { results } = parser.feed("");
  });
  test("comments", () => {
    const prog = `
type Set 
type Point 
type Set
    `;
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
      expect(compileDomain(prog).unsafelyUnwrapErr().tag).toBe(errorType);
      console.log(errorString(result.error));
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
  test("TypeVarNotFound", () => {
    const prog = `
constructor Cons ['X] : 'Z head * List('Y) tail -> List('X)
    `;
    expectErrorOf(prog, "TypeVarNotFound");
  });
});

import linearAlgebra from "@penrose/examples/dist/linear-algebra-domain";
import setTheory from "@penrose/examples/dist/set-theory-domain";
import * as fs from "fs";
import nearley from "nearley";
import * as path from "path";
import { beforeEach, describe, expect, test } from "vitest";
import grammar from "../parser/DomainParser";
import { Env } from "../types/domain";
import { PenroseError } from "../types/errors";
import { Result, showError } from "../utils/Error";
import { compileDomain, isSubtype } from "./Domain";

const outputDir = "/tmp/contexts";
const saveContexts = false;
const printError = false;

const domains = [
  [
    "linear-algebra-domain/linear-algebra.domain",
    linearAlgebra["linear-algebra.domain"],
  ],
  ["set-theory-domain/setTheory.domain", setTheory["setTheory.domain"]],
];

const contextHas = (
  res: Result<Env, PenroseError>,
  expectedTypes: string[],
  expectedConstructors: string[],
  expectedFunctions: string[],
  expectedPredicates: string[]
) => {
  if (res.isOk()) {
    const { types, typeVars, constructors, functions, predicates } = res.value;
    expect(typeVars.size).toBe(0);
    expectedTypes.forEach((t) => expect(types.has(t)).toBe(true));
    expectedConstructors.forEach((c) => expect(constructors.has(c)).toBe(true));
    expectedFunctions.forEach((f) => expect(functions.has(f)).toBe(true));
    expectedPredicates.forEach((p) => expect(predicates.has(p)).toBe(true));
  } else {
    throw Error(showError(res.error));
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
  type C <: B
  type D
  type E
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
      expect(isSubtype(typeB, typeA, env)).toBe(true);
      expect(isSubtype(typeC, typeB, env)).toBe(true);
      expect(isSubtype(typeC, typeA, env)).toBe(true);
      expect(isSubtype(typeA, typeA, env)).toBe(true);
      expect(isSubtype(typeB, typeB, env)).toBe(true);
      expect(isSubtype(typeA, typeC, env)).toBe(false);
      expect(isSubtype(typeA, typeB, env)).toBe(false);
    } else {
      throw Error(showError(res.error));
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
constructor Cons ['X] ('X head, List('X) tail) -> List('X)
constructor Nil['X]() -> List('X)
constructor CreateInterval(Real left, Real right) -> Interval
constructor CreateOpenInterval(Real left, Real right) -> OpenInterval
constructor CreateClosedInterval(Real left, Real right) -> ClosedInterval
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
predicate Not(Prop p1)
predicate From(Map f, Set domain, Set codomain)
predicate Empty(Set s)
predicate Intersecting(Set s1, Set s2)
predicate IsSubset(Set s1, Set s2)
predicate PointIn(Set s, Point p)
predicate In(Point p, Set s)
predicate Injection(Map m)
predicate Surjection(Map m)
predicate Bijection(Map m)
predicate PairIn(Point, Point, Map)
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
  test("symmetric predicate decl", () => {
    const prog = `
type MyType
type MySubType
predicate MyNormalPredicate(MyType a, MyType b)
symmetric predicate MyExcellentPredicate1(MyType a, MyType b)
symmetric predicate MyExcellentPredicate2(MySubType, MySubType)
    `;
    const res = compileDomain(prog);
    const predicates = [
      "MyNormalPredicate",
      "MyExcellentPredicate1",
      "MyExcellentPredicate2",
    ];
    contextHas(res, [], [], [], predicates);
    expect(res.isOk()).toEqual(true);
    if (res.isOk()) {
      let env = res.value;
      expect(env.predicates.get("MyNormalPredicate")!.symmetric).toEqual(false);
      expect(env.predicates.get("MyExcellentPredicate1")!.symmetric).toEqual(
        true
      );
      expect(env.predicates.get("MyExcellentPredicate2")!.symmetric).toEqual(
        true
      );
    }
  });
});

describe("Errors", () => {
  const expectErrorOf = (prog: string, errorType: string) => {
    const result = compileDomain(prog);
    if (result.isErr()) {
      if (printError) console.log(showError(result.error));
      expect(result.error.tag).toBe(errorType);
    } else {
      throw Error(`Error ${errorType} was suppoed to occur.`);
    }
  };
  test("Parse error", () => {
    const prog = `
type Set somethingthatshouldn'tparse
    `;
    expectErrorOf(prog, "ParseError");
  });
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
constructor Cons ['X] ('X head, List('X) tail) -> List('X)
    `;
    expectErrorOf(prog, "TypeNotFound");
  });
  test("type var not found", () => {
    const prog = `
  constructor Cons ['X] ('Z head, List('Y) tail) -> List('X)
    `;
    expectErrorOf(prog, "TypeVarNotFound");
  });
  test("prop in subtyping relation", () => {
    const prog = `
  type Set
  Prop <: Set
    `;
    expectErrorOf(prog, "ParseError");
  });
  test("type var in subtyping relation", () => {
    const prog = `
  'T <: 'V
    `;
    expectErrorOf(prog, "ParseError");
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
  test("argument type mismatch in symmetric predicates without subtypes", () => {
    const prog = `
type MyType
type MyOtherType
predicate MyNormalPredicate(MyType a, MyType b)
symmetric predicate MyExcellentPredicate(MyType a, MyType b)
symmetric predicate MyBadPredicate(MyType a, MyOtherType b)
    `;
    expectErrorOf(prog, "SymmetricTypeMismatch");
  });

  test("argument type mismatch in symmetric predicates with subtypes", () => {
    const prog = `
type MyType
type MySubType <: MyType
symmetric predicate MyBadPredicate(MyType a, MySubType b)
    `;
    expectErrorOf(prog, "SymmetricTypeMismatch");
  });
  test("argument count mismatch in symmetric predicates", () => {
    const prog = `
type MyType
symmetric predicate MyBadPredicate(MyType, MyType, MyType)
    `;
    expectErrorOf(prog, "SymmetricArgLengthMismatch");
  });
});

describe("Real Programs", () => {
  // create output folder
  if (saveContexts && !fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir);
  }

  domains.map(([examplePath, prog]) => {
    test(examplePath, () => {
      const res = compileDomain(prog);
      expect(res.isOk()).toBe(true);
      // write to output folder
      if (res.isOk() && saveContexts) {
        const exampleName = path.basename(examplePath, ".domain");
        const astPath = path.join(outputDir, exampleName + ".env.json");
        fs.writeFileSync(astPath, JSON.stringify(res.value), "utf8");
      }
    });
  });
});

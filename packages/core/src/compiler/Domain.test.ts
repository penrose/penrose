import nearley from "nearley";
import { beforeEach, describe, expect, test } from "vitest";
import grammar from "../parser/DomainParser.js";
import { DomainEnv } from "../types/domain.js";
import { PenroseError } from "../types/errors.js";
import { Result, showError } from "../utils/Error.js";
import { compileDomain, isSubtype } from "./Domain.js";

const printError = false;

const contextHas = (
  res: Result<DomainEnv, PenroseError>,
  expectedTypes: string[],
  expectedConstructors: string[],
  expectedFunctions: string[],
  expectedPredicates: string[],
) => {
  if (res.isOk()) {
    const {
      types,
      constructorDecls: constructors,
      functionDecls: functions,
      predicateDecls: predicates,
    } = res.value;
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
 `;
    const res = compileDomain(prog);
    if (res.isOk()) {
      const env = res.value;
      const typeA = env.types.get("A")!;
      const typeB = env.types.get("B")!;
      const typeC = env.types.get("C")!;
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
type List
type Real
type Interval
type OpenInterval
type ClosedInterval
constructor Cons(Real head, List tail) -> List
constructor Nil() -> List
constructor CreateInterval(Real left, Real right) -> Interval
constructor CreateOpenInterval(Real left, Real right) -> OpenInterval
constructor CreateClosedInterval(Real left, Real right) -> ClosedInterval
    `;
    const res = compileDomain(prog);
    const types = [
      "String",
      "Number",
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
predicate From(Map f, Set domain, Set codomain)
predicate Empty(Set s)
predicate Intersecting(Set s1, Set s2)
predicate Subset(Set s1, Set s2)
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
      "From",
      "Empty",
      "Intersecting",
      "Subset",
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
      expect(env.predicateDecls.get("MyNormalPredicate")!.symmetric).toEqual(
        false,
      );
      expect(
        env.predicateDecls.get("MyExcellentPredicate1")!.symmetric,
      ).toEqual(true);
      expect(
        env.predicateDecls.get("MyExcellentPredicate2")!.symmetric,
      ).toEqual(true);
    }
  });
  test("builtin types", () => {
    const prog = `type Set
    predicate ContainsStr(Set s, String str)
    predicate ContainsNum(Set s, Number num)`;

    const res = compileDomain(prog);
    const predicates = ["ContainsStr", "ContainsNum"];
    contextHas(res, ["String", "Number", "Set"], [], [], predicates);
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
  test("Type Declared errors", () => {
    const prog = `
type Set
type Point
type Set
    `;
    expectErrorOf(prog, "TypeDeclared");
  });
  test("Type not found", () => {
    const prog = `
constructor Cons(Real head, List tail) -> List
    `;
    expectErrorOf(prog, "TypeNotFound");
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

  test("builtin literal types", () => {
    const prog1 = `
type Set
type Number
type String`;
    expectErrorOf(prog1, "TypeDeclared");

    const prog2 = `
type Set
Set <: Number
    `;
    expectErrorOf(prog2, "SubOrSuperLiteralTypeError");

    const prog3 = `
type Set
String <: Set
    `;
    expectErrorOf(prog3, "SubOrSuperLiteralTypeError");

    const prog4 = `
type Set
function F(Set s) -> String
    `;
    expectErrorOf(prog4, "OutputLiteralTypeError");
    const prog5 = `
type Set <: Number
        `;
    expectErrorOf(prog5, "SubOrSuperLiteralTypeError");
  });
});

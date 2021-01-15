import * as nearley from "nearley";
import grammar from "parser/DomainParser";
import * as path from "path";
import * as fs from "fs";
import { checkDomain, errorString } from "compiler/Domain";

const compileDomain = (prog: string) => {
  const { results } = parser.feed(prog);
  return checkDomain(results[0]);
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
  test("constructor decl", () => {
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

    if (res.isOk()) {
      const [...typeVars] = res.value.typeVars.keys();
      expect(typeVars.length).toBe(0);
      const [...types] = res.value.types.keys();
      expect(types).toEqual([
        "String",
        "List",
        "Real",
        "Interval",
        "OpenInterval",
        "ClosedInterval",
      ]);
      const [...constructors] = res.value.constructors.keys();
      expect(constructors).toEqual([
        "Cons",
        "Nil",
        "CreateInterval",
        "CreateOpenInterval",
        "CreateClosedInterval",
      ]);
    } else {
      fail(errorString(res.error));
    }
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

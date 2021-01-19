import * as nearley from "nearley";
import grammar from "parser/SubstanceParser";
import { domain } from "process";
import { Result, showError } from "utils/Error";
import { compileDomain, Env } from "./Domain";
import { checkSubstance, compileSubstance } from "./Substance";

const domainProg = `
type Set
type OpenSet
type Vector
type List('T)
type Tuple('T, 'U)
constructor Subset: Set A * Set B -> Set
constructor Cons ['X] : 'X head * List('X) tail -> List('X)
constructor Nil['X] -> List('X)
constructor CreateTuple['T, 'U] : 'T fst * 'U snd -> Tuple('T, 'U)
OpenSet <: Set
`;

const envOrError = (prog: string): Env => {
  const res = compileDomain(prog);
  if (res.isErr()) fail(showError(res.error));
  return res.value;
};

let parser: nearley.Parser;
beforeEach(() => {
  // NOTE: Neither `feed` nor `finish` will reset the parser state. Therefore recompiling before each unit test
  parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
});

describe("Common", () => {
  test("empty program", () => {
    const prog = ``;
    const env = envOrError(domainProg);
    const res = compileSubstance(prog, env);
    expect(res.isOk()).toBe(true);
  });
});

describe("Check statements", () => {
  const hasVars = (env: Env, vars: [string, string][]) => {
    vars.map(([name, type]: [string, string]) => {
      expect(env.vars.has(name)).toBe(true);
      expect(env.vars.get(name)?.name.value).toEqual(type);
    });
  };
  test("decls", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
List(Set) l
OpenSet D
D := A
    `;
    const res = compileSubstance(prog, env);
    expect(res.isOk()).toBe(true);
    if (res.isOk())
      hasVars(res.value, [
        ["A", "Set"],
        ["l", "List"],
      ]);
  });
  test("func", () => {
    const env = envOrError(domainProg);
    const prog = `
List(Set) l, nil
nil := Nil()
Set A
l := Cons(A, nil)
      `;
    const res = compileSubstance(prog, env);
    if (res.isOk()) {
      hasVars(res.value, [
        ["A", "Set"],
        ["l", "List"],
        ["nil", "List"],
      ]);
    } else {
      fail(`unexpected error ${showError(res.error)}`);
    }
  });
});

describe("Errors", () => {
  const expectErrorOf = (
    result: Result<Env, SubstanceError | DomainError>,
    errorType: string
  ) => {
    if (result.isErr()) {
      console.log(showError(result.error));
      expect(result.error.tag).toBe(errorType);
    } else {
      fail(`Error ${errorType} was suppoed to occur.`);
    }
  };
  test("type not found", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
List(Set) l
Alien A
NotExistentType B
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeNotFound");
  });
  test("var not found", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
D := Subset(B, C)
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "VarNotFound");
  });
  test("type mismatch: var", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B
Vector v
A := B -- ok
A := v -- error
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeMismatch");
  });
  test("func: arg length mismatch", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
C := Subset(A) -- error
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "ArgLengthMismatch");
  });
  test("func: arg type mismatch", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
Vector v
C := Subset(A, B) -- ok
C := Subset(A, v) -- error
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeMismatch");
  });
  test("func: output type mismatch", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
Vector v
C := Subset(A, B) -- ok
v := Subset(A, B) -- error
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeMismatch");
  });
  // TODO: fix typeconstructor check and pass this test
  test("func: type argument mismatch", () => {
    const env = envOrError(domainProg);
    const prog = `
-- type Tuple('T, 'U)
-- constructor CreateTuple['T, 'U] : 'T fst * 'U snd -> Tuple('T, 'U)
List(Set) nil
Tuple(Set, Set) t -- Maybe an error?
t := CreateTuple(nil, nil) -- Definitely an error
      `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeMismatch");
  });
  test("func: type argument mismatch 2", () => {
    const env = envOrError(domainProg);
    const prog = `
  -- Substance program for type checking
  List(Set) l, nil
  nil := Nil()
  Set A
  l := Cons(nil, A)
      `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeMismatch");
  });
});

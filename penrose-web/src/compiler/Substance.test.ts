import * as nearley from "nearley";
import grammar from "parser/SubstanceParser";
import { domain } from "process";
import { Result, showError } from "utils/Error";
import { compileDomain, Env } from "./Domain";
import { checkSubstance, compileSubstance } from "./Substance";

const domainProg = `
type Set
type List('T)
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
  test("Decls", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
List(Set) l
    `;
    const res = compileSubstance(prog, env);
    expect(res.isOk()).toBe(true);
    if (res.isOk())
      hasVars(res.value, [
        ["A", "Set"],
        ["l", "List"],
      ]);
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
});

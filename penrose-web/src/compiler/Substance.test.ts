import * as nearley from "nearley";
import grammar from "parser/SubstanceParser";
import { checkSubstance } from "./Substance";

const compileSubstance = (prog: string) => {
  const { results } = parser.feed(prog);
  return checkSubstance(results[0]);
};

let parser: nearley.Parser;
beforeEach(() => {
  // NOTE: Neither `feed` nor `finish` will reset the parser state. Therefore recompiling before each unit test
  parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
});

describe("Common", () => {
  test("empty program", () => {
    const prog = ``;
    const res = compileSubstance(prog);
    expect(res.isOk()).toBe(true);
  });
});

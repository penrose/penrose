import * as nearley from "nearley";
import grammar from "parser/DomainParser";
import * as path from "path";
import * as fs from "fs";
import { result } from "lodash";
import { checkDomain } from "compiler/Domain";

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
    const { results } = parser.feed(prog);
    const res = checkDomain(results[0]);
    console.log(res);
  });
});

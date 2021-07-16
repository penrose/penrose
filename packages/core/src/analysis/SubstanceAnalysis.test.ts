import { compileDomain } from "compiler/Domain";
import {
  compileSubstance,
  prettyStmt,
  prettySubstance,
} from "compiler/Substance";
import { dummyIdentifier } from "engine/EngineUtils";
import { Env } from "types/domain";
import { SubStmt } from "types/substance";
import { appendStmt, removeStmt, replaceStmt } from "./SubstanceAnalysis";

const domain = `
type Set
`;
const env: Env = compileDomain(domain).unsafelyUnwrap();

describe("AST mutation operations", () => {
  test("addition", () => {
    const original = `Set A
Set B`;
    const expected = `Set A
Set B
Set C`;
    const originalAST = compileSubstance(original, env).unsafelyUnwrap()[0].ast;
    const newStmt: SubStmt = {
      nodeType: "SyntheticSubstance",
      children: [],
      tag: "Decl",
      name: dummyIdentifier("C", "SyntheticSubstance"),
      type: {
        tag: "TypeConstructor",
        args: [],
        name: dummyIdentifier("Set", "SyntheticSubstance"),
      },
    };
    const newAST = appendStmt(originalAST, newStmt);
    expect(expected).toEqual(prettySubstance(newAST));
  });

  test("deletion", () => {
    const original = `Set A
Set B`;
    const expected = `Set B`;
    const originalAST = compileSubstance(original, env).unsafelyUnwrap()[0].ast;
    const toDelete = originalAST.statements[1];
    const newAST = removeStmt(originalAST, toDelete);
    expect(expected).toEqual(prettySubstance(newAST));
  });

  test("replacement", () => {
    const original = `Set A
Set B`;
    const expected = `Set A
Set ZZZ`;
    const originalAST = compileSubstance(original, env).unsafelyUnwrap()[0].ast;
    const toReplace = originalAST.statements[1];
    const newStmt: SubStmt = {
      nodeType: "SyntheticSubstance",
      children: [],
      tag: "Decl",
      name: dummyIdentifier("ZZZ", "SyntheticSubstance"),
      type: {
        tag: "TypeConstructor",
        args: [],
        name: dummyIdentifier("Set", "SyntheticSubstance"),
      },
    };
    const newAST = replaceStmt(originalAST, toReplace, newStmt);
    expect(expected).toEqual(prettySubstance(newAST));
  });
});

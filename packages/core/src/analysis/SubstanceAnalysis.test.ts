import { compileDomain } from "compiler/Domain";
import {
  compileSubstance,
  prettyStmt,
  prettySubstance,
} from "compiler/Substance";
import { dummyIdentifier } from "engine/EngineUtils";
import { Env } from "types/domain";
import { DefaultLabels, SubProg, SubStmt } from "types/substance";
import {
  appendStmt,
  nodesEqual,
  removeStmt,
  replaceStmt,
  sortStmts,
} from "./SubstanceAnalysis";

const domain = `
type Set
predicate IsSubset: Set * Set
function Subset : Set a * Set b -> Set
`;
const env: Env = compileDomain(domain).unsafelyUnwrap();

describe("Substance AST queries", () => {
  test("Node equality check", () => {
    const id1 = dummyIdentifier("A", "SyntheticSubstance");
    const id2 = dummyIdentifier("B", "SyntheticSubstance");
    expect(nodesEqual(id1, id2)).toBe(false);
    expect(nodesEqual(id2, id2)).toBe(true);
    // create two SubStmts with different source locs and children and the equality check should still return true
    const prog1: DefaultLabels = {
      children: [],
      tag: "DefaultLabels",
      nodeType: "Substance",
      start: { line: 0, col: 0 },
      end: { line: 0, col: 0 },
    };
    const prog2: DefaultLabels = {
      children: [prog1],
      tag: "DefaultLabels",
      nodeType: "Substance",
      start: { line: 5, col: 0 },
      end: { line: 8, col: 0 },
    };
    const prog3 = { ...prog2, tag: "AutoLabel" };
    expect(nodesEqual(prog1, prog2)).toBe(true);
    expect(nodesEqual(prog3, prog2)).toBe(false);
  });
});

describe("AST mutation operations", () => {
  test("sorting", () => {
    const original = `
Set B
Set A
Set C
IsSubset(A, B)
IsSubset(B, A)
C := Subset(A, B)
`;
    const expected = `\
C := Subset(A, B)
IsSubset(A, B)
IsSubset(B, A)
Set A
Set B
Set C\
`;
    const originalAST = compileSubstance(original, env).unsafelyUnwrap()[0].ast;
    const sortedAST = sortStmts(originalAST);
    expect(prettySubstance(sortedAST)).toEqual(expected);
  });
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

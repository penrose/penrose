import { compileDomain } from "@penrose/core/dist/compiler/Domain";
import {
  compileSubstance,
  prettyStmt,
  prettySubstance,
} from "@penrose/core/dist/compiler/Substance";
import { dummyIdentifier } from "@penrose/core/dist/engine/EngineUtils";
import { A } from "@penrose/core/dist/types/ast";
import { Env } from "@penrose/core/dist/types/domain";
import { SubProg, SubStmt } from "@penrose/core/dist/types/substance";
import _ from "lodash";
import { describe, expect, test } from "vitest";
import { SubNode, similarMappings, similarNodes } from "../synthesis/Search.js";
import {
  appendStmt,
  intersection,
  nodesEqual,
  removeStmt,
  replaceStmt,
  sortStmts,
} from "./SubstanceAnalysis.js";

const domain = `
type Set
type Point
predicate IsSubset(Set, Set)
predicate Equal(Set, Set)
function Subset(Set a, Set b) -> Set
`;
const env: Env = compileDomain(domain).unsafelyUnwrap();

const compile = (src: string): SubProg<A> =>
  compileSubstance(src, env).unsafelyUnwrap()[0].ast;

describe("Substance AST queries", () => {
  test("Similar AST nodes", () => {
    let node1: SubNode<A>;
    let node2: SubNode<A>;
    node1 = compile("Set A");
    node2 = compile("Set B");
    expect(similarNodes(node1, node2)).toBe(true);
    expect(similarNodes(node1, node1)).toBe(true);
    expect(similarNodes(node2, node2)).toBe(true);
    node1 = compile("Point A");
    node2 = compile("Set B");
    expect(similarNodes(node1, node2)).toBe(false);
    node1 = compile("Set A, B\nIsSubset(A, B)").statements[2];
    node2 = compile("Set A, B\nIsSubset(B, A)").statements[2];
    expect(similarNodes(node1, node2)).toBe(true);
    expect(similarNodes(node1, node1)).toBe(true);
    node1 = compile("Set A, B, C\nIsSubset(A, B)").statements[3];
    node2 = compile("Set A, B, C\nIsSubset(C, C)").statements[3];
    expect(similarNodes(node1, node2)).toBe(true);
    expect(similarNodes(node1, node1)).toBe(true);
    node1 = compile("Set A, B, C\nIsSubset(A, B)").statements[3];
    node2 = compile("Set A, B, C\nEqual(C, C)").statements[3];
    expect(similarNodes(node1, node2)).toBe(false);
    expect(similarNodes(node1, node1)).toBe(true);
    node1 = compile("Set A, B, C\nIsSubset(A, B)").statements[3];
    node2 = compile("Set A, B, C\nC := Subset(A, B)").statements[3];
    expect(similarNodes(node1, node2)).toBe(true);
    expect(similarNodes(node1, node1)).toBe(true);
  });
  test("Find difference between ASTs", () => {
    const left = `
    Set A
    Set B
    Set C
    IsSubset(A, B)
    IsSubset(B, A)
    `;
    const right = `\
    Set A
    Set B
    Set C
    IsSubset(B, A)
    C := Subset(A, B)
    `;
    const leftAST = compile(left);
    const rightAST = compile(right);
    const commonStmts = intersection(leftAST, rightAST);
    const leftFiltered = leftAST.statements.filter((a) => {
      return _.intersectionWith(commonStmts, [a], nodesEqual).length === 0;
    });
    const rightFiltered = rightAST.statements.filter((a) => {
      return _.intersectionWith(commonStmts, [a], nodesEqual).length === 0;
    });
    expect(commonStmts.map(prettyStmt)).toEqual([
      "Set A",
      "Set B",
      "Set C",
      "IsSubset(B, A)",
    ]);
    expect(leftFiltered.map(prettyStmt)).toEqual(["IsSubset(A, B)"]);
    expect(rightFiltered.map(prettyStmt)).toEqual(["C := Subset(A, B)"]);
    const similarMap = similarMappings(leftFiltered, rightFiltered);
    expect(similarMap[0].similarStmts).toHaveLength(1);
    expect(prettyStmt(similarMap[0].similarStmts[0])).toEqual(
      "C := Subset(A, B)",
    );
  });
  // TODO: this test is out of date because the equality checks don't clean nodes anymore
  // test("Node equality check", () => {
  //   const id1 = dummyIdentifier("A", "SyntheticSubstance");
  //   const id2 = dummyIdentifier("B", "SyntheticSubstance");
  //   expect(nodesEqual(id1, id2)).toBe(false);
  //   expect(nodesEqual(id2, id2)).toBe(true);
  //   // create two SubStmts with different source locs and the equality check should still return true
  //   const prog1: DefaultLabels = {
  //     tag: "DefaultLabels",
  //     nodeType: "Substance",
  //     start: { line: 0, col: 0 },
  //     end: { line: 0, col: 0 },
  //   };
  //   const prog2: DefaultLabels = {
  //     tag: "DefaultLabels",
  //     nodeType: "Substance",
  //     start: { line: 5, col: 0 },
  //     end: { line: 8, col: 0 },
  //   };
  //   const prog3 = { ...prog2, tag: "AutoLabel" };
  //   expect(nodesEqual(prog1, prog2)).toBe(true);
  //   expect(nodesEqual(prog3, prog2)).toBe(false);
  // });
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
Set A
Set B
Set C
IsSubset(A, B)
IsSubset(B, A)
C := Subset(A, B)\
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
    const newStmt: SubStmt<A> = {
      nodeType: "SyntheticSubstance",
      tag: "Decl",
      name: dummyIdentifier("C", "SyntheticSubstance"),
      type: {
        tag: "TypeConstructor",
        nodeType: "SyntheticSubstance",
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
    const toDelete = originalAST.statements[0];
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
    const newStmt: SubStmt<A> = {
      nodeType: "SyntheticSubstance",
      tag: "Decl",
      name: dummyIdentifier("ZZZ", "SyntheticSubstance"),
      type: {
        tag: "TypeConstructor",
        nodeType: "SyntheticSubstance",
        args: [],
        name: dummyIdentifier("Set", "SyntheticSubstance"),
      },
    };
    const newAST = replaceStmt(originalAST, toReplace, newStmt);
    expect(expected).toEqual(prettySubstance(newAST));
  });
});

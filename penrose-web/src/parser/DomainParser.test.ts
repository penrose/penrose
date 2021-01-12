import * as nearley from "nearley";
import grammar from "./DomainParser";
import * as path from "path";
import * as fs from "fs";
import { result } from "lodash";

// const outputDir = "/tmp/asts";
// const saveASTs = true;

let parser: nearley.Parser;
const sameASTs = (results: any[]) => {
  for (const p of results) expect(results[0]).toEqual(p);
  expect(results.length).toEqual(1);
};

// USAGE:
// printAST(results[0])
const printAST = (ast: any) => {
  console.log(JSON.stringify(ast));
};

const domainPaths = [
  "linear-algebra-domain/linear-algebra.dsl",
  "set-theory-domain/setTheory.dsl",
];

beforeEach(() => {
  // NOTE: Neither `feed` nor `finish` will reset the parser state. Therefore recompiling before each unit test
  parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
});

describe("Common", () => {
  test("empty program", () => {
    const { results } = parser.feed("");
    sameASTs(results);
  });
  test("comments", () => {
    const { results } = parser.feed("");
    sameASTs(results);
  });
});

describe("Statement types", () => {
  test("type decls", () => {
    const prog = `
-- comments
type Set 
type Point 
type ParametrizedSet (s1: Set, s2: Set)
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("predicate decls", () => {
    const prog = `
-- comments
predicate Not : Prop p1
predicate From : Map f * Set domain * Set codomain
predicate Empty : Set s
predicate Intersecting : Set s1 * Set s2
predicate IsSubset : Set s1 * Set s2
predicate PointIn : Set s * Point p
predicate In : Point p * Set s
predicate Injection : Map m
predicate Surjection : Map m
predicate Bijection : Map m
predicate PairIn : Point * Point * Map
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("function decls", () => {
    const prog = `
-- comments
function Intersection : Set a * Set b -> Set
function Union : Set a * Set b -> Set
function Subtraction : Set a * Set b -> Set
function CartesianProduct : Set a * Set b -> Set
function Difference : Set a * Set b -> Set
function Subset : Set a * Set b -> Set
function AddPoint : Point p * Set s1 -> Set
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
    printAST(results[0]);
  });
});

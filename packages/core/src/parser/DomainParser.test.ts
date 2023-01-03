import { examples } from "@penrose/examples";
import * as fs from "fs";
import nearley from "nearley";
import * as path from "path";
import { SourceRange } from "../types/ast";
import { DomainProg, PredicateDecl } from "../types/domain";
import grammar from "./DomainParser";

const outputDir = "/tmp/asts";
const saveASTs = false;

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
  test("comments and whitespaces", () => {
    const prog = `
-- comments
type Set -- inline comments\r
-- type Point 
type ParametrizedSet ('T, 'U)\r\n
predicate From(Map f, Set domain, Set codomain)\n
/* Multi-line comments
type ParametrizedSet ('T, 'U)
predicate From(Map f, Set domain, Set codomain)
*/
predicate From(Map f, Set domain, Set codomain)
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("tree integrity", () => {
    const prog = `
-- comments
type Set -- inline comments
-- type Point 
type ParametrizedSet ('T, 'U)
predicate From(Map f, Set domain, Set codomain)
/* Multi-line comments
type ParametrizedSet ('T, 'U)
predicate From(Map f, Set domain, Set codomain)
*/
predicate From(Map f, Set domain, Set codomain)
function Intersection(Set a, Set b) -> Set
function Union(Set a, Set b) -> Set c
function Subtraction(Set a, Set b) -> Set
function CartesianProduct(Set a, Set b) -> Set
function Difference(Set a, Set b) -> Set
function Subset(Set a, Set b) -> Set
function AddPoint(Point p, Set s1) -> Set
-- with type params
function AddV['V](Vector('V) v1, Vector('V) v2) -> Vector('V)
function Norm['V](Vector('V) v1) -> Scalar
-- edge case
function Empty() -> Scalar
-- generics
constructor Cons ['X] ('X head, List('X) tail) -> List('X)
constructor Nil['X]() -> List('X)
notation "A ⊂ B" ~ "IsSubset(A, B)"
notation "p ∈ A" ~ "PointIn(A, p)"
notation "p ∉ A" ~ "PointNotIn(A, p)"
notation "A ∩ B = ∅" ~ "Not(Intersecting(A, B))"
notation "f: A -> B" ~ "Map f; From(f, A, B)"
RightClopenInterval <: Interval
-- edge cases
List(Vector) <: List(Matrix)
List('T) <: List('U)
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
});

describe("Statement types", () => {
  test("type decls", () => {
    const prog = `
-- comments
type Set 
type Point 
-- type ParametrizedSet1 () -- this is not okay
type ParametrizedSet2 ('T)
type ParametrizedSet3 ( 'T,    'V)
-- inline subtype
type Nonempty <: Set
type SmallerSet <: Point, Set
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("predicate decls", () => {
    const prog = `
-- comments
predicate Not(Prop p1)
predicate From(Map f, Set domain, Set codomain)
predicate Empty(Set s)
predicate Intersecting(Set s1, Set s2)
predicate IsSubset(Set s1, Set s2)
predicate PointIn(Set s, Point p)
predicate In(Point p, Set s)
predicate Injection(Map m)
predicate Surjection(Map m)
predicate Bijection(Map m)
predicate PairIn(Point, Point, Map)
symmetric predicate Intersecting(Set s1, Set s2)
symmetric predicate Disjoint(Set, Set)
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);

    // New part of the test:
    // Also check the symmetry of the predicates

    // These are the correct symmetries of the predicates
    const areSymmetricReference: boolean[] = [
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      true,
      true,
    ];
    // These are what the parser generates
    const areSymmetric: boolean[] = (<DomainProg<SourceRange>>(
      results[0]
    )).statements.map((stmt) => {
      return (<PredicateDecl<SourceRange>>stmt).symmetric;
    });

    expect(areSymmetric).toEqual(areSymmetricReference);
  });
  test("function decls", () => {
    const prog = `
-- comments
function Intersection(Set a, Set b) -> Set
function Union(Set a, Set b) -> Set c
function Subtraction(Set a, Set b) -> Set
function CartesianProduct(Set a, Set b) -> Set
function Difference(Set a, Set b) -> Set
function Subset(Set a, Set b) -> Set
function AddPoint(Point p, Set s1) -> Set
-- with type params
function AddV['V](Vector('V) v1, Vector('V) v2) -> Vector('V)
function Norm['V](Vector('V) v1) -> Scalar
-- edge case
function Empty() -> Scalar
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("constructor decls", () => {
    const prog = `
  -- real program
  constructor CreateInterval(Real left, Real right) -> Interval
  constructor CreateOpenInterval(Real left, Real right) -> OpenInterval
  constructor CreateClosedInterval(Real left, Real right) -> ClosedInterval
  constructor CreateLeftClopenInterval(Real left, Real right) -> LeftClopenInterval
  constructor CreateRightClopenInterval(Real left, Real right) -> RightClopenInterval
  constructor CreateFunction(Set s1, Set s2) -> Function
  constructor Pt(Real x, Real y) -> Point
  -- generics
  constructor Cons ['X] ('X head, List('X) tail) -> List('X)
  constructor Nil['X]() -> List('X)
      `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("prelude decls", () => {
    const prog = `
  -- real program
  value R : Reals
  value R2 : Set
  -- generics
  value R2 : Set('T, 'U, Vector)
      `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("notation decls", () => {
    const prog = `
notation "A ⊂ B" ~ "IsSubset(A, B)"
notation "p ∈ A" ~ "PointIn(A, p)"
notation "p ∉ A" ~ "PointNotIn(A, p)"
notation "A ∩ B = ∅" ~ "Not(Intersecting(A, B))"
notation "f: A -> B" ~ "Map f; From(f, A, B)"
      `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("notation decls", () => {
    const prog = `
Reals <: Set
Interval <: Set
Reals <: Interval
OpenInterval <: Interval
ClosedInterval <: Interval
LeftClopenInterval <: Interval
RightClopenInterval <: Interval
-- edge cases
List(Vector) <: List(Matrix)
List('T) <: List('U)
      `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
});

describe("Real Programs", () => {
  // create output folder
  if (saveASTs && !fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir);
  }

  domainPaths.forEach((examplePath) => {
    // a bit hacky, only works with 2-part paths
    const [part0, part1] = examplePath.split("/");
    const prog = examples[part0][part1];
    test(examplePath, () => {
      const { results } = parser.feed(prog);
      sameASTs(results);
      // write to output folder
      if (saveASTs) {
        const exampleName = path.basename(examplePath, ".dsl");
        const astPath = path.join(outputDir, exampleName + ".ast.json");
        fs.writeFileSync(astPath, JSON.stringify(results[0]), "utf8");
      }
    });
  });
});

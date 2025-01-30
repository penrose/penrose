import { Tree } from "@lezer/common";
import { assert, describe, expect, test } from "vitest";
import { printNode, validateDomain } from "../compiler/Domain.js";
import { parser } from "../parser/DomainParser.js";
import { SourceRange } from "../types/ast.js";
import { PredicateDecl } from "../types/domain.js";

// if the `Tree` contains error node(s)
const isError = (ast: Tree, prog: string) => {
  ast.iterate({
    enter: (node) => {
      if (node.type.isError) {
        assert.fail(
          `Unexpected error found in node${printNode(node.node.parent!, prog)}`,
        );
      }
    },
  });
};

describe("Common", () => {
  test("empty program", () => {
    const tree = parser.parse("");
    isError(tree, "");
  });
  test("comments and whitespaces", () => {
    const prog = `
-- comments
type Set -- inline comments\r
-- type Point
predicate From(Map f, Set domain, Set codomain)\r\n
/* Multi-line comments
type ParametrizedSet ('T, 'U)
predicate From(Map f, Set domain, Set codomain)
*/
predicate From(Map f, Set domain, Set codomain)
	`;
    const tree = parser.parse(prog);
    isError(tree, prog);
  });
  test("tree integrity", () => {
    const prog = `
-- comments
type Set -- inline comments
-- type Point
predicate From(Map f, Set domain, Set codomain)
/* Multi-line comments
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
-- edge case
-- due to the ambiguity of the output name and subtype decls, this will fail without the semi.
function Empty() -> Scalar; 
-- generics
RightClopenInterval <: Interval
	`;
    const tree = parser.parse(prog);
    isError(tree, prog);
  });
});

describe("Statement types", () => {
  test("type decls", () => {
    const prog = `
-- comments
type Set
type Point
-- inline subtype
type Nonempty <: Set
type SmallerSet <: Point, Set
	`;
    const tree = parser.parse(prog);
    isError(tree, prog);
  });
  test("predicate decls", () => {
    const prog = `
-- comments
predicate Not(Prop p1)
predicate From(Map f, Set domain, Set codomain)
predicate Empty(Set s)
predicate Intersecting(Set s1, Set s2)
predicate Subset(Set s1, Set s2)
predicate PointIn(Set s, Point p)
predicate In(Point p, Set s)
predicate Injection(Map m)
predicate Surjection(Map m)
predicate Bijection(Map m)
predicate PairIn(Point, Point, Map)
symmetric predicate Intersecting(Set s1, Set s2)
symmetric predicate Disjoint(Set, Set)
	`;
    const tree = parser.parse(prog);
    isError(tree, prog);

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
    const areSymmetric: boolean[] = validateDomain(tree, prog)
      .unwrapOr(undefined)!
      .statements.map((stmt) => {
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
-- edge case
function Empty() -> Scalar
	`;
    const tree = parser.parse(prog);
    isError(tree, prog);
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
		`;
    const tree = parser.parse(prog);
    isError(tree, prog);
  });
  test("Subtype decls", () => {
    const prog = `
Reals <: Set
Interval <: Set
Reals <: Interval
OpenInterval <: Interval
ClosedInterval <: Interval
LeftClopenInterval <: Interval
RightClopenInterval <: Interval
		`;
    const tree = parser.parse(prog);
    isError(tree, prog);
  });
  test("dangling output type conflict with subtype decls", () => {
    const prog = `
		type A
		type B        
		-- due to the ambiguity of the output name and subtype decls, this will fail without the semi.
		function f(A arg) -> B;
		A <: B`;
    const tree = parser.parse(prog);
    isError(tree, prog);
  });
});

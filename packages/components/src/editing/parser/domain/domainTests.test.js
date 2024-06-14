import { describe, test } from "vitest";
import { createTestParser } from "../parserUtils.js";
import { parser } from "./domain.js";

const testParser = createTestParser(parser);

describe("Common", () => {
  test("empty", () => {
    let input = "";
    let expected = "Program";
    testParser(input, expected);
  });
  test("comments and whitespaces", () => {
    // For some reason the two line comments following type Set
    // are counted as children of Type
    let input = `-- comments
            type Set -- inline comments\r
            -- type Point
            predicate From(Map f, Set domain, Set codomain)\r\n
            /* Multi-line comments
            type ParametrizedSet ('T, 'U)
            predicate From(Map f, Set domain, Set codomain)
            */
            predicate From(Map f, Set domain, Set codomain)`;
    let expected = `Program(LineComment,
        Type(type,Identifier,LineComment,LineComment),
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier,Identifier,Identifier)),
        BlockComment,
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier,Identifier,Identifier)))`;

    testParser(input, expected);
  });
  test("mixed", () => {
    // in the old test file, there was an intentional syntax error
    // to see how nearley.js responds (c after "Set")
    // lezer responds by throwing an error node (add any character after Set in function output type)
    let input = `
-- comments
type Set -- inline comments
-- type Point
predicate From(Map f, Set domain, Set codomain)
/* Multi-line comments
predicate From(Map f, Set domain, Set codomain)
*/
predicate From(Map f, Set domain, Set codomain)
function Intersection(Set a, Set b) -> Set
function Union(Set a, Set b) -> Set
function Subtraction(Set a, Set b) -> Set
function CartesianProduct(Set a, Set b) -> Set
function Difference(Set a, Set b) -> Set
function Subset(Set a, Set b) -> Set
function AddPoint(Point p, Set s1) -> Set
-- edge case
function Empty() -> Scalar
-- generics
RightClopenInterval <: Interval`;
    let expected = `Program(LineComment,
        Type(type,Identifier,LineComment,LineComment),
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier,Identifier,Identifier)),
        BlockComment,
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier,Identifier,Identifier)),
        Function(function,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier),Identifier),
        Function(function,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier),Identifier),
        Function(function,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier),Identifier),
        Function(function,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier),Identifier),
        Function(function,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier),Identifier),
        Function(function,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier),Identifier),
        Function(function,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier),Identifier),
        LineComment,
        Function(function,Identifier,
            ParamList,Identifier),
        LineComment,
        Subtype(Identifier,Identifier))`;
    testParser(input, expected);
  });
});

describe("Statement types", () => {
  test("type decls", () => {
    let input = `
-- comments
type Set
type Point
-- inline subtype
type Nonempty <: Set
type SmallerSet <: Point, Set`;
    let expected = `
Program(LineComment,
Type(type,Identifier),
Type(type,Identifier),
LineComment,
Type(type,
    Subtype(Identifier,Identifier)),
Type(type,
    Subtype(Identifier,Identifier,Identifier)))`;
    testParser(input, expected);
  });

  test("predicate decls", () => {
    let input = `
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
symmetric predicate Disjoint(Set, Set)`;

    let expected = `
        Program(LineComment,
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier)),
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier,Identifier,Identifier)),
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier)),
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier)),
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier)),
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier)),
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier)),
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier)),
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier)),
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier)),
        Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier,Identifier)),
        SymPred(symmetric,Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier))),
        SymPred(symmetric,Predicate(predicate,Identifier,
            ParamList(Identifier,Identifier))))`;
    testParser(input, expected);
  });

  test("constructor decls", () => {
    const input = `
  -- real program
  constructor CreateInterval(Real left, Real right) -> Interval
  constructor CreateOpenInterval(Real left, Real right) -> OpenInterval
  constructor CreateClosedInterval(Real left, Real right) -> ClosedInterval
  constructor CreateLeftClopenInterval(Real left, Real right) -> LeftClopenInterval
  constructor CreateRightClopenInterval(Real left, Real right) -> RightClopenInterval
  constructor CreateFunction(Set s1, Set s2) -> Function
  constructor Pt(Real x, Real y) -> Point
      `;

    const expected = `
      Program(LineComment,
        Constructor(constructor,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier),Identifier),
        Constructor(constructor,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier),Identifier),
        Constructor(constructor,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier),Identifier),
        Constructor(constructor,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier),Identifier),
        Constructor(constructor,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier),Identifier),
        Constructor(constructor,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier),Identifier),
        Constructor(constructor,Identifier,
            ParamList(Identifier,Identifier,Identifier,Identifier),Identifier))`;
    testParser(input, expected);
  });

  test("Subtype decls", () => {
    let input = `
Reals <: Set
Interval <: Set
Reals <: Interval
OpenInterval <: Interval
ClosedInterval <: Interval
LeftClopenInterval <: Interval
RightClopenInterval <: Interval`;

    let expected = `
        Program(Subtype(Identifier,Identifier),
        Subtype(Identifier,Identifier),
        Subtype(Identifier,Identifier),
        Subtype(Identifier,Identifier),
        Subtype(Identifier,Identifier),
        Subtype(Identifier,Identifier),
        Subtype(Identifier,Identifier))`;

    testParser(input, expected);
  });
});

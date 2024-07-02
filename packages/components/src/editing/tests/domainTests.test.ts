import { domainParser as parser } from "@penrose/core";
import { describe, expect, test } from "vitest";
import { builtinTypes, domainKws } from "../hooks/domain/domainAutocomplete";
import { getDomainCache } from "../hooks/domain/getDomainCache";
import {
  compareDicts,
  constructDomainCacheObj,
  hasNoErrors,
  testDomainAutocomplete,
} from "./testUtils";

describe("Parser", () => {
  test("empty", () => {
    let prog = "";
    hasNoErrors(parser, prog);
  });

  test("comments and whitespaces", () => {
    const prog = `-- comments
type Set -- inline comments\r
-- type Point
predicate From(Map f, Set domain, Set codomain)\r\n
/* Multi-line comments
type ParametrizedSet ('T, 'U)
predicate From(Map f, Set domain, Set codomain)
*/
predicate From(Map f, Set domain, Set codomain)
	`;

    hasNoErrors(parser, prog);
  });

  test("tree integrity", () => {
    const prog = `-- comments
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
function Empty() -> Scalar
-- generics
RightClopenInterval <: Interval
	`;

    hasNoErrors(parser, prog);
  });

  test("predicate decls", () => {
    const prog = `-- comments
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

    hasNoErrors(parser, prog);
  });

  test("function decls", () => {
    const prog = `-- comments
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
    hasNoErrors(parser, prog);
  });

  test("constructor decls", () => {
    const prog = `-- real program
constructor CreateInterval(Real left, Real right) -> Interval
constructor CreateOpenInterval(Real left, Real right) -> OpenInterval
constructor CreateClosedInterval(Real left, Real right) -> ClosedInterval
constructor CreateLeftClopenInterval(Real left, Real right) -> LeftClopenInterval
constructor CreateRightClopenInterval(Real left, Real right) -> RightClopenInterval
constructor CreateFunction(Set s1, Set s2) -> Function
constructor Pt(Real x, Real y) -> Point
		`;
    hasNoErrors(parser, prog);
  });

  test("Subtype decls", () => {
    const prog = `Reals <: Set
Interval <: Set
Reals <: Interval
OpenInterval <: Interval
ClosedInterval <: Interval
LeftClopenInterval <: Interval
RightClopenInterval <: Interval
		`;

    hasNoErrors(parser, prog);
  });
  test("dangling output type conflict with subtype decls", () => {
    const prog = `type A
		type B        
		function f(A arg) -> B
		A <: B`;
    hasNoErrors(parser, prog);
  });
});

describe("Caching", () => {
  test("type names", () => {
    const input = `type Set
    type Segment
    type Element`;
    const expected = constructDomainCacheObj(
      ["Set", "Segment", "Element"],
      [],
      [],
      [],
    );
    expect(compareDicts(getDomainCache(input), expected)).toBe(true);
  });

  test("pred names", () => {
    const input = `predicate Disjoint(Set s1, Set s2)
predicate Intersecting(Set s1, Set s2)
predicate Subset(Set s1, Set s2)`;
    const expected = constructDomainCacheObj(
      [],
      ["Subset", "Disjoint", "Intersecting"],
      [],
      [],
    );

    expect(compareDicts(getDomainCache(input), expected)).toBe(true);
  });

  test("function names", () => {
    const input = `function Intersection(Set a, Set b) -> Set
function Union(Set a, Set b) -> Set
function Subtraction(Set a, Set b) -> Set
function CartesianProduct(Set a, Set b) -> Set`;
    const expected = constructDomainCacheObj(
      [],
      [],
      ["Union", "Subtraction", "Intersection", "CartesianProduct"],
      [],
    );
    expect(compareDicts(getDomainCache(input), expected)).toBe(true);
  });

  test("constructor names", () => {
    const input = `constructor Segment( Point p, Point q )
constructor LineSegment( Point p, Point q ) -> Segment
constructor ClosestPoint( Set s, Point p ) -> Point
constructor ClosestSegment( Set s, Point p ) -> Segment`;
    const expected = constructDomainCacheObj(
      [],
      [],
      [],
      ["Segment", "LineSegment", "ClosestPoint", "ClosestSegment"],
    );
    expect(compareDicts(getDomainCache(input), expected)).toBe(true);
  });

  test("subtypes", () => {
    const input = `type Set
type Point <: Set
type Segment <: Set, Point
type Ray <: Set
Ray <: Segment`;
    const expected = constructDomainCacheObj(
      ["Set", "Ray", "Point", "Segment"],
      [],
      [],
      [],
    );
    expect(compareDicts(getDomainCache(input), expected)).toBe(true);
  });

  test("combined 1", () => {
    const input = `type Graph
type Layer
type Layer1
type Layer2

Layer1 <: Layer
Layer2 <: Layer

predicate GraphHasNode(Graph g, Node n)

predicate Layer1HasEdge(Layer1 l, Edge e)
predicate Layer2HasEdge(Layer2 l, Edge e)
constructor Quadrilateral(Point p, Point q, Point r, Point s) -- This function creates a quadrilateral from four points. 
constructor CircleR(Point center, Point radius) -> Circle -- This constructor creates a circle from a center point and a radius point. 
function Bisector(Angle) -> Ray -- This function creates a ray as the angle bisector of an angle. 
`;
    const expected = constructDomainCacheObj(
      ["Graph", "Layer", "Layer1", "Layer2"],
      ["GraphHasNode", "Layer1HasEdge", "Layer2HasEdge"],
      ["Bisector"],
      ["Quadrilateral", "CircleR"],
    );
    expect(compareDicts(getDomainCache(input), expected)).toBe(true);
  });

  test("combined 2", () => {
    const input = `predicate IsLine()
    predicate IsBig()
    type Angle
    function Bisector (Angle) -> Angle
    constructor Amongus (Angle a) -> Angle`;

    const expected = constructDomainCacheObj(
      ["Angle"],
      ["IsLine", "IsBig"],
      ["Bisector"],
      ["Amongus"],
    );
    expect(compareDicts(getDomainCache(input), expected)).toBe(true);
  });

  test("runtime 1", () => {
    const input = `type Set
type Point <: Set
type Segment <: Set
type Ray <: Set
type Vector <: Set
type Triangle <: Set
type Circle <: Set
type Disk <: Set
type Chord <: Set
type Length <: Set
type Polyline <: Set
type Angle <: Set

constructor Segment( Point p, Point q )
constructor LineSegment( Point p, Point q ) -> Segment
constructor ClosestPoint( Set s, Point p ) -> Point
constructor ClosestSegment( Set s, Point p ) -> Segment
constructor RayFrom( Point p, Vector v ) -> Ray
constructor RayIntersection( Ray r, Set s ) -> Point
constructor RaySegmentIntersection( Ray r, Segment s ) -> Point
constructor Triangle( Point p1, Point p2, Point p3 )
constructor LengthBetween( Point x, Point y ) -> Length
constructor LengthOf( Segment s ) -> Length
constructor Normal( Set s ) -> Vector
constructor InteriorAngle( Point a, Point b, Point c ) -> Angle

predicate RootedAt( Point p, Vector v )
predicate InTri( Point p, Triangle t )
predicate OnCircle( Point p, Circle c )
predicate InDisk( Point p, Disk d )
predicate OfDisk( Chord c, Disk d )
predicate IsDashed( Set s )
predicate IsOriented( Set s )`;

    const start = performance.now();
    getDomainCache(input);
    const end = performance.now();
    const executionTime = end - start;

    console.log(`Domain Cache Execution Time: ${executionTime} ms`);
    expect(executionTime).toBeLessThan(10);
  });

  test("runtime 2", () => {
    const input = `-- Mesh combinatorics
type Vertex
type Edge
type Halfedge
type DualEdge
type Triangle
type Corner

constructor MakeEdge(Vertex i, Vertex j) -> Edge
constructor MakeHalfedge(Vertex from, Vertex to) -> Halfedge
constructor MakeTriangle(Vertex i, Vertex j, Vertex k) -> Triangle
constructor MakeCorner( Vertex inner, Vertex outer1, Vertex outer2 ) -> Corner
constructor MakeDualEdge(Triangle a, Triangle b) -> DualEdge

predicate IsBoundaryVertex(Vertex v)
predicate IsBoundaryEdge(Edge e)
predicate IsBoundaryTriangle(Triangle t)

predicate HasLabel(Vertex v)

predicate IsPositivelyOriented(Triangle t)
predicate IsNegativelyOriented(Triangle t)

-- Geometry
type Point
type Circle
type Length

constructor Barycenter(Triangle t) -> Point
constructor Circumcenter(Triangle t) -> Point
constructor Incenter(Triangle t) -> Point

constructor Circumcircle(Triangle t) -> Circle
constructor Incircle(Triangle t) -> Circle

constructor EdgeLength(Edge e) -> Length
constructor DualEdgeLength(DualEdge d) -> Length

constructor Intersection(Edge e, Edge f) -> Point

-- Specific to angle-equivalence.substance
function similarity(Vertex i) -> Vertex

-- Specific to concyclic-pair.substance
predicate IsFlipped(Edge e)
predicate Concyclic(Triangle s, Triangle t)`;

    const start = performance.now();
    getDomainCache(input);
    const end = performance.now();
    const executionTime = end - start;

    console.log(`Domain Cache Execution Time: ${executionTime} ms`);
    expect(executionTime).toBeLessThan(10);
  });
});

// Narrowing of results is handled by a separate Codemirror extension
// End back tick for input strings should be same line as the last character
describe("Autocomplete", () => {
  test("Top level kws basic", async () => {
    const input = `type Circle
    ty`;

    await testDomainAutocomplete(input, domainKws.concat(["Circle"]));
  });

  test("Top level kws mixed", async () => {
    const input = `type Angle
    type Point
    predicate Segment(Point x, Point y)
    f`;

    await testDomainAutocomplete(input, domainKws.concat(["Angle", "Point"]));
  });

  test("Top level kws function ambiguous case 1", async () => {
    const input = `type Angle
    function Bisect(Angle a) -> Angle
    sy`;

    await testDomainAutocomplete(input, domainKws.concat(["Angle"]));
  });

  test("Top level kws ambiguous case 2", async () => {
    const input = `type Angle
    predicate Large(Angle a, Angle b)
    constructor Bisect(Angle a) -> Angle
    sy`;

    await testDomainAutocomplete(input, domainKws.concat(["Angle"]));
  });

  test("Top level kws constructor no output type", async () => {
    const input = `type Angle
    constructor Bisect(Angle a)
    sy`;

    await testDomainAutocomplete(input, domainKws.concat(["Angle"]));
  });

  test("Top level kws constructor with identifier", async () => {
    const input = `type Angle
    constructor Bisect(Angle a) -> Angle b
    sy`;

    await testDomainAutocomplete(input, domainKws.concat(["Angle"]));
  });

  test("Symmetric predicate", async () => {
    const input = `type Point
    type Circle
    symmetric p`;

    await testDomainAutocomplete(input, ["predicate"]);
  });

  // Offset by 1 to put cursor after P rather than after )
  test("Type names, first item in function param list", async () => {
    const input = `type Point
    type Rectangle
    function Box(P)`;

    await testDomainAutocomplete(
      input,
      ["Point", "Rectangle"].concat(builtinTypes),
      1,
    );
  });

  test("Type names, first item in constructor param list", async () => {
    const input = `type Point
    type Rectangle
    constructor Box(P)`;

    await testDomainAutocomplete(
      input,
      ["Point", "Rectangle"].concat(builtinTypes),
      1,
    );
  });

  test("Type names, comma sep in function param list", async () => {
    const input = `type Point
    type Rectangle
    function Box(Point x1, Point y1, Point x2, P)`;

    await testDomainAutocomplete(
      input,
      ["Point", "Rectangle"].concat(builtinTypes),
      1,
    );
  });

  test("Function output type", async () => {
    const input = `type Array
    type Element
    type Program
    function Compute(Array a) -> E`;

    await testDomainAutocomplete(
      input,
      ["Element", "Array", "Program"].concat(builtinTypes),
    );
  });

  test("Constructor output type", async () => {
    const input = `type Array
    type Element
    type Program
    constructor Compute(Array a) -> E`;

    await testDomainAutocomplete(
      input,
      ["Element", "Array", "Program"].concat(builtinTypes),
    );
  });

  test("Subtypes implicit", async () => {
    const input = `type Array
    type Element
    type Program
    Element <: P`;

    await testDomainAutocomplete(
      input,
      ["Element", "Array", "Program"].concat(builtinTypes),
    );
  });

  test("Subtypes explicit", async () => {
    const input = `type Array
    type Element
    type Program
    type Balls <: P`;

    await testDomainAutocomplete(
      input,
      ["Element", "Array", "Program", "Balls"].concat(builtinTypes),
    );
  });
});

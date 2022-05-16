type Type

-- | Set domain program

type Set <: Type
type Point <: Type
type Map

constructor Singleton(Point p) -> Set

function Intersection(Set a, Set b) -> Set
function Union(Set a, Set b) -> Set
function Subtraction(Set a, Set b) -> Set
function CartesianProduct(Set a, Set b) -> Set
function Difference(Set a, Set b) -> Set
function Subset(Set a, Set b) -> Set
function AddPoint(Point p, Set s1) -> Set

predicate From(Map f, Set domain, Set codomain)
predicate Empty(Set s)
predicate Nonempty(Set s)
predicate Intersect(Set s1, Set s2)
predicate NotIntersecting(Set s1, Set s2)
predicate IsSubset(Set s1, Set s2)
predicate NotSubset(Set s1, Set s2)
predicate PointIn(Set s, Point p)
predicate PointNotIn(Set s, Point p)
predicate Injection(Map m)
predicate Surjection(Map m)
predicate Bijection(Map m)
predicate PairIn(Point, Point, Map)

-- predicate Not(Predicate p)

-- These are new, and should go back in the Set domain
notation "A ⊂ B" ~ "IsSubset(A, B)"
notation "p ∈ A" ~ "PointIn(A, p)"
notation "p ∉ A" ~ "PointNotIn(A, p)"

------------------------------
-- | Mesh domain program

type Vertex <: Type
type Edge <: Type
type Face <: Type
type SimplicialSubset -- Subset of a mesh; might not be a simplicial complex
type SimplicialComplex <: SimplicialSubset -- Mesh := SimplicialComplex(2); simplicial complex
type Subcomplex <: SimplicialSubset -- (V, E, F) linked to a mesh; is a simplicial complex

Vertex <: Subcomplex -- TODO: plugin doesn't deal w/ this
Edge <: SimplicialSubset
Face <: SimplicialSubset
-- Subcomplex <: SimplicialComplex
-- TODO: Technically true, but messes up our Style matching

constructor MkEdge(Vertex v1, Vertex v2) -> Edge
constructor MkFace(Edge e1, Edge e2, Edge e3) -> Face

function Star(SimplicialSubset s) -> SimplicialSubset
function Closure(SimplicialSubset s) -> Subcomplex
function Link(SimplicialSubset s) -> SimplicialSubset
function SetMinus(SimplicialSubset s, SimplicialSubset t) -> SimplicialSubset
function Boundary(SimplicialSubset s) -> SimplicialSubset
-- function Union(SimplicialSubset s, SimplicialSubset t) -> SimplicialSubset

-- Generic connectivity and selection predicates
predicate InVE(Vertex v, Edge e)
predicate InEF(Edge e, Face f)

predicate InVS(Vertex v, SimplicialSubset s)
predicate InES(Edge e, SimplicialSubset s)
predicate InFS(Face f, SimplicialSubset s)

-- For plugin use
predicate DeclaredV(Vertex v)
predicate DeclaredE(Edge e)
predicate DeclaredF(Face f)

type Object
Vertex <: Object
Edge <: Object
Face <: Object
SimplicialSubset <: Object
SimplicialComplex <: Object
Subcomplex <: Object

predicate Result(Object o) -- The Style only draws objects that are declared as results

-- Syntactic sugar
notation "Vertex v ∈ K" ~ "Vertex v; InVS(v, K)"
notation "Edge e ∈ K" ~ "Edge e; InES(e, K)"
notation "Face f ∈ K" ~ "Face f; InFS(f, K)"
-- notation "SimplicialSubset S ⊆ K" ~ "SimplicialSubset S; SubsetOf(S, K)"
-- notation "Subcomplex S ⊆ K" ~ "Subcomplex S; SubsetOf(S, K)"

-- These are new, from geometry.dsl
notation "{p, q}" ~ "MkEdge(p, q)"
notation "{p, q, r}" ~ "MkFace(p, q, r)"
-- | above this line, concatenate the two .dom files for Sets and Meshes

------------------------------

-- predicate Identified(Point, Vertex)
predicate Identified(Type, Type)
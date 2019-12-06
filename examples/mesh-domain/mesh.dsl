-- Keenan's notes:
-- http://brickisland.net/DDGSpring2019/
-- https://www.cs.cmu.edu/~kmcrane/Projects/DDG/paper.pdf (chapter 2)
-- http://brickisland.net/DDGSpring2019/2019/01/22/a0-coding-combinatorial-surfaces/

-- geometry-processing-js types and classes:
-- https://geometrycollective.github.io/geometry-processing-js/docs/module-Core.html
-- https://geometrycollective.github.io/geometry-processing-js/docs/module-Projects.SimplicialComplexOperators.html

-- Other resources:
-- https://github.com/cmu462/Scotty3D/wiki/Edge-Flip-Tutorial
-- https://github.com/cmu462/Scotty3D/wiki/Local-Mesh-Operations

type Vertex
type Edge
type Face
type SimplicialSubset -- Subset of a mesh; might not be a simplicial complex
type SimplicialComplex -- Mesh := SimplicialComplex(2); simplicial complex
type Subcomplex -- (V, E, F) linked to a mesh; is a simplicial complex

SimplicialComplex <: SimplicialSubset
Subcomplex <: SimplicialSubset
Vertex <: Subcomplex -- TODO: plugin doesn't deal w/ this
Edge <: SimplicialSubset
Face <: SimplicialSubset

-- Subcomplex <: SimplicialComplex
-- TODO: Technically true, but messes up our Style matching

constructor MkEdge : Vertex v1 * Vertex v2 -> Edge
constructor MkFace : Edge e1 * Edge e2 * Edge e3 -> Face

function Star: SimplicialSubset s -> SimplicialSubset -- This function does not include the simplicial complex that v is in (?)
function StarV: Vertex v -> SimplicialSubset 
function Closure: SimplicialSubset s -> Subcomplex
function Link: SimplicialSubset s -> SimplicialSubset
function SetMinus: SimplicialSubset s * SimplicialSubset t -> SimplicialSubset
function Boundary: SimplicialSubset s -> SimplicialSubset
function Union: SimplicialSubset s * SimplicialSubset t -> SimplicialSubset
-- function Coboundary: SimplicialSubset s -> SimplicialSubset
-- function ToSimplicialComplex: SimplicialSubset s -> SimplicialComplex

-- Math-related predicates
predicate IsSubsetOf: SimplicialSubset s * SimplicialComplex c
predicate IsSimplicialComplex: SimplicialSubset s
predicate IsBoundary: SimplicialSubset s -- Not sure how to check it

predicate InBoundary: Vertex v
predicate InInterior: Vertex v

-- Generic connectivity and selection predicates
-- Does this work WRT subtyping?
predicate InVE: Vertex v * Edge e
predicate InEF: Edge e * Face f

predicate InVS: Vertex v * SimplicialSubset s
predicate InES: Edge e * SimplicialSubset s
predicate InFS: Face f * SimplicialSubset s

-- For plugin use
predicate DeclaredV: Vertex v
predicate DeclaredE: Edge e
predicate DeclaredF: Face f

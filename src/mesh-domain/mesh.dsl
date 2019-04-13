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
type SSubset
type SComplex -- Mesh := SComplex(2)
type Subcomplex -- (V, E, F) linked to a mesh

SComplex <: SSubset
Subcomplex <: SSubset
-- Subcomplex <: SComplex
-- TODO: Technically true, but messes up our Style matching

constructor MkEdge : Vertex v1 * Vertex v2 -> Edge
constructor MkFace : Edge e1 * Edge e2 * Edge e3 -> Face

function Star: SComplex c -> SSubset
function StarV: Vertex v -> SSubset -- This function does not include the simplicial complex that v is in
function Closure: SSubset s -> Subcomplex
function Link: SSubset s -> SSubset
function LinkV: Vertex c -> SSubset
function Boundary: SComplex s -> SSubset
function Coboundary: SSubset s -> SSubset
function ToSComplex: SSubset s -> SComplex

-- Math-related predicates
predicate IsSubsetOf: SSubset s * SComplex c
predicate IsSComplex: SSubset s
predicate Pure: SSubset s
predicate IsBoundary: SSubset s -- Not sure how to check it

-- Generic connectivity and selection predicates
-- Does this work WRT subtyping?
predicate InVE: Vertex v * Edge e
predicate InEF: Edge e * Face f

predicate InVS: Vertex v * SSubset s
predicate InES: Edge e * SSubset s
predicate InFS: Face f * SSubset s

-- For plugin use
predicate DeclaredV: Vertex v
predicate DeclaredE: Edge e
predicate DeclaredF: Face f

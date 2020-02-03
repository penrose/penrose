-- Modeled after geometry-processing-js types and classes
-- https://geometrycollective.github.io/geometry-processing-js/docs/module-Core.html
-- https://geometrycollective.github.io/geometry-processing-js/docs/module-Projects.SimplicialComplexOperators.html

tconstructor Edge : type
tconstructor Halfedge : type
tconstructor Vertex : type
tconstructor Face : type
-- tconstructor SimplicialComplex : type
-- tconstructor SimplicialSet : type
tconstructor Mesh : type
tconstructor MeshSubset : type

-- Mesh <: SimplicialComplex
MeshSubset <: Mesh

vconstructor MkEdge(v1 : Vertex, v2 : Vertex) : Edge
vconstructor MkHalfedge(twin : Halfedge, next : Halfedge, edge : Edge) : Halfedge

-- vconstructor Face(edges : List(Edge)) : Face -- In CCW order
-- vconstructor Mesh(vs : List(Vertex), es : List(Edge), hs : List(Halfedge), fs : List(Face))

-- TODO: check if these types are right

operator Star(v : Vertex) : MeshSubset
operator Closure(m : MeshSubset) : Mesh
operator Link(m : MeshSubset) : Mesh
operator EdgeFlip(m : Mesh) : Mesh

predicate SelectedV(v : Vertex) : Prop
predicate InVS(v : Vertex, S : Mesh) : Prop
predicate BoundaryFace(f : Face) : Prop
predicate Subset(s : MeshSubset, m : Mesh) : Prop

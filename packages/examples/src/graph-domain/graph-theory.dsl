type Vertex
type Edge
type Graph

-- Subtypes
type Face <: Graph
type Path <: Graph

constructor MkEdge(Vertex from, Vertex to) -> Edge

-- COMBAK: Add lists
-- constructor MkGraph(List(Vertex) vertices, List(Edge) edges) -> Graph
-- operator Neighbors(Vertex v) -> List(Vertex)

predicate Directed(Edge e)
predicate Undirected(Edge e)

predicate SelectedV(Vertex v)
predicate SelectedE(Edge e)

-- predicate InV(Vertex v, Graph G)
-- predicate InE(Edge e, Graph G)
-- predicate InF(Face f, Graph G)

-- TODO syntactic sugar
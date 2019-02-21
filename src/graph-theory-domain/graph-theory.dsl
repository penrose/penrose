-- Main types
tconstructor List ('X : type) : type
vconstructor Cons ['X : type] (head : 'X, tail : List('X)) : List('X)
vconstructor Nil ['X : type] : List('X)

tconstructor Vertex : type
tconstructor Edge : type
tconstructor Graph : type

-- Subtypes
tconstructor UndirectedEdge : type
tconstructor DirectedEdge : type
tconstructor UndirectedGraph : type
tconstructor DirectedGraph : type
tconstructor Tree : type
tconstructor BinaryTree : type
tconstructor Face : type
tconstructor Path : type
tconstructor Cycle : type

-- vconstructor GenerateGraph() : Graph
vconstructor MkGraph(vertices : List(Vertex), edges : List(Edge)) : Graph
vconstructor MkEdge(v1 : Vertex, v2 : Vertex) : Edge
vconstructor MkUndirectedEdge(v1 : Vertex, v2 : Vertex) : UndirectedEdge
vconstructor MkDirectedEdge(v1 : Vertex, v2 : Vertex) : DirectedEdge 
-- Would be nice to name these fields "from" and "to"

UndirectedEdge <: Edge
DirectedEdge <: Edge
UndirectedGraph <: Graph
DirectedGraph <: Graph
Tree <: Graph
BinaryTree <: Tree
Face <: Graph
Path <: Graph
Cycle <: Graph

operator Union(G1 : Graph, G2 : Graph) : Graph
operator Product(G1 : Graph, G2 : Graph) : Graph
operator Neighbors(v : Vertex) : List(Vertex)
operator FindFace(G : Graph) : Face
operator FindVertex(G : Graph) : Vertex

predicate SelectedV(v : Vertex) : Prop
predicate SelectedE(e : Edge) : Prop
predicate Colored(G : Graph) : Prop
predicate FullyConnected(G : Graph) : Prop
predicate Bipartite(G : Graph) : Prop
predicate SmallerDegree(v1 : Vertex, v2 : Vertex) : Prop
predicate InV(v : Vertex, G : Graph) : Prop
predicate InE(e : Edge, G : Graph) : Prop
predicate InF(f : Face, G : Graph) : Prop

-- TODO: syntactic sugar
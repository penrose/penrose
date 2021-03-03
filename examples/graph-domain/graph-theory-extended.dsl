type List ('X)
vconstructor Cons ['X] (head : 'X, tail : List('X)) : List('X)
vconstructor Nil ['X] : List('X)

-- Main types
type Vertex
type Edge
type Graph

-- Subtypes
type UndirectedEdge
type DirectedEdge
type UndirectedGraph
type DirectedGraph
type Tree
type BinaryTree
type Face
type Path
type Cycle

UndirectedEdge <: Edge
DirectedEdge <: Edge
UndirectedGraph <: Graph
DirectedGraph <: Graph
Tree <: Graph
BinaryTree <: Tree
Face <: Graph
Path <: Graph
Cycle <: Graph

constructor MkGraph : List(Vertex) vertices * List(Edge) edges -> Graph
constructor MkEdge : Vertex from * Vertex to -> Edge
constructor MkUndirectedEdge : Vertex from * Vertex to -> UndirectedEdge
constructor MkDirectedEdge : Vertex from * Vertex to -> DirectedEdge 

operator Union : Graph G1 * Graph G2 -> Graph
operator Product : Graph G1 * Graph G2 -> Graph
operator Neighbors : Vertex v -> List(Vertex)
operator FindFace : Graph G -> Face
operator FindVertex : Graph G -> Vertex

predicate SelectedV : Vertex v
predicate SelectedE : Edge e
predicate Colored : Graph G
predicate FullyConnected : Graph G
predicate Bipartite : Graph G
predicate SmallerDegree : Vertex v1 * Vertex v2
predicate InV : Vertex v * Graph G
predicate InE : Edge e * Graph G
predicate InF : Face f * Graph G

-- TODO syntactic sugar
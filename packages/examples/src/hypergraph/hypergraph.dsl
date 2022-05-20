
type Graph

type Layer1
type Layer2
type Layer3

type Node
type Edge

predicate GraphHasLayer1(Graph g, Layer1 l)
predicate GraphHasLayer2(Graph g, Layer2 l)
predicate GraphHasLayer3(Graph g, Layer3 l)

predicate GraphHasNode(Graph g, Node n)

predicate Layer1HasEdge(Layer1 l, Edge e)
predicate Layer2HasEdge(Layer2 l, Edge e)
predicate Layer3HasEdge(Layer3 l, Edge e)

predicate EdgeHasNodeInLayer1(Edge e, Node n)
predicate EdgeHasNodeInLayer2(Edge e, Node n)
predicate EdgeHasNodeInLayer3(Edge e, Node n)

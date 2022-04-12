type Graph
type Variable

predicate not(Graph g1)
predicate and(Graph g1, Graph g2)
function if(Graph, Graph) -> Graph
function or(Graph, Graph) -> Graph

predicate equal(Variable v1, Variable v2)
predicate invisibleGraph(Graph p1)
predicate some(Variable, Graph, Graph)
predicate all(Variable, Graph, Graph)

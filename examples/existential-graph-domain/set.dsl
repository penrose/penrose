type Graph
type Variable

predicate not : Graph  g1
predicate and : Graph g1 * Graph g2
function if : Graph * Graph -> Graph
function or : Graph * Graph -> Graph

predicate some : Variable * Graph * Graph
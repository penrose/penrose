type Graph
type Variable

-- TODO: would be good to unify the interface for `not`, `and`, and `or`. Or just anonymous function instead? Maybe easier for nesting, too.
predicate not : Graph  g1
predicate and : Graph g1 * Graph g2
function if : Graph * Graph -> Graph
-- NOTE: "the lack the uniformity"
-- the function lets us access the variables created by the relationship
-- cory: the keyword is "recursion"
function or : Graph * Graph -> Graph

predicate equal: Variable v1 * Variable v2
predicate invisibleGraph: Graph p1
predicate some : Variable * Graph * Graph
predicate all : Variable * Graph * Graph

-- proof steps
-- NOTE: this is the first construct that's in the code, but not math. 
predicate connect: Graph * Graph
predicate doubleElliminate : Graph
predicate deiterate: Graph
predicate erase: Graph
type Node
type DirectedEdge
type UndirectedEdge

function CreateDirectedEdge : Node n1 * Node n2 -> DirectedEdge
function CreateUndirectedEdge : Node n1 * Node n2 -> UndirectedEdge

notation "n1 -> n2" ~ "CreateDirectedEdge(n1, n2)"
notation "n1 -- n2" ~ "CreateUndirectedEdge(n1, n2)"



type Node

type OptType
type Union
type Objective
type Constraint
type Label
type Function
type Arg
type Const

OptType <: Node
Union <: Node
Objective <: Node
Constraint <: Node
Label <: Node
Function <: Node
Arg <: Node
Const <: Node

-- function apply: Node parent -> Node child1 -> Node child2 -> 

-- TODO: we can't take lists rn!!!
-- would like to write y = f[x]
predicate to: Node parent * Node child
predicate ParentOf2: Node parent * Node child1 * Node child2
predicate ParentOf3: Node parent * Node child1 * Node child2 * Node child3

notation "p -> c" ~ "to(p, c)"
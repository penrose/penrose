type Object
type Morphism

function tensor(Object, Object) -> Object
-- notation “a * b” ~ “tensor(a, b)” -- Could use unicode

predicate ChildOf(Object, Morphism)
predicate NotChildOf(Object, Morphism)

constructor join(Object first, Object second) -> Morphism
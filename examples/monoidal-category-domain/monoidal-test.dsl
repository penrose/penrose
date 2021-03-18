type Object
type Morphism

function tensor : Object * Object -> Object
-- notation “a * b” ~ “tensor(a, b)” -- Could use unicode

predicate InputOf : Object * Morphism
predicate OutputOf : Object * Morphism
predicate NotInputOf : Object * Morphism

constructor join : Object first * Object second -> Morphism
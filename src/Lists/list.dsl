tconstructor List('X : type) : type

vconstructor Nil ['X : type] : List('X)
vconstructor Cons ['X : type] (head : 'X , tail : List('X)) : List('X)

predicate InList ['X : type] (x: 'X, xs: List('X)) : Prop
predicate Sublist ['X : type] (l1: List('X), l2: List('X)) : Prop

tconstructor Scalar : type
tconstructor VectorSpace : type
tconstructor Vector : type
tconstructor LinearMap : type

operator add (v1 : Vector, v2 : Vector) : Vector
operator neg (v1 : Vector) : Vector
operator scale (c : Scalar, v : Vector) : Vector
operator norm (v1 : Vector) : Scalar
operator innerProduct (v1 : Vector, v2 : Vector) : Scalar
operator determinant (v1 : Vector, v2 : Vector) : Scalar
operator apply (f : LinearMap, v : Vector) : Vector

predicate In(v : Vector, V : VectorSpace) : Prop
predicate From(f : LinearMap, V : VectorSpace, W : VectorSpace) : Prop
predicate Not(p : Prop) : Prop

-- Syntactic Sugar Definition

StmtNotation  "det( v1, v2) " -> "determinant(v1 , v2)   "

/*
ExprNotation "add($v1 ,$v2)" -> "$v1 + $v2" (at level 2, right associativity)
ExprNotation "scale($s , $v1)" -> "$s * $v1" (at level 1, right associativity)
ExprNotation "norm($v1)" -> "|$v1|"
ExprNotation "neg($v1)" -> "-$v1"
ExprNotation "innerProduct($v1 , $v2)" -> "<$v1, $v2>"
ExprNotation "apply($f,$v)" -> "$f($v)"

StmtNotation "LinearMap $f ; From($f,$U,$V)" -> "LinearMap $f : $U → $V"
StmtNotation "In( [.] , $V)" -> "[...] ∈ $V"
StmtNotation "Scalar $x ; $x := $c" -> "Scalar $x := $c"
*/

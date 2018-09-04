------------------------- Type Constructors ------------------------------------
tconstructor Scalar : type
tconstructor VectorSpace : type
tconstructor Vector : type
tconstructor LinearMap : type

operator addV (v1 : Vector, v2 : Vector) : Vector
operator addS (c1 : Scalar, c2 : Scalar) : Scalar
operator neg (v1 : Vector) : Vector
operator scale (c : Scalar, v : Vector) : Vector
operator norm (v1 : Vector) : Scalar
operator innerProduct (v1 : Vector, v2 : Vector) : Scalar
operator determinant (v1 : Vector, v2 : Vector) : Scalar
operator apply (f : LinearMap, v : Vector) : Vector

---------------------------- Predicates ----------------------------------------
predicate In(v : Vector, V : VectorSpace) : Prop
predicate From(f : LinearMap, V : VectorSpace, W : VectorSpace) : Prop
predicate Not(p : Prop) : Prop

------------------------- Syntactic Sugar Definition ---------------------------


StmtNotation  "det(v1, v2) " -> "determinant(v1 , v2)"
StmtNotation "LinearMap f : U → V" -> "LinearMap f;From(f,U,V)"
StmtNotation "v1 + v2" -> "add(v1,v2)"
StmtNotation "-v1" -> "neg(v1)"
StmtNotation "Vector a ∈ U" -> "Vector a;In(a,U)"
StmtNotation "|y1|" -> "norm(y1)"
StmtNotation "<v1,v2>" -> "innerProduct(v1 , v2)"
StmtNotation "s * v1" -> "scale(s , v1)"
StmtNotation "f(v)" -> "apply(f,v)"

-- StmtNotation "Vector ... ∈ U" -> "Vector ... ;In(..,U)"


/* TODO in the near future
StmtNotation "Scalar $x ; $x := $c" -> "Scalar $x := $c"

ExprNotation "v1 + v2" -> "add(v1 ,v2)"  (at level 2, right associativity)
ExprNotation "s * v1" -> "scale(s , v1)"   (at level 1, right associativity)
ExprNotation "|v1|" -> "norm(v1)"
ExprNotation "-v1" -> "neg(v1)"
ExprNotation "<v1, v2>" -> "innerProduct(v1 , v2)"
ExprNotation "f(v)" -> "apply(f,v)"
*/

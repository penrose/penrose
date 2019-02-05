------------------- Type Constructors --------------------

tconstructor Scalar : type
tconstructor VectorSpace : type
tconstructor Vector : type
tconstructor LinearMap : type

-------------------- Operators -----------------------

operator neg (v : Vector) : Vector
operator scale (c : Scalar, v : Vector) : Vector
operator addV (v1 : Vector, v2 : Vector) : Vector
operator addS(s1 : Scalar, s2 : Scalar) : Scalar
operator norm (v1 : Vector) : Scalar
operator innerProduct (v1 : Vector, v2 : Vector) : Scalar
operator determinant (v1 : Vector, v2 : Vector) : Scalar
operator apply (f : LinearMap, v : Vector) : Vector

-------------------- Predicates -----------------------

predicate In (v : Vector, V : VectorSpace) : Prop
predicate From (f : LinearMap, V : VectorSpace, W : VectorSpace) : Prop
predicate Not (p1 : Prop) : Prop

--------------- Syntactic Sugar Definition -----------------

StmtNotation "det(v1, v2)" -> "determinant(v1 , v2)"
StmtNotation "LinearMap f : U → V" -> "LinearMap f;From(f,U,V)"
StmtNotation "v1 + v2" -> "addV(v1,v2)"
StmtNotation "-v1" -> "neg(v1)"
StmtNotation "Vector a ∈ U" -> "Vector a;In(a,U)"
StmtNotation "|y1|" -> "norm(y1)"
StmtNotation "<v1,v2>" -> "innerProduct(v1 , v2)"
StmtNotation "s * v1" -> "scale(s , v1)"
StmtNotation "Scalar c := " -> "Scalar c ;c := "
StmtNotation "f(v)" -> "apply(f,v)"

-- Examples for prelude, just for reproducing (Should be removed)

--value T : VectorSpace
-- value T1 : VectorSpace

------------------- Type Constructors --------------------

type Scalar : type
type VectorSpace : type
type Vector : type
type LinearMap : type

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

notation "det(v1, v2)" ~ "determinant(v1 , v2)"
notation "LinearMap f : U → V" ~ "LinearMap f;From(f,U,V)"
notation "v1 + v2" ~ "addV(v1,v2)"
notation "-v1" ~ "neg(v1)"
notation "Vector a ∈ U" ~ "Vector a;In(a,U)"
notation "|y1|" ~ "norm(y1)"
notation "<v1,v2>" ~ "innerProduct(v1 , v2)"
notation "s * v1" ~ "scale(s , v1)"
notation "Scalar c := " ~ "Scalar c ;c := "
notation "f(v)" ~ "apply(f,v)"

-- Examples for prelude, just for reproducing (Should be removed)

--value T : VectorSpace
-- value T1 : VectorSpace

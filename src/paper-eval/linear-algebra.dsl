tconstructor Scalar : type
tconstructor VectorSpace : type
tconstructor Vector : type
tconstructor LinearMap : type

operator Add (v1 : Vector, v2 : Vector) : Vector
operator Scale (c : Scalar, v : Scalar) : Vector
operator Norm (v1 : Vector) : Scalar
operator InnerProduct (v1 : Vector, v2 : Vector) : Scalar
operator Determinant (v1 : Vector, v2 : Vector) : Scalar
operator Apply (f : LinearMap, v : Vector) : Vector

predicate In(v : Vector, V : VectorSpace) : Prop
predicate From(f : LinearMap, V : VectorSpace, W : VectorSpace) : Prop
predicate Not(p : Prop) : Prop

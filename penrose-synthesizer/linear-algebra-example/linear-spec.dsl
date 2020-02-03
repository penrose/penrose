------------------- Type Constructors --------------------

type Scalar
type VectorSpace
type Vector
type LinearMap

-------------------- Operators -----------------------

function neg: Vector v -> Vector
function scale: Scalar c * Vector v -> Vector cv
function addV: Vector * Vector -> Vector
function addS: Scalar s1 * Scalar s2 -> Scalar
function norm: Vector v -> Scalar
function innerProduct: Vector * Vector -> Scalar
function determinant: Vector * Vector -> Scalar
function apply: LinearMap f * Vector -> Vector

-------------------- Predicates -----------------------

predicate In: Vector * VectorSpace V
predicate From: LinearMap V * VectorSpace domain * VectorSpace codomain
-- predicate Not: Prop p1
predicate Orthogonal: Vector v1 * Vector v2


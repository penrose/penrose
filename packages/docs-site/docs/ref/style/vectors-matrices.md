# Vectors and Matrices

Style supports dense n-dimensional vector and matrix types, and standard operations on these types. These types behave largely like small, dense matrix types found in other languages (such as GLSL), with some specific differences noted below. Note that these types are meant largely for manipulating small 2D, 3D, and 4D vectors/matrices in the context of standard graphics operations (transformations, perspective projection, etc.), and may not perform well for larger matrix manipulations. Like all other objects in Style, the value of any vector or matrix entry can be declared as unknown (`?`) and determined automatically via optimization by the layout engine.

Additional vector and matrix functions may be available—refer to the [function library](functions) for additional information.

#### Vector and matrix types

Style is designed to support n-dimensional dense vectors of type `vecN`, and square n-dimensional matrices of type `matNxN`, where in both cases `N` is an integer greater than or equal to 2.  Some types commonly used for diagramming are `vec2` (representing points in the plane) and `mat3x3` (representing linear transformations of three-dimensional space).

Note that some library functions are meaningful only for vectors or matrices of a specific size.  For instance, the function `cross(u,v)` computes a cross product, which is well-defined only in three dimensions, and hence assumes that both `u` and `v` have type `vec3`.

#### Initializing vectors and matrices

A vector is constructed by specifying its components. For instance,

```style
vec2 u = (1.23, 4.56)
```

constructs a 2-dimensional vector with `x`-component `1.23` and `y`-component `4.56`. As noted above, unknown values can be used as components, e.g.,

```style
vec2 p = (?, 0.0)
```

specifies a point `p` that sits on the `x`-axis with an unknown `x`-coordinate which is determined by the optimizer, according to any constraints and objectives involving `p`. More advanced initializers (e.g., initializing a 3-vector from a 2-vector and a scalar) are currently not supported, but are planned for future language versions. In most cases, the same functionality can currently be emulated by directly referencing components of a vector, e.g.,

```style
vec3 a = ( b[0], b[1], 1.0 )
```

A matrix is constructed by specifying a list of vectors. Each vector corresponds to a row (not a column) of the matrix. For instance,

```style
mat2x2 A = ((1,2),(3,4))
```

initializes a 2x2 matrix where the top row has entries 1, 2 and the bottom row has entries 3, 4. Rows can also reference existing vectors, e.g.,

```style
vec2 a1 = (1, 2)
vec2 a2 = (3, 4)
mat2x2 A = (a1, a2)
```

builds the same matrix as above. As with vectors, matrix entries can be unknown. E.g.,

```style
scalar d = ?
mat3x3 D = ((d, 0, 0), (0, d, 0), (0, 0, d))
```

describes a 3x3 diagonal matrix, where all three diagonal entries take the same, undetermined value `d`.

#### Vector and matrix element access

Individual elements of a `vecN` can be accessed using square brackets, and an index `i` between `0` and `N`-1 (inclusive). For instance,

```style
vec3 u = (1, 2, 3)
scalar y = u[1]
```

will extract the `y`-coordinate of `u` (i.e. `y=2`). Matrix entries are similarly accessed:

```style
mat2x2 M = ((?, ?), (?, ?))
scalar trM = M[0][0] + M[1][1]
```

constructs an expression for the trace of `M`. In this case, since the elements of `M` are declared as unknown scalars, the value of the trace will depend on the entry values of the optimized matrix.

**Note:** At present, a single index cannot be used to extract a vector from a matrix or list.  For instance, the following usage is not valid:

```style
mat3x3 M = ( (1,2,3), (4,5,6), (7,8,9) )
vec3 row1 = M[0] -- attempt to extract row 1 of M
```

Instead, the individual components must be enumerated:

```style
mat3x3 M = ( (1,2,3), (4,5,6), (7,8,9) )
vec3 row1 = ( M[0][0], M[0][1], M[0][2] ) -- extract row 1 of M
```

More concise indexing is planned for future language versions; see [issue #1509](https://github.com/penrose/penrose/issues/1509).


## Vector and matrix operations

Like `scalar` variables, vector and matrix types support a variety of standard arithmetic operations, listed below.  Here we assume that `c` and `d` have type `scalar`, `u` and `v` have type `vecN`, and `A` and `B` have type `matNxN` (all for the same size `N`).

Note that an _elementwise_ operation is one that gets applied independently to each entry of a vector or matrix. For instance, if `u = (6, 8, 9)` and `v = (3, 2, 3)`, then the elementwise division operation `u ./ v` yields the vector `(2, 4, 3)` (i.e. six divided by three, eight divided by two, and nine divided by three).

**The `then` keyword.** Style provides a special keyword `then` that can be useful for describing sequences of spatial transformations.  In English, one might naturally say something like, "_flip it over, then put it in the oven"_ to mean that a dish should first be flipped over, and only _then_ be put in the oven.  (In fact, reversing this order could be quite dangerous!)  Likewise, the `then` keyword in Style means that a sequence of transformations should be applied from left-to-right, in the same order as in natural language.  For example,

```style
mat4x4 transform = translate(x,y) then rotate(theta) then scale(a,b)
```

indicates that translation happens first, rotation happens second, and scaling happens third.  The resulting transformation is equivalent to writing matrix multiplication in the usual right-to-left order, i.e.,

```style
mat4x4 transform = rotate(theta) * scale(a,b) * translate(x,y)
```

In general, `A1 then A2 then ... then An = An * ... * A2 * A1`.  The use of the `then` keyword simply makes it easier in some cases to understand the correct meaning of a sequence of operations, by just reading code in the usual left-to-right order.

#### Scalar-Vector

- `c * v` — product of `v` and `c` (from the left)
- `v * c` — product of `v` and `c` (from the right)
- `v / c` — quotient of `v` by `c`

#### Scalar-Matrix

- `c * A` — product of `A` and `c` (from the left)
- `A * c` — product of `A` and `c` (from the right)
- `A / c` — quotient of `A` by `c`

#### Vector-Vector

- `u + v` — sum of `u` and `v`
- `u - v` — difference of `u` and `v`
- `u .* v` — elementwise product of `u` and `v`
- `u ./ v` — elementwise quotient of `u` and `v`

#### Vector-Matrix

- `A*u` — matrix-vector product `Au`
- `u*A` — matrix vector product `uᵀA`

#### Matrix-Matrix

- `A * B` — matrix-matrix product `AB`
- `A + B` — sum of `A` and `B`
- `A - B` — difference of `A` and `B`
- `A .* B` — elementwise product of `A` and `B`
- `A ./ B` — elementwise quotient of `A` and `B`
- `A'` — matrix transpose `Aᵀ`
- `A then B` — matrix-matrix product `BA`

## Vector and matrix functions

A variety of methods are available for constructing standard matrices.

## 2D and 3D transformation matrices

_TODO explain all this business about linear vs. affine, homogeneous coordinates, etc., and how the library is designed.  Explain the naming for basic, composable 2D transformations is just the bare words._


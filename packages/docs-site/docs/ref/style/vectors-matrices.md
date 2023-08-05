# Vectors and Matrices

Style supports dense n-dimensional vector and matrix types, and standard operations on these types. These types behave largely like small, dense matrix types found in other languages (such as GLSL), with some specific differences noted below. Note that these types are meant largely for manipulating small 2D, 3D, and 4D vectors/matrices in the context of standard graphics operations (transformations, perspective projection, etc.), and may not perform well for larger matrix manipulations. Like all other objects in Style, the value of any vector or matrix entry can be declared as unknown (`?`) and determined automatically via optimization by the layout engine.

Additional vector and matrix functions may be available—refer to the [function library](functions) for additional information.

#### Vector and matrix types

Style is designed to support n-dimensional dense vectors of type `vecN`, and square n-dimensional matrices of type `matNxN`, where in both cases `N` is an integer greater than or equal to 2. Some types commonly used for diagramming are `vec2` (representing points in the plane) and `mat3x3` (representing linear transformations of three-dimensional space).

Note that some library functions are meaningful only for vectors or matrices of a specific size. For instance, the function `cross(u,v)` computes a cross product, which is well-defined only in three dimensions, and hence assumes that both `u` and `v` have type `vec3`.

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

::: warning

At present, a single index cannot be used to extract a vector from a matrix or list. For instance, the following usage is not valid:

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

:::

## Vector and matrix operations

Like `scalar` variables, vector and matrix types support a variety of standard arithmetic operations, listed below. Here we assume that `c` and `d` have type `scalar`, `u` and `v` have type `vecN`, and `A` and `B` have type `matNxN` (all for the same size `N`).

Note that an _elementwise_ operation is one that gets applied independently to each entry of a vector or matrix. For instance, if `u = (6, 8, 9)` and `v = (3, 2, 3)`, then the elementwise division operation `u ./ v` yields the vector `(2, 4, 3)` (i.e. six divided by three, eight divided by two, and nine divided by three).

**The `then` keyword.** Style provides a special keyword `then` that can be useful for describing sequences of spatial transformations. In English, one might naturally say something like, "_flip it over, then put it in the oven"_ to mean that a dish should first be flipped over, and only _then_ be put in the oven. (In fact, reversing this order could be quite dangerous!) Likewise, the `then` keyword in Style means that a sequence of transformations should be applied from left-to-right, in the same order as in natural language. For example,

```style
mat4x4 transform = translate(x,y) then rotate(theta) then scale(a,b)
```

indicates that translation happens first, rotation happens second, and scaling happens third. The resulting transformation is equivalent to writing matrix multiplication in the usual right-to-left order, i.e.,

```style
mat4x4 transform = rotate(theta) * scale(a,b) * translate(x,y)
```

In general, `A1 then A2 then ... then An = An * ... * A2 * A1`. The use of the `then` keyword simply makes it easier in some cases to understand the correct meaning of a sequence of operations, by just reading code in the usual left-to-right order.

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

A variety of methods are available for constructing standard matrices (click through for more detailed description):

- [identity](functions#computation-identity)
- [diagonal](functions#computation-diagonal)
- [trace](functions#computation-trace)
- [determinant](functions#computation-determinant)
- [inverse](functions#computation-inverse)
- [outerProduct](functions#computation-outerProduct)
- [crossProductMatrix](functions#computation-crossProductMatrix)
- [matrix](functions#computation-matrix)
- [matrix3d](functions#computation-matrix3d)

## 2D and 3D transformation matrices

Penrose provides matrices which express basic 2D and 3D spatial transformations (rotation, translation, etc.). The definitions of these functions are designed to closely follow the standard definitions found in SVG and CSS (2D) and OpenGL (3D), so that code from these languages can be easily ported to Style. Since 2D is the most common case, 2D transformations are referred to by the simple names [`rotate`](functions#computation-rotate), [`translate`](functions#computation-translate), [`scale`](functions#computation-scale), [`skew`](functions#computation-skew), and [`shear`](functions#computation-shear). Other functions are referred to by dimension, e.g., `rotate3d()`; a suffix of `h` indicates that a transformation is expressed in homogeneous coordinates (e.g., `rotate3dh()`). A full list is given below (click through for more detailed descriptions).

### Composition of transformations

As noted above, transformations can be combined using the `then` keyword. For instance,

```style
mat4x4 transform = translate(x,y) then rotate(theta) then scale(a,b),
```

which performs the given transformations in left-to-right order. (If preferred, one can also compose transformations in right-to-left order using ordinary matrix multiplication.)

### Linear vs. affine transformations, homogeneous coordinates

An important concept when working with spatial transformations is the distinction between _linear_ and _affine_ transformations. A linear transformation of a point $x$ in $n$-dimensional space is one that can be expressed via matrix-vector multiplication $Ax$ for some $n \times n$ matrix $A$. Whereas some basic transformations, like rotation, can be expressed this way, other standard transformations cannot. For instance, there is no way to encode translation by a vector $u$ as a linear transformation, i.e., there is no $n \times n$ matrix $A$ such that $Ax = x + u$. This transformation is an example of an _affine transformation_. Any affine transformation can be encoded as a linear transformation "one dimension up," by writing our points in _homogeneous coordinates_. In particular, if we work with the coordinates $\hat{x} = (x,1)$ (i.e., we append 1 to the end of the original coordinate vector), then we can find a matrix $\hat{A}$ such that $\hat{y} := \hat{A}\hat{x}$ describes the result of translation, also in homogeneous coordinates. To recover the translated vector in ordinary Cartesian coordinates, we then divide the first $n$ components of $\hat{y}$ by its final, "extra" coordinate. For further information, see [this discussion](https://en.wikipedia.org/wiki/Homogeneous_coordinates#Use_in_computer_graphics_and_computer_vision).

Transformations in Style follow a standard naming convention:

- Transformations with no suffix refer to 2D transformations expressed in homogeneous coordinates (`rotate()`, `translate()`, `scale()`, etc.). These functions can all be composed with each-other as one would naturally expect.
- Transformations with a suffix `2D` refer to linear 2D transformations, expressed in ordinary Cartesian coordinates (`rotate2D()`, `scale2D()`, etc.). Note that there is no `translate2D()`, because this transformation is affine; likewise, linear 2D transformations cannot be directly composed with affine 2D transformations.
- Transformations with a suffix `3D` refer to linear 3D transformations, expressed in ordinary Cartesian coordinates (`rotate3D()`, `scale3D()`, etc.).
- Transformations with a suffix `3dh` refer to affine 3D transformations, expressed in homogeneous coordinates (`rotate3Dh()`, `translate3Dh`, etc.).

Style also provides helper functions for converting to/from homogeneous coordinates:

- [fromHomogeneous](functions#computation-fromHomogeneous)
- [fromHomogeneousList](functions#computation-fromHomogeneousList)
- [toHomogeneous](functions#computation-toHomogeneous)
- [toHomogeneousList](functions#computation-toHomogeneousList)
- [toHomogeneousMatrix](functions#computation-toHomogeneousMatrix)

Example usage:

```style
vec3 p = (1,2,3) -- express a 3D point in ordinary Cartesian coordinates

-- express a 3D transformation in 3+1 homogeneous coordinates
mat4x4 A = translate3dh(x,y,z) then rotate3dh(theta,u) then scale3dh(a,b,c)

-- transform the point by converting to/from homogeneous coordinates
vec3 q = fromHomogeneous( A * toHomogeneous(p) )
```

#### 2D transformations (affine)

- [rotate](functions#computation-rotate)
- [scale](functions#computation-scale)
- [skew](functions#computation-skew)
- [shear](functions#computation-shear)
- [translate](functions#computation-translate)

#### 2D transformations (linear)

- [rotate2d](functions#computation-rotate2d)
- [scale2d](functions#computation-scale2d)
- [skew2d](functions#computation-skew2d)
- [shear2d](functions#computation-shear2d)

#### 3D transformations (linear)

- [rotate3d](functions#computation-rotate3d)
- [scale3d](functions#computation-scale3d)
- [shear3d](functions#computation-shear3d)

#### 3D transformations (affine)

- [rotate3dh](functions#computation-rotate3dh)
- [scale3dh](functions#computation-scale3dh)
- [shear](functions#computation-shear)
- [translate3dh](functions#computation-translate3dh)

### Camera matrices

Penrose also provides camera projection matrices, which are helpful for making 3D diagrams (see especially the [`dinoshade` example](https://github.com/penrose/penrose/tree/main/packages/examples/src/dinoshade)). These functions closely match the standard definitions in OpenGL:

- [lookAt](functions#computation-lookAt)
- [perspective](functions#computation-perspective)
- [ortho](functions#computation-ortho)
- [project](functions#computation-project)
- [projectDepth](functions#computation-projectDepth)
- [projectList](functions#computation-projectList)

Finally, the utility function `matrixMultiplyList` is useful for applying the same transformation to many points (e.g., points in a 3D mesh):

- [matrixMultiplyList](functions#computation-matrixMultiplyList)

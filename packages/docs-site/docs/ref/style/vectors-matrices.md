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

A variety of methods are available for constructing standard

#### `identity(n)`

Returns the $n \times n$ identity matrix

$$I = \left[ \begin{array}{cccc} 1 & 0 & \cdots & 0 \\ 0 & 1 & \cdots & 0 \\ \vdots & \vdots & \ddots & \vdots \\ 0 & 0 & \cdots & 1 \end{array} \right]$$


#### `diagonal(v)`

Given a vector `v` of length `n`, returns the diagonal matrix

$$D = \left[ \begin{array}{cccc} v_1 & 0 & \cdots & 0 \\ 0 & v_2 & \cdots & 0 \\ \vdots & \vdots & \ddots & \vdots \\ 0 & 0 & \cdots & v_n \end{array} \right]$$

#### `trace(A)`

Given a square matrix $A$, returns the trace $\text{tr}(A)$, equal to the sum of its diagonal entries.

#### `determinant(A)`

Given a $2 \times 2$, $3 \times 3$, or $4 \times 4$ matrix $A$, returns its determinant $\text{det}(A)$.

#### `inverse(A)`

Given a $2 \times 2$, $3 \times 3$, or $4 \times 4$ matrix $A$, returns its inverse.  If the matrix is not invertible, evaluation of this function may produce a numerically invalid matrix (with `INF` or `NaN` entries).

#### `outerProduct(u,v)`

Given two vectors $u$, $v$ of equal length $n$, returns the outer product matrix $A$ with entries $A_{ij} = u_i v_j$.

#### `crossProductMatrix`

Given a 3-vector `v`, returns a 3x3 skew symmetric matrix `A` such that `Au = v x u` for any vector `u`.

  params: [
    { name: "v", description: "Vector `v`", type: realNT() }
  ],

#### `matrix`

Specifies a transformation matrix `[ a c e; b d f; 0 0 1]`, mirroring the SVG/CSS `matrix` transform function.

  params: [
    { name: "a", description: "top left entry", type: realT() }
    { name: "b", description: "middle left entry", type: realT() }
    { name: "c", description: "top center entry", type: realT() }
    { name: "d", description: "middle center entry", type: realT() }
    { name: "e", description: "top right entry", type: realT() }
    { name: "f", description: "middle right entry", type: realT() }
  ],

#### `matrix3d`

Specifies a transformation matrix `[ a1 a2 a3 a4; b1 b2 b3 b4; c1 c2 c3 c4; d1 d2 d3 d4 ]`, mirroring the CSS `matrix3d` transform function.

  params: [
    { name: "a1", description: "1st column of 1st row", type: realT() }
    { name: "b1", description: "1st column of 2nd row", type: realT() }
    { name: "c1", description: "1st column of 3rd row", type: realT() }
    { name: "d1", description: "1st column of 4th row", type: realT() }
    { name: "a2", description: "2nd column of 1st row", type: realT() }
    { name: "b2", description: "2nd column of 2nd row", type: realT() }
    { name: "c2", description: "2nd column of 3rd row", type: realT() }
    { name: "d2", description: "2nd column of 4th row", type: realT() }
    { name: "a3", description: "3rd column of 1st row", type: realT() }
    { name: "b3", description: "3rd column of 2nd row", type: realT() }
    { name: "c3", description: "3rd column of 3rd row", type: realT() }
    { name: "d3", description: "3rd column of 4th row", type: realT() }
    { name: "a4", description: "4th column of 1st row", type: realT() }
    { name: "b4", description: "4th column of 2nd row", type: realT() }
    { name: "c4", description: "4th column of 3rd row", type: realT() }
    { name: "d4", description: "4th column of 4th row", type: realT() }
  ],

## 2D and 3D spatial transformations

#### `rotate`

Returns a 2D rotation by an angle `theta` around the point (`x`,`y`).  If no point is specified, the rotation is around the origin.  (Note: this transformation is encoded as a 3x3 matrix in homogeneous coordinates, since in general it is an affine transformation.  For the 2x2 linear version, see `rotate2d()`.)

  params: [
    { name: "theta", description: "angle of rotation (in radians)", type: realT() }
    { name: "x", description: "center of rotation (x coordinate)", type: realT(), default: 0 }
    { name: "y", description: "center of rotation (y coordinate)", type: realT(), default: 0 }
  ],

#### `rotate2d`

Returns a 2D rotation around the origin by a given angle `theta`.  (Note: this transformation is encoded as a 2x2 matrix that cannot directly be composed with 2D affine transformations.  For the 3x3 affine version, see `rotate()`.)

  params: [
    { name: "theta", description: "angle of rotation (in radians)", type: realT() }
  ],

#### `rotate3d`

Returns a 3D rotation by a given angle `theta` around a unit axis `v`.  (Note: this transformation is encoded as a 3x3 matrix that cannot directly be composed with 3D affine transformations.  For the 4x4 affine version, see `rotate3dh()`.)

  params: [
    { name: "theta", description: "angle of rotation (in radians)", type: realT() }
    { name: "v", description: "axis of rotation (unit vector)", type: realNT() }
  ],

#### `rotate3dh`

Returns a 3D rotation by a given angle `theta` around a unit axis `v`.  (Note: this transformation is encoded as a 4x4 matrix in homogeneous coordinates, so that it can be composed with 3D affine transformations.  For the 3x3 linear version, see `rotate3d()`.)

  params: [
    { name: "theta", description: "angle of rotation (in radians)", type: realT() }
    { name: "v", description: "axis of rotation (unit vector)", type: realNT() }
  ],

#### `scale`

Returns a nonuniform scaling by factors `sx`, `sy` along `x`, `y` axes, respectively.  (Note: this transformation is encoded as a 3x3 matrix in homogeneous coordinates, so that it can be composed with 2D affine transformations.  For the 2x2 linear version, see `scale2d()`.)

  params: [
    { name: "sx", description: "horizontal scale factor", type: realT() }
    { name: "sy", description: "vertical scale factor", type: realT() }
  ],

#### `scale`

Returns a 2x2 matrix representing nonuniform scaling by factors `sx`, `sy` along `x`, `y` axes, respectively.  (Note: this transformation is encoded as a 2x2 matrix that cannot directly be composed with 2D affine transformations.  For the 3x3 affine version, see `rotate()`.)

  params: [
    { name: "sx", description: "horizontal scale factor", type: realT() }
    { name: "sy", description: "vertical scale factor", type: realT() }
  ],

#### `scale`

Returns a nonuniform scaling by factors `sx`, `sy`, `sz` along `x`, `y`, `z` axes, respectively.  (Note: this transformation is encoded as a 3x3 matrix that cannot directly be composed with 3D affine transformations.  For the 4x4 affine version, see `scale3dh()`.)

  params: [
    { name: "sx", description: "x scale factor", type: realT() }
    { name: "sy", description: "y scale factor", type: realT() }
    { name: "sz", description: "z scale factor", type: realT() }
  ],

#### `scale`

Returns a 4x4 matrix representing nonuniform scaling by factors `sx`, `sy`, `sz` along `x`, `y`, `z` axes, respectively.  (Note: this transformation is encoded as a 4x4 matrix in homogeneous coordinates, so that it can be composed with 3D affine transformations.  For the 3x3 linear version, see `scale3D()`.)

  params: [
    { name: "sx", description: "x scale factor", type: realT() }
    { name: "sy", description: "y scale factor", type: realT() }
    { name: "sz", description: "z scale factor", type: realT() }
  ],

#### `skew`

Given angles `ax` and `ay`, returns a transformation skewing an element on the 2D plane.  If `ay` is not defined, its default value is `0`, resulting in a purely horizontal skewing.  (Note: this transformation is encoded as a 3x3 matrix in homogeneous coordinates, so that it can be composed with affine transformations.  For the linear version, see `skew2d()`.)

  params: [
    { name: "ax", description: "horizontal angle", type: realT() }
    { name: "ay", description: "vertical angle", type: realT(), default: 0 }
  ],

#### `skew2d`

Given angles `ax` and `ay`, returns a transformation skewing an element on the 2D plane.  If `ay` is not defined, its default value is `0`, resulting in a purely horizontal skewing.  (Note: this transformation is encoded as a 2x2 matrix that cannot directly be composed with 2D affine transformations.  For the 3x3 affine version, see `skew()`.)

  params: [
    { name: "ax", description: "horizontal angle", type: realT() }
    { name: "ay", description: "vertical angle", type: realT(), default: 0 }
  ],

#### `shear`

Given `n`-dimensional vectors `u` and `v`, returns a shear transformation `A` such that `Ax` displaces any given point `x` in the direction `u` according to its extent along the direction `v`, i.e., `Ax = x + <v,x>u`.  (Note: this transformation is encoded as an (`n`+1)x(`n`+1) matrix in homogeneous coordinates, so that it can be composed with affine transformations.  For the linear version, see `shear2d()` or `shear3d()`.)

  params: [
    { name: "u", description: "offset direction", type: realNT() }
    { name: "v", description: "shear axis", type: realNT() }
  ],

#### `shear2d`

Given 2-dimensional vectors `u` and `v`, returns a shear transformation `A` such that `Ax` displaces any given point `x` in the direction `u` according to its extent along the direction `v`, i.e., `Ax = x + <v,x>u`.  (Note: this transformation is encoded as a 2x2 matrix that cannot directly be composed with 2-dimensional affine transformations.  For the affine version, see `shear()`.)

  params: [
    { name: "u", description: "offset direction", type: realNT() }
    { name: "v", description: "shear axis", type: realNT() }
  ],

#### `shear3d`

Given 3-dimensional vectors `u` and `v`, returns a shear transformation `A` such that `Ax` displaces any given point `x` in the direction `u` according to its extent along the direction `v`, i.e., `Ax = x + <v,x>u`.  (Note: this transformation is encoded as a 3x3 matrix that cannot directly be composed with 3-dimensional affine transformations.  For the affine version, see `shear()`.)

  params: [
    { name: "u", description: "offset direction", type: realNT() }
    { name: "v", description: "shear axis", type: realNT() }
  ],

#### `translate`

Returns a translation by the given offset `x`,`y`.  If `y` is not specified, it is assumed to be `0`. (Note: since this transformation is affine rather than linear, it is encoded as a 3x3 matrix in homogeneous coordinates.)

  params: [
    { name: "x", description: "horizontal offset", type: realT() }
    { name: "y", description: "vertical offset", type: realT(), default: 0 }
  ],

#### `translate3dh`

Returns a translation by the given offset (`x`,`y`,`z`).  (Note: since this transformation is affine rather than linear, it is encoded as a 4x4 matrix in homogeneous coordinates.)

  params: [
    { name: "x", description: "x offset", type: realT() }
    { name: "y", description: "y offset", type: realT() }
    { name: "z", description: "z offset", type: realT() }
  ],

#### `lookAt`

Returns a 4x4 viewing matrix derived from an eye point, a reference point indicating the center of the scene, and an up vector.  The matrix maps the reference point to the negative z axis and the eye point to the origin. When a typical projection matrix is used, the center of the scene therefore maps to the center of the viewport. Similarly, the direction described by the up vector projected onto the viewing plane is mapped to the positive y axis so that it points upward in the viewport. The up vector must not be parallel to the line of sight from the eye point to the reference point.

  params: [
    { name: "eye", description: "position of the eye point", type: realNT() }
    { name: "center", description: "position of the reference point", type: realNT() }
    { name: "up", description: "direction of the up vector", type: realNT() }
  ],

#### `perspective`

Returns a 4x4 perspective projection matrix.  The aspect ratio should match the aspect ratio of the associated viewport.  For example, aspect = 2.0 means the viewer's angle of view is twice as wide in x as it is in y.  If the viewport is twice as wide as it is tall, it displays the image without distortion.

  params: [
    { name: "fovy", description: "field of view angle, in degrees, in the y direction", type: realT() }
    { name: "aspect", description: "aspect ratio that determines the field of view in the x direction, equal to the ratio of x (width) to y (height)", type: realT() }
    { name: "zNear", description: "distance from the viewer to the near clipping plane (always positive), with a default value of 0.1", type: realT(), default: 0.1 }
    { name: "zFar", description: "distance from the viewer to the far clipping plane (always positive), with a default value of 100.0", type: realT(), default: 100.0 }
  ],

#### `ortho`

Returns a 4x4 transformation that produces a parallel projection.

  params: [
    { name: "Left", description: "coordinate of the left vertical clipping plane", type: realT() }
    { name: "Right", description: "coordinate of the right vertical clipping plane", type: realT() }
    { name: "Bottom", description: "coordinate of the bottom horizontal clipping plane", type: realT() }
    { name: "Top", description: "coordinate of the top horizontal clipping plane", type: realT() }
    { name: "zNear", description: "distance to the nearer depth clipping plane (negative if the plane is behind the viewer), with a default value of 0.1", type: realT(), default: 0.1 }
    { name: "zFar", description: "distance to the farther depth clipping plane (negative if the plane is behind the viewer), with a default value of 100", type: realT(), default: 100.0 }
  ],

#### `project`

Transforms the specified object coordinates into window coordinates using a given model and projection transformation, and a given viewport.  It returns the projected x,y coordinates.  To get the depth, see `projectDepth()`

  params: [
    { name: "p", description: "3D object coordinates (x,y,z)", type: realNT() }
    { name: "model", description: "4x4 modelview matrix", type: realNMT() }
    { name: "proj", description: "4x4 projection matrix", type: realNMT() }
    { name: "view", description: "viewport (x, y, width, height)", type: realNT() }
  ],

#### `projectDepth`

Transforms the specified object coordinates into window coordinates using a given model and projection transformation, and a given viewport.  It returns the projected x,y coordinates, as well as the depth relative to the view.

  params: [
    { name: "p", description: "3D object coordinates (x,y,z)", type: realNT() }
    { name: "model", description: "4x4 modelview matrix", type: realNMT() }
    { name: "proj", description: "4x4 projection matrix", type: realNMT() }
    { name: "view", description: "viewport (x, y, width, height)", type: realNT() }
  ],

#### `projectList`

Transforms the specified list of object coordinates into window coordinates using a given model and projection transformation, and a given viewport.  Returns the list of projected x,y coordinates.

  params: [
    { name: "p", description: "list of 3D object coordinates (x,y,z)", type: realNMT() }
    { name: "model", description: "4x4 modelview matrix", type: realNMT() }
    { name: "proj", description: "4x4 projection matrix", type: realNMT() }
    { name: "view", description: "viewport (x, y, width, height)", type: realNT() }
  ],

#### `matrixMultiplyList`

Multiplies each list element by the given matrix, returning the list of products.  List elements must have dimensions compatible with the matrix.

  params: [
    { name: "A", description: "`n`x`n` matrix", type: realNMT() }
    { name: "V", description: "list of `n`-dimensional vectors", type: realNMT() }
  ],

#### `fromHomogeneous`

Given a vector `q` of length `n`+1, encoding a point in `n`-dimensional homogeneous coordinates, returns a vector `p` of length `n`, encoding the same point in Cartesian coordinates.

  params: [
    { name: "q", description: "homogeneous coordinates", type: realNT() }
  ],

#### `fromHomogeneousList`

Given a list `Q` of vectors of length `n`+1, encoding points in `n`-dimensional homogeneous coordinates, returns a list `P` of vectors of length `n`, encoding the same points in Cartesian coordinates.

  params: [
    { name: "Q", description: "list of points in homogeneous coordinates", type: realNMT() }
  ],

#### `toHomogeneous`

Given a vector `p` of length `n`, encoding a point in `n`-dimensional Cartesian coordinates, returns a vector `q` of length `n`+1, encoding the same point in homogeneous coordinates.

  params: [
    { name: "p", description: "Cartesian coordinates", type: realNT() }
  ],

#### `toHomogeneousList`

Given a list `P` of vectors of length `n`, encoding points in `n`-dimensional Cartesian coordinates, returns a list `Q` of vectors of length `n`+1, encoding the same points in homogeneous coordinates.

  params: [
    { name: "P", description: "list of points in Cartesian coordinates", type: realNMT() }
  ],

#### `toHomogeneousMatrix`

Given a square `n` x `n` matrix `A` representing a spatial transformation in `n` dimensions, returns an (`n`+1) x (`n`+1) matrix representing the same transformation in homogeneous coordinates.

  params: [
    { name: "A", description: "matrix encoding linear transformation", type: realNMT() }
  ],





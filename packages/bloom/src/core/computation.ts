import {
  Num,
  Line as PenroseLine,
  Path as PenrosePath,
  Shape as PenroseShape,
  compDict,
} from "@penrose/core";
import {
  Color,
  Line,
  Path,
  PathData,
  Shape,
  Vec2,
  Vec3,
  Vec4,
  VecN,
} from "./types.js";
import { fromPenroseColor, toPenroseColor, toPenroseShape } from "./utils.js";

const context = {
  makeInput: () => {
    throw new Error("Dummy context was called");
  },
};

/**
 * See https://github.com/penrose/penrose/issues/716
 * @param start Start point of the path
 * @param end End point of the path
 * @param curveHeight Height of the curve
 * @param padding Padding between the curve and the labels
 * @returns Path data
 */
export const makePath = (
  start: Vec2,
  end: Vec2,
  curveHeight: Num,
  padding: Num,
): PathData =>
  compDict.makePath.body(context, start, end, curveHeight, padding).value
    .contents;

/**
 * Return `i`th element of list `xs`, assuming lists only hold floats.
 * @param xs List of floats
 * @param i Index of the element to return
 * @returns The i-th element of xs
 */
export const get = (xs: VecN, i: number): Num =>
  compDict.get.body(context, xs, i).value.contents;

/**
 * Return a paint color of elements `r`, `g`, `b`, `a` (red, green, blue, opacity).
 * @param r Red
 * @param g Green
 * @param b Blue
 * @param a Opacity
 * @returns RGBA color
 */
export const rgba = (r: Num, g: Num, b: Num, a: Num): Color =>
  fromPenroseColor(compDict.rgba.body(context, r, g, b, a).value);

/**
 * Select a color based on a level
 * @param color1 First color
 * @param color2 Second color
 * @param level Level
 * @returns Selected color
 */
export const selectColor = (color1: Color, color2: Color, level: Num): Color =>
  fromPenroseColor(
    compDict.selectColor.body(
      context,
      toPenroseColor(color1),
      toPenroseColor(color2),
      level,
    ).value,
  );

/**
 * Return a paint color of elements `h`, `s`, `v`, `a` (hue, saturation, value, opacity).
 * @param h Hue in [0, 360)
 * @param s Saturation in [0, 100]
 * @param v Value in [0, 100]
 * @param a Opacity
 * @returns HSVA color
 */
export const hsva = (h: Num, s: Num, v: Num, a: Num): Color =>
  fromPenroseColor(compDict.hsva.body(context, h, s, v, a).value);

/**
 * Return a paint of none (no paint)
 * @returns None color
 */
export const none = (): Color =>
  fromPenroseColor(compDict.none.body(context).value);

/**
 * Index a point list using 1-based indexing.
 * @param points List of points
 * @param i 1-based index
 * @returns The i-th point
 */
export const oneBasedElement = (points: Vec2[], i: number): Vec2 =>
  compDict.oneBasedElement.body(context, points, i).value.contents as Vec2;

/**
 * Return `acosh(x)`.
 * @param x Input value
 * @returns acosh(x)
 */
export const acosh = (x: Num): Num =>
  compDict.acosh.body(context, x).value.contents;

/**
 * Return `acos(x)`.
 * @param x Input value
 * @returns acos(x)
 */
export const acos = (x: Num): Num =>
  compDict.acos.body(context, x).value.contents;

/**
 * Return `asin(x)`.
 * @param x Input value
 * @returns asin(x)
 */
export const asin = (x: Num): Num =>
  compDict.asin.body(context, x).value.contents;

/**
 * Return `asinh(x)`.
 * @param x Input value
 * @returns asinh(x)
 */
export const asinh = (x: Num): Num =>
  compDict.asinh.body(context, x).value.contents;

/**
 * Return `mod(a, n)`.
 * @param a Dividend
 * @param n Divisor
 * @returns a mod n
 */
export const mod = (a: Num, n: Num): Num =>
  compDict.mod.body(context, a, n).value.contents;

/**
 * Return `atan(x)`.
 * @param x Input value
 * @returns atan(x)
 */
export const atan = (x: Num): Num =>
  compDict.atan.body(context, x).value.contents;

/**
 * Return `atan2(y, x)`.
 * @param x x-coordinate
 * @param y y-coordinate
 * @returns atan2(y, x)
 */
export const atan2 = (x: Num, y: Num): Num =>
  compDict.atan2.body(context, x, y).value.contents;

/**
 * Return `atanh(x)`.
 * @param x Input value
 * @returns atanh(x)
 */
export const atanh = (x: Num): Num =>
  compDict.atanh.body(context, x).value.contents;

/**
 * Return `cbrt(x)`.
 * @param x Input value
 * @returns cbrt(x)
 */
export const cbrt = (x: Num): Num =>
  compDict.cbrt.body(context, x).value.contents;

/**
 * Return `ceil(x)`.
 * @param x Input value
 * @returns ceil(x)
 */
export const ceil = (x: Num): Num =>
  compDict.ceil.body(context, x).value.contents;

/**
 * Return `cos(x)`.
 * @param x Input value
 * @returns cos(x)
 */
export const cos = (x: Num): Num =>
  compDict.cos.body(context, x).value.contents;

/**
 * Return `cosh(x)`.
 * @param x Input value
 * @returns cosh(x)
 */
export const cosh = (x: Num): Num =>
  compDict.cosh.body(context, x).value.contents;

/**
 * Return `exp(x)`.
 * @param x Input value
 * @returns exp(x)
 */
export const exp = (x: Num): Num =>
  compDict.exp.body(context, x).value.contents;

/**
 * Return `expm1(x)`.
 * @param x Input value
 * @returns expm1(x)
 */
export const expm1 = (x: Num): Num =>
  compDict.expm1.body(context, x).value.contents;

/**
 * Return `floor(x)`.
 * @param x Input value
 * @returns floor(x)
 */
export const floor = (x: Num): Num =>
  compDict.floor.body(context, x).value.contents;

/**
 * Return `log(x)`.
 * @param x Input value
 * @returns log(x)
 */
export const log = (x: Num): Num =>
  compDict.log.body(context, x).value.contents;

/**
 * Return `log2(x)`.
 * @param x Input value
 * @returns log2(x)
 */
export const log2 = (x: Num): Num =>
  compDict.log2.body(context, x).value.contents;

/**
 * Return `log10(x)`.
 * @param x Input value
 * @returns log10(x)
 */
export const log10 = (x: Num): Num =>
  compDict.log10.body(context, x).value.contents;

/**
 * Return `log1p(x)`.
 * @param x Input value
 * @returns log1p(x)
 */
export const log1p = (x: Num): Num =>
  compDict.log1p.body(context, x).value.contents;

/**
 * Return `pow(x,y)`.
 * @param x Base
 * @param y Exponent
 * @returns pow(x,y)
 */
export const pow = (x: Num, y: Num): Num =>
  compDict.pow.body(context, x, y).value.contents;

/**
 * Return `round(x)`.
 * @param x Input value
 * @returns round(x)
 */
export const round = (x: Num): Num =>
  compDict.round.body(context, x).value.contents;

/**
 * Return `sign(x)`.
 * @param x Input value
 * @returns sign(x)
 */
export const sign = (x: Num): Num =>
  compDict.sign.body(context, x).value.contents;

/**
 * Return `sin(x)`.
 * @param x Input value
 * @returns sin(x)
 */
export const sin = (x: Num): Num =>
  compDict.sin.body(context, x).value.contents;

/**
 * Return `sinh(x)`.
 * @param x Input value
 * @returns sinh(x)
 */
export const sinh = (x: Num): Num =>
  compDict.sinh.body(context, x).value.contents;

/**
 * Return `tan(x)`.
 * @param x Input value
 * @returns tan(x)
 */
export const tan = (x: Num): Num =>
  compDict.tan.body(context, x).value.contents;

/**
 * Return `tanh(x)`.
 * @param x Input value
 * @returns tanh(x)
 */
export const tanh = (x: Num): Num =>
  compDict.tanh.body(context, x).value.contents;

/**
 * Return `trunc(x)`.
 * @param x Input value
 * @returns trunc(x)
 */
export const trunc = (x: Num): Num =>
  compDict.trunc.body(context, x).value.contents;

/**
 * Return the sum of elements in a vector.
 * @param xs Elements
 * @returns Sum of elements
 */
export const sum = (xs: VecN): Num =>
  compDict.sum.body(context, xs).value.contents;

/**
 * Return the sum of vectors in a list of vectors.
 * @param vecs Vectors
 * @returns Sum of vectors
 */
export const sumVectors = (vecs: Vec2[]): Vec2 =>
  compDict.sumVectors.body(context, vecs).value.contents as Vec2;

/**
 * Return the maximum of the elements in a vector.
 * @param xs Elements
 * @returns Maximum element
 */
export const maxList = (xs: VecN): Num =>
  compDict.maxList.body(context, xs).value.contents;

/**
 * Return the minimum of the elements in a vector.
 * @param xs Elements
 * @returns Minimum element
 */
export const minList = (xs: VecN): Num =>
  compDict.minList.body(context, xs).value.contents;

/**
 * Return the number of the elements in a vector.
 * @param xs Elements
 * @returns Number of elements
 */
export const count = (xs: VecN): Num =>
  compDict.count.body(context, xs).value.contents;

/**
 * Return the dot product of `v` and `w`.
 * @param v Vector `v`
 * @param w Vector `w`
 * @returns Dot product of v and w
 */
export const dot = (v: VecN, w: VecN): Num =>
  compDict.dot.body(context, v, w).value.contents;

/**
 * `identity(n)` returns the $n \times n$ identity matrix
 * $$I = \left[ \begin{array}{cccc} 1 & 0 & \cdots & 0 \\ 0 & 1 & \cdots & 0 \\ \vdots & \vdots & \ddots & \vdots \\ 0 & 0 & \cdots & 1 \end{array} \right].$$
 * @param n dimension
 * @returns Identity matrix
 */
export const identity = (n: number): Num[][] =>
  compDict.identity.body(context, n).value.contents;

/**
 * `diagonal(v)` takes a vector $v$ of length $n$, and returns the $n \times n$ diagonal matrix
 * $$D = \left[ \begin{array}{cccc} v_1 & 0 & \cdots & 0 \\ 0 & v_2 & \cdots & 0 \\ \vdots & \vdots & \ddots & \vdots \\ 0 & 0 & \cdots & v_n \end{array} \right].$$
 * @param v vector of diagonal entries
 * @returns Diagonal matrix
 */
export const diagonal = (v: VecN): Num[][] =>
  compDict.diagonal.body(context, v).value.contents;

/**
 * `trace(A)` takes a square matrix $A$, and returns the trace $\text{tr}(A)$, equal to the sum of its diagonal entries.
 * @param A a square matrix
 * @returns Trace of A
 */
export const trace = (A: Num[][]): Num =>
  compDict.trace.body(context, A).value.contents;

/**
 * `determinant(A)` takes a $2 \times 2$, $3 \times 3$, or $4 \times 4$ matrix $A$, and returns its determinant $\text{det}(A)$.
 * @param A a square matrix
 * @returns Determinant of A
 */
export const determinant = (A: Num[][]): Num =>
  compDict.determinant.body(context, A).value.contents;

/**
 * `inverse(A)` takes a $2 \times 2$, $3 \times 3$, or $4 \times 4$ matrix $A$, and returns its inverse $A^{-1}$.  If the matrix is not invertible, the result may be numerically invalid (with `INF` or `NaN` entries).
 * @param A a 2x2, 3x3, or 4x4 matrix
 * @returns Inverse of A
 */
export const inverse = (A: Num[][]): Num[][] =>
  compDict.inverse.body(context, A).value.contents;

/**
 * `outerProduct(u,v)` takes two vectors $u$, $v$ of equal length $n$, and returns the $n \times n$ outer product matrix $A$, with entries $A_{ij} = u_i v_j$.
 * @param v Vector `v`
 * @param w Vector `w`
 * @returns Outer product matrix
 */
export const outerProduct = (v: VecN, w: VecN): Num[][] =>
  compDict.outerProduct.body(context, v, w).value.contents;

/**
 * `crossProductMatrix(v)` takes a 3-vector $v$, and returns a $3 \times 3$ skew symmetric matrix $A^T = -A$ such that $Au = v \times u$ for any vector $u$.
 * @param v Vector `v`
 * @returns Cross product matrix
 */
export const crossProductMatrix = (v: VecN): Num[][] =>
  compDict.crossProductMatrix.body(context, v).value.contents;

/**
 * `matrix(a,b,c,d,e,f)` specifies a transformation matrix
 * $$\left[ \begin{array}{ccc} a & c & e \\ b & d & f \\ 0 & 0 & 1 \end{array} \right].$$
 * This function mirrors the SVG/CSS `matrix` transform function.
 * @param a top left entry
 * @param b middle left entry
 * @param c top center entry
 * @param d middle center entry
 * @param e top right entry
 * @param f middle right entry
 * @returns Transformation matrix
 */
export const matrix = (
  a: Num,
  b: Num,
  c: Num,
  d: Num,
  e: Num,
  f: Num,
): Num[][] => compDict.matrix.body(context, a, b, c, d, e, f).value.contents;

/**
 * `matrix3d(a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4)` specifies a transformation matrix
 * $$\left[ \begin{array}{cccc} a1 & a2 & a3 & a4 \\ b1 & b2 & b3 & b4 \\ c1 & c2 & c3 & c4 \\ d1 & d2 & d3 & d4 \end{array} \right].$$
 * This function mirrors the CSS `matrix3d` transform function.
 * @param a1 1st column of 1st row
 * @param b1 1st column of 2nd row
 * @param c1 1st column of 3rd row
 * @param d1 1st column of 4th row
 * @param a2 2nd column of 1st row
 * @param b2 2nd column of 2nd row
 * @param c2 2nd column of 3rd row
 * @param d2 2nd column of 4th row
 * @param a3 3rd column of 1st row
 * @param b3 3rd column of 2nd row
 * @param c3 3rd column of 3rd row
 * @param d3 3rd column of 4th row
 * @param a4 4th column of 1st row
 * @param b4 4th column of 2nd row
 * @param c4 4th column of 3rd row
 * @param d4 4th column of 4th row
 * @returns 3D transformation matrix
 */
export const matrix3d = (
  a1: Num,
  b1: Num,
  c1: Num,
  d1: Num,
  a2: Num,
  b2: Num,
  c2: Num,
  d2: Num,
  a3: Num,
  b3: Num,
  c3: Num,
  d3: Num,
  a4: Num,
  b4: Num,
  c4: Num,
  d4: Num,
): Num[][] =>
  compDict.matrix3d.body(
    context,
    a1,
    b1,
    c1,
    d1,
    a2,
    b2,
    c2,
    d2,
    a3,
    b3,
    c3,
    d3,
    a4,
    b4,
    c4,
    d4,
  ).value.contents;

/**
 * `rotate(theta, [x], [y])` returns a counter-clockwise 2D rotation by an angle $\theta$, optionally around the point $(x,y)$.  If no point is specified, the rotation is around the origin.
 * @param theta angle of rotation (in radians)
 * @param x center of rotation (x coordinate)
 * @param y center of rotation (y coordinate)
 * @returns Rotation matrix
 */
export const rotate = (theta: Num, x: Num = 0, y: Num = 0): Num[][] =>
  compDict.rotate.body(context, theta, x, y).value.contents;

/**
 * `rotate2d(theta)` returns a 2D rotation around the origin by a given angle $\theta$, encoded via the $2 \times 2$ matrix.
 * @param theta angle of rotation (in radians)
 * @returns 2D rotation matrix
 */
export const rotate2d = (theta: Num): Num[][] =>
  compDict.rotate2d.body(context, theta).value.contents;

/**
 * `rotate3d(theta, v)` returns a 3D rotation by an angle $\theta$ around a unit axis $v$.
 * @param theta angle of rotation (in radians)
 * @param v axis of rotation (unit vector)
 * @returns 3D rotation matrix
 */
export const rotate3d = (theta: Num, v: VecN): Num[][] =>
  compDict.rotate3d.body(context, theta, v).value.contents;

/**
 * `rotate3dh(theta, v)` returns a 3D rotation by a given angle $\theta$ around a unit axis $v$.  This transformation is encoded as a $4 \times 4$ matrix in homogeneous coordinates.
 * @param theta angle of rotation (in radians)
 * @param v axis of rotation (unit vector)
 * @returns 3D rotation matrix in homogeneous coordinates
 */
export const rotate3dh = (theta: Num, v: VecN): Num[][] =>
  compDict.rotate3dh.body(context, theta, v).value.contents;

/**
 * `scale(sx, sy)` returns a nonuniform scaling by factors $s_x$, $s_y$ along $x$, $y$ axes.
 * @param sx horizontal scale factor
 * @param sy vertical scale factor
 * @returns Scaling matrix
 */
export const scale = (sx: Num, sy: Num): Num[][] =>
  compDict.scale.body(context, sx, sy).value.contents;

/**
 * `scale2d(sx,sy)` returns a $2 \times 2$ matrix representing nonuniform scaling by factors $s_x$, $s_y$ along $x$, $y$ axes.
 * @param sx horizontal scale factor
 * @param sy vertical scale factor
 * @returns 2D scaling matrix
 */
export const scale2d = (sx: Num, sy: Num): Num[][] =>
  compDict.scale2d.body(context, sx, sy).value.contents;

/**
 * `scale3d(sx, sy, sz)` returns a nonuniform scaling by factors $s_x$, $s_y$, $s_z$ along $x$, $y$, $z$ axes.
 * @param sx x scale factor
 * @param sy y scale factor
 * @param sz z scale factor
 * @returns 3D scaling matrix
 */
export const scale3d = (sx: Num, sy: Num, sz: Num): Num[][] =>
  compDict.scale3d.body(context, sx, sy, sz).value.contents;

/**
 * `scale3dh(sx, sy, sz)` returns a $4 \times 4$ matrix representing nonuniform scaling by factors $s_x$, $s_y$, $s_z$ along $x$, $y$, $z$ axes.
 * @param sx x scale factor
 * @param sy y scale factor
 * @param sz z scale factor
 * @returns 3D scaling matrix in homogeneous coordinates
 */
export const scale3dh = (sx: Num, sy: Num, sz: Num): Num[][] =>
  compDict.scale3dh.body(context, sx, sy, sz).value.contents;

/**
 * `skew(a_x, a_y)` takes angles $a_x$ and $a_y$, and returns a 2D skew transformation.
 * @param ax horizontal angle
 * @param ay vertical angle
 * @returns Skew matrix
 */
export const skew = (ax: Num, ay: Num = 0): Num[][] =>
  compDict.skew.body(context, ax, ay).value.contents;

/**
 * `skew2d(a_x, a_y)` takes angles $a_x$ and $a_y$, and returns a 2D skew transformation encoded via a $2 \times 2$ matrix.
 * @param ax horizontal angle
 * @param ay vertical angle
 * @returns 2D skew matrix
 */
export const skew2d = (ax: Num, ay: Num = 0): Num[][] =>
  compDict.skew2d.body(context, ax, ay).value.contents;

/**
 * `shear(u,v)` takes two $n$-dimensional vectors $u$ and $v$, and returns a transformation that shears any given point $x$ in the direction $u$ according to its extent along the direction $v$.
 * @param u offset direction
 * @param v shear axis
 * @returns Shear matrix
 */
export const shear = (u: VecN, v: VecN): Num[][] =>
  compDict.shear.body(context, u, v).value.contents;

/**
 * `shear2d(u,v)` takes two 2-dimensional vectors $u$ and $v$, and returns a transformation that shears any given point $x$ in the direction $u$ according to its extent along the direction $v$.
 * @param u offset direction
 * @param v shear axis
 * @returns 2D shear matrix
 */
export const shear2d = (u: Vec2, v: Vec2): Num[][] =>
  compDict.shear2d.body(context, u, v).value.contents;

/**
 * `shear3d(u,v)` takes two 3-dimensional vectors $u$ and $v$, returns a transformation that shears any given point $x$ in the direction $u$ according to its extent along the direction $v$.
 * @param u offset direction
 * @param v shear axis
 * @returns 3D shear matrix
 */
export const shear3d = (u: Vec3, v: Vec3): Num[][] =>
  compDict.shear3d.body(context, u, v).value.contents;

/**
 * `translate(x,y)` returns a translation by the given offset $(x,y)$.
 * @param x horizontal offset
 * @param y vertical offset
 * @returns Translation matrix
 */
export const translate = (x: Num, y: Num = 0): Num[][] =>
  compDict.translate.body(context, x, y).value.contents;

/**
 * `translate3dh(x,y,z)` returns a translation by $(x,y,z)$.
 * @param x x offset
 * @param y y offset
 * @param z z offset
 * @returns 3D translation matrix
 */
export const translate3dh = (x: Num, y: Num, z: Num): Num[][] =>
  compDict.translate3dh.body(context, x, y, z).value.contents;

/**
 * `lookAt(eye, center, up)` returns a $4 \times 4$ viewing matrix derived from an eye point $e$, a reference point $c$ indicating the center of the scene, and an up vector $u$.
 * @param eye position of the eye point
 * @param center position of the reference point
 * @param up unit vector in the upward direction
 * @returns Look-at matrix
 */
export const lookAt = (eye: VecN, center: VecN, up: VecN): Num[][] =>
  compDict.lookAt.body(context, eye, center, up).value.contents;

/**
 * `perspective(fovy, aspect, [zNear], [zFar])` returns a $4 \times 4$ perspective projection matrix.
 * @param fovy field of view angle, in degrees, in the y direction
 * @param aspect aspect ratio that determines the field of view in the x direction
 * @param zNear distance from the viewer to the near clipping plane (always positive)
 * @param zFar distance from the viewer to the far clipping plane (always positive)
 * @returns Perspective projection matrix
 */
export const perspective = (
  fovy: Num,
  aspect: Num,
  zNear: Num = 0.1,
  zFar: Num = 100.0,
): Num[][] =>
  compDict.perspective.body(context, fovy, aspect, zNear, zFar).value.contents;

/**
 * `ortho(left, right, bottom, top, [zNear], [zFar])` returns a $4 \times 4$ transformation that produces a parallel projection.
 * @param Left coordinate of the left vertical clipping plane
 * @param Right coordinate of the right vertical clipping plane
 * @param Bottom coordinate of the bottom horizontal clipping plane
 * @param Top coordinate of the top horizontal clipping plane
 * @param zNear distance to the nearer depth clipping plane
 * @param zFar distance to the farther depth clipping plane
 * @returns Orthographic projection matrix
 */
export const ortho = (
  Left: Num,
  Right: Num,
  Bottom: Num,
  Top: Num,
  zNear: Num = 0.1,
  zFar: Num = 100.0,
): Num[][] =>
  compDict.ortho.body(context, Left, Right, Bottom, Top, zNear, zFar).value
    .contents;

/**
 * `project(p, model, proj, view)` transforms the specified 3D object coordinates $p$ into 2D window coordinates $q$.
 * @param p 3D object coordinates (x,y,z)
 * @param model 4x4 modelview matrix
 * @param proj 4x4 projection matrix
 * @param view viewport (x, y, width, height)
 * @returns Projected 2D coordinates
 */
export const project = (
  p: Vec3,
  model: Num[][],
  proj: Num[][],
  view: Vec4,
): Vec2 =>
  compDict.project.body(context, p, model, proj, view).value.contents as Vec2;

/**
 * `projectDepth(p, model, proj, view)` transforms the specified 3D object coordinates $p$ into 2D window coordinates $q$ and returns the depth.
 * @param p 3D object coordinates (x,y,z)
 * @param model 4x4 modelview matrix
 * @param proj 4x4 projection matrix
 * @param view viewport (x, y, width, height)
 * @returns Projected 2D coordinates and depth
 */
export const projectDepth = (
  p: Vec3,
  model: Num[][],
  proj: Num[][],
  view: Vec4,
): Vec3 =>
  compDict.projectDepth.body(context, p, model, proj, view).value
    .contents as Vec3;

/**
 * `projectList(P, model, proj, view)` transforms a list of 3D object coordinates $P = p_1, \ldots, p_k$ into window 2D coordinates.
 * @param p list of 3D object coordinates (x,y,z)
 * @param model 4x4 modelview matrix
 * @param proj 4x4 projection matrix
 * @param view viewport (x, y, width, height)
 * @returns List of projected 2D coordinates
 */
export const projectList = (
  p: Vec3[],
  model: Num[][],
  proj: Num[][],
  view: Vec4,
): Vec2[] =>
  compDict.projectList.body(context, p, model, proj, view).value
    .contents as Vec2[];

/**
 * `matrixMultiplyList(A, V)` multiplies each vector $v_1, \ldots, v_k$ in the list $V$ by the given $n \times n$ matrix $A$.
 * @param A `n`x`n` matrix
 * @param V list of `n`-dimensional vectors
 * @returns List of matrix-vector products
 */
export const matrixMultiplyList = (A: Num[][], V: VecN[]): VecN[] =>
  compDict.matrixMultiplyList.body(context, A, V).value.contents;

/**
 * `fromHomogeneous(q)` takes a vector $q$ of length $n+1$, encoding a point in $n$-dimensional homogeneous coordinates, and returns a vector $p$ of length $n$, encoding the same point in Cartesian coordinates.
 * @param q homogeneous coordinates
 * @returns Cartesian coordinates
 */
export const fromHomogeneous = (q: VecN): VecN =>
  compDict.fromHomogeneous.body(context, q).value.contents;

/**
 * `fromHomogeneousList(Q)` takes a list $Q = q_1, \ldots, q_k$ of vectors of length $n+1$, encoding points in $n$-dimensional homogeneous coordinates, and returns a list $P = p_1, \ldots, p_k$ of vectors of length $n$, encoding the same points in Cartesian coordinates.
 * @param Q list of points in homogeneous coordinates
 * @returns List of points in Cartesian coordinates
 */
export const fromHomogeneousList = (Q: VecN[]): VecN[] =>
  compDict.fromHomogeneousList.body(context, Q).value.contents;

/**
 * `toHomogeneous(p)` takes a vector $p$ of length $n$, encoding a point in $n$-dimensional Cartesian coordinates, and returns a vector $q$ of length $n+1$, encoding the same point in homogeneous coordinates.
 * @param p Cartesian coordinates
 * @returns Homogeneous coordinates
 */
export const toHomogeneous = (p: VecN): VecN =>
  compDict.toHomogeneous.body(context, p).value.contents;

/**
 * `toHomogeneousList(P)` takes a list $P = p_1, \ldots, p_k$ of vectors of length $n$, encoding points in $n$-dimensional Cartesian coordinates, and returns a list $Q = q_1, \ldots, q_k$ of vectors of length $n+1$, encoding the same points in homogeneous coordinates.
 * @param P list of points in Cartesian coordinates
 * @returns List of points in homogeneous coordinates
 */
export const toHomogeneousList = (P: VecN[]): VecN[] =>
  compDict.toHomogeneousList.body(context, P).value.contents;

/**
 * `toHomogeneousMatrix(A)` takes a square $n \times n$ matrix $A$ representing a spatial transformation in $A$ dimensions, and returns an $(n+1) \times (n+1)$ matrix representing the same transformation in homogeneous coordinates.
 * @param A matrix encoding linear transformation
 * @returns Homogeneous transformation matrix
 */
export const toHomogeneousMatrix = (A: Num[][]): Num[][] =>
  compDict.toHomogeneousMatrix.body(context, A).value.contents;

/**
 * Return the length of the Line shape.
 * @param l A line
 * @returns Length of the line
 */
export const length = (l: Line): Num =>
  compDict.length.body(context, toPenroseShape(l) as PenroseLine<Num>).value
    .contents;

/**
 * Return the normalized version of vector `v`.
 * @param v Vector `v`
 * @returns Normalized vector
 */
export const normalize = (v: VecN): VecN =>
  compDict.normalize.body(context, v).value.contents;

/**
 * Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.
 * @param pathType Path Type
 * @param pts List of points
 * @returns Path data
 */
export const pathFromPoints = (pathType: string, pts: Vec2[]): PathData =>
  compDict.pathFromPoints.body(context, pathType, pts).value.contents;

/**
 * Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.
 * @param pathType Path Type
 * @param pts List of points
 * @returns Path data for a quadratic curve
 */
export const quadraticCurveFromPoints = (
  pathType: string,
  pts: Vec2[],
): PathData =>
  compDict.quadraticCurveFromPoints.body(context, pathType, pts).value.contents;

/**
 * Draw a curve interpolating three given points.
 * (Note that this is different from specifying the three control points of a quadratic Bézier curve, since a Bézier does not interpolate the middle control point.)
 * @param pathType Path Type
 * @param p0 First point
 * @param p1 Second point
 * @param p2 Third point
 * @returns Path data for an interpolating quadratic curve
 */
export const interpolateQuadraticFromPoints = (
  pathType: string,
  p0: Vec2,
  p1: Vec2,
  p2: Vec2,
): PathData =>
  compDict.interpolateQuadraticFromPoints.body(context, pathType, p0, p1, p2)
    .value.contents;

/**
 * Given a list of points `pts`, returns a `PathData` that can be used as input to the `Path` shape's `pathData` attribute to be drawn on the screen.
 * @param pathType Path type
 * @param pts List of points
 * @returns Path data for a cubic curve
 */
export const cubicCurveFromPoints = (pathType: string, pts: Vec2[]): PathData =>
  compDict.cubicCurveFromPoints.body(context, pathType, pts).value.contents;

/**
 * Given a list of `PathData`s, returns a `PathData` representing the union of these paths with lines connecting the start and end points.
 * @param pathType Path type
 * @param pathDataList List of path data
 * @returns Connected path data
 */
export const connectPaths = (
  pathType: string,
  pathDataList: PathData[],
): PathData =>
  compDict.connectPaths.body(context, pathType, pathDataList).value.contents;

/**
 * Given a list of `PathData`s, return the union of these paths.
 * @param pathDataList List of path data
 * @returns Concatenated path data
 */
export const concatenatePaths = (pathDataList: PathData[]): PathData =>
  compDict.concatenatePaths.body(context, pathDataList).value.contents;

/**
 * Given a list of `PathData`s, join them into one SVG path. For correct results, the end points and start points of each path must already coincide.
 * @param pathDataList List of path data
 * @returns Joined path data
 */
export const joinPaths = (pathDataList: PathData[]): PathData =>
  compDict.joinPaths.body(context, pathDataList).value.contents;

/**
 * Return path data describing an "impossible polygon."
 * @param center (x,y) translation
 * @param radius radius of outer polygon (must be positive)
 * @param holeSize radius of inner polygon as a fraction of the outer radius (in range (0,1])
 * @param angle angle of rotation
 * @param nSides number of sides (integer ≥ 3)
 * @param chirality either "cw" for clockwise, or "ccw" for counterclockwise
 * @returns Path data for a Penrose polygon
 */
export const Penrose = (
  center: Vec2 = [0, 0],
  radius: Num = 50,
  holeSize: Num = 0.35,
  angle: Num = 0,
  nSides: number = 5,
  chirality: string = "ccw",
): PathData =>
  compDict.Penrose.body(
    context,
    center,
    radius,
    holeSize,
    angle,
    nSides,
    chirality,
  ).value.contents;

/**
 * Returns the first point in a list.
 * @param points list of points
 * @returns First point in the list
 */
export const firstPoint = (points: VecN[]): VecN =>
  compDict.firstPoint.body(context, points).value.contents;

/**
 * Returns the last point in a list.
 * @param points list of points
 * @returns Last point in the list
 */
export const lastPoint = (points: VecN[]): VecN =>
  compDict.lastPoint.body(context, points).value.contents;

/**
 * Returns the average (mean) of all points in a list.
 * @param points list of points
 * @returns Average point
 */
export const averagePoint = (points: VecN[]): VecN =>
  compDict.averagePoint.body(context, points).value.contents;

/**
 * Returns path data for a curve that smoothly interpolates the given points. Interpolation is performed via a Catmull-Rom spline.
 * @param pathType either "open" or "closed."
 * @param points points to be interpolated
 * @param tension smoothness of curve (0=piecewise linear, .25=default)
 * @returns Path data for interpolating spline
 */
export const interpolatingSpline = (
  pathType: string,
  points: VecN[],
  tension: Num = 0.25,
): PathData =>
  compDict.interpolatingSpline.body(context, pathType, points, tension).value
    .contents;

/**
 * Return `n` points sampled from a diffusion process starting at `X0`, with covariance matrix `A` and constant drift `omega`.
 * This path approximately integrates the stochastic differential equation dX_t = omega dt + A dW_t, where W_t is a Wiener process.
 * @param n number of points
 * @param X0 starting location
 * @param A covariance matrix
 * @param omega drift direction
 * @returns List of points from the diffusion process
 */
export const diffusionProcess = (
  n: number,
  X0: Vec2,
  A: Vec2[],
  omega: Vec2,
): Vec2[] =>
  compDict.diffusionProcess.body(context, n, X0, A, omega).value
    .contents as Vec2[];

/**
 * Return two points parallel to line `s1` using its normal line `s2`.
 * @param s1 First line
 * @param s2 Second line (normal to s1)
 * @param padding Padding distance
 * @returns Two points for unit mark
 */
export const unitMark = (s1: Line, s2: Line, padding: Num): Vec2[] =>
  compDict.unitMark.body(
    context,
    toPenroseShape(s1) as PenroseLine<Num>,
    toPenroseShape(s2) as PenroseLine<Num>,
    padding,
  ).value.contents as Vec2[];

/**
 * Return two points to "cap off" the line made in `unitMark`.
 * @param [start, end] Start and end points of the line
 * @param t Either "start" or "end"
 * @param size Size of the cap
 * @returns Two points for unit mark cap
 */
export const unitMark2 = (
  [start, end]: [Vec2, Vec2],
  t: string,
  size: Num,
): Vec2[] =>
  compDict.unitMark2.body(context, [start, end], t, size).value
    .contents as Vec2[];

/**
 * Return series of elements that can render an arc SVG.
 * @param pathType either "open" or "closed."
 * @param start coordinate to start drawing the arc
 * @param end coordinate to finish drawing the arc
 * @param [width, height] width and height of the ellipse to draw the arc along
 * @param rotation angle in degrees to rotate ellipse about its center
 * @param largeArc 0 to draw shorter of 2 arcs, 1 to draw longer
 * @param arcSweep 0 to rotate CCW, 1 to rotate CW
 * @returns Path data for an arc
 */
export const arc = (
  pathType: string,
  start: Vec2,
  end: Vec2,
  [width, height]: Vec2,
  rotation: Num,
  largeArc: Num,
  arcSweep: Num,
): PathData =>
  compDict.arc.body(
    context,
    pathType,
    start,
    end,
    [width, height],
    rotation,
    largeArc,
    arcSweep,
  ).value.contents;

/**
 * Return path data that describes a circular arc.
 * @param pathType either "open" or "closed."
 * @param center circle center
 * @param r circle radius
 * @param theta0 start angle in radians
 * @param theta1 end angle in radians
 * @returns Path data for a circular arc
 */
export const circularArc = (
  pathType: string,
  center: Vec2,
  r: Num,
  theta0: Num,
  theta1: Num,
): PathData =>
  compDict.circularArc.body(context, pathType, center, r, theta0, theta1).value
    .contents;

/**
 * Generate multiple concentric arcs. Useful for denoting equal angles.
 * @param innerStart coordinate to start drawing the inner arc
 * @param innerEnd coordinate to end the inner arc
 * @param outerStart coordinate to start drawing the outer arc
 * @param outerEnd coordinate to end the outer arc
 * @param innerRadius radii of the ellipse to draw the inner arc along (width, height)
 * @param repeat number of times to repeat the arc
 * @param spacing spacing between arcs
 * @param arcSweep arc length to sweep
 * @returns Path data for repeated arcs
 */
export const repeatedArcs = (
  innerStart: Vec2,
  innerEnd: Vec2,
  outerStart: Vec2,
  outerEnd: Vec2,
  innerRadius: Vec2,
  repeat: number,
  spacing: Num,
  arcSweep: Num,
): PathData =>
  compDict.repeatedArcs.body(
    context,
    innerStart,
    innerEnd,
    outerStart,
    outerEnd,
    innerRadius,
    repeat,
    spacing,
    arcSweep,
  ).value.contents;

/**
 * Return series of elements that render a "wedge", which is the same as the arc above except that it's connected to the circle center and filled.
 * @param center center of the circle on which the arc sits
 * @param start coordinate to start drawing the arc
 * @param end coordinate to finish drawing the arc
 * @param radius width and height of the ellipse to draw the arc along (i.e. [width, height])
 * @param rotation angle in degrees to rotate ellipse about its center
 * @param largeArc 0 to draw shorter of 2 arcs, 1 to draw longer
 * @param arcSweep 0 to rotate CCW, 1 to rotate CW
 * @returns Path data for a wedge
 */
export const wedge = (
  center: Vec2,
  start: Vec2,
  end: Vec2,
  radius: Vec2,
  rotation: Num,
  largeArc: Num,
  arcSweep: Num,
): PathData =>
  compDict.wedge.body(
    context,
    center,
    start,
    end,
    radius,
    rotation,
    largeArc,
    arcSweep,
  ).value.contents;

/**
 * Find the point that is located at dist r along a line between p1 and p2.
 * @param p1 start point of line segment
 * @param p2 endpoint of line segment
 * @param r distance from p1 to travel along the line
 * @returns vector representation of the point of intersection
 */
export const ptOnLine = (p1: VecN, p2: VecN, r: Num): VecN =>
  compDict.ptOnLine.body(context, p1, p2, r).value.contents;

/**
 * Return 0 if direction of rotation is CCW, 1 if direction of rotation is CW.
 * @param [x1, y1] x, y coordinates of the circle/ellipse that the arc is drawn on
 * @param start start point of the arc
 * @param end end point of the arc
 * @returns 0 or 1 depending on CCW or CW rotation
 */
export const arcSweepFlag = ([x1, y1]: Vec2, start: Vec2, end: Vec2): Num =>
  compDict.arcSweepFlag.body(context, [x1, y1], start, end).value.contents;

/**
 * Return the unsigned angle between vectors `u, v`, in radians.
 * Assumes that both u and v have nonzero magnitude.
 * The returned value will be in the range [0,pi].
 * @param u A vector
 * @param v A vector
 * @returns Angle between vectors in radians
 */
export const angleBetween = (u: VecN, v: VecN): Num =>
  compDict.angleBetween.body(context, u, v).value.contents;

/**
 * Return the signed angle from vector `u` to vector `v`, in radians.
 * Assumes that both u and v are 2D vectors and have nonzero magnitude.
 * The returned value will be in the range [-pi,pi].
 * @param u A vector
 * @param v A vector
 * @returns Signed angle from u to v in radians
 */
export const angleFrom = (u: VecN, v: VecN): Num =>
  compDict.angleFrom.body(context, u, v).value.contents;

/**
 * Return the 2D cross product of `u` and `v`, equal to the determinant of the 2x2 matrix [u v]
 * @param u A vector
 * @param v A vector
 * @returns 2D cross product
 */
export const cross2D = (u: Vec2, v: Vec2): Num =>
  compDict.cross2D.body(context, u, v).value.contents;

/**
 * Return the 3D cross product of `u` and `v`.
 * @param u A vector
 * @param v A vector
 * @returns 3D cross product
 */
export const cross = (u: Vec3, v: Vec3): Vec3 =>
  compDict.cross.body(context, u, v).value.contents as Vec3;

/**
 * Return the intersection of a line passing through `a0` and `a1` with a line passing through `b0` and `b1`
 * @param a0 First point of first line
 * @param a1 Second point of first line
 * @param b0 First point of second line
 * @param b1 Second point of second line
 * @returns Intersection point
 */
export const lineLineIntersection = (
  a0: Vec2,
  a1: Vec2,
  b0: Vec2,
  b1: Vec2,
): Vec2 =>
  compDict.lineLineIntersection.body(context, a0, a1, b0, b1).value
    .contents as Vec2;

/**
 * Return a point located at the midpoint between pts `start` and `end`
 * @param start First point
 * @param end Second point
 * @returns Midpoint
 */
export const midpoint = (start: VecN, end: VecN): VecN =>
  compDict.midpoint.body(context, start, end).value.contents;

/**
 * Return a point located at the midpoint of a line `s1` but offset by `padding` in its normal direction (for labeling).
 * @param s1 A line
 * @param padding Padding between midpoint and label
 * @returns Offset midpoint
 */
export const midpointOffset = (s1: Line, padding: Num): Vec2 =>
  compDict.midpointOffset.body(
    context,
    toPenroseShape(s1) as PenroseLine<Num>,
    padding,
  ).value.contents as Vec2;

/**
 * Return a list of points for a chevron shape comprised of two line segments intersecting at a right angle at the midpoint of `s1`, which can then be passed to `pathFromPoints` to draw the chevron.
 * @param s1 A line
 * @param padding Length of each line segment
 * @returns List of points for chevron
 */
export const chevron = (s1: Line, padding: Num): Vec2[] =>
  compDict.chevron.body(
    context,
    toPenroseShape(s1) as PenroseLine<Num>,
    padding,
  ).value.contents as Vec2[];

/**
 * Return a point located at `padding` of a line `s1` offset by `padding` in its normal direction (for making right angle markers).
 * @param pt1 First point
 * @param pt2 Second point
 * @param pt3 Third point
 * @param padding Offset from line to returned point
 * @returns Offset point
 */
export const innerPointOffset = (
  pt1: Vec2,
  pt2: Vec2,
  pt3: Vec2,
  padding: Num,
): Vec2 =>
  compDict.innerPointOffset.body(context, pt1, pt2, pt3, padding).value
    .contents as Vec2;

/**
 * Create equally spaced tick marks centered at the midpoint of a line
 * @param pt1 starting point of a line
 * @param pt2 ending point of a line
 * @param spacing space in px between each tick
 * @param numTicks number of tick marks to create
 * @param tickLength 1/2 length of each tick
 * @returns Path data for tick marks
 */
export const ticksOnLine = (
  pt1: Vec2,
  pt2: Vec2,
  spacing: Num,
  numTicks: number,
  tickLength: Num,
): PathData =>
  compDict.ticksOnLine.body(context, pt1, pt2, spacing, numTicks, tickLength)
    .value.contents;

/**
 * Given two orthogonal segments that intersect at `intersection`, and a size `len`
 * return a path comprised of three points that describe a perpendicular mark at the angle where the segments intersect.
 * @param s1 First line segment
 * @param s2 Second line segment
 * @param intersection Point of intersection
 * @param len Side length of square marker
 * @returns Path data for oriented square
 */
export const orientedSquare = (
  s1: Line,
  s2: Line,
  intersection: Vec2,
  len: Num,
): PathData =>
  compDict.orientedSquare.body(
    context,
    toPenroseShape(s1) as PenroseLine<Num>,
    toPenroseShape(s2) as PenroseLine<Num>,
    intersection,
    len,
  ).value.contents;

/**
 * Given three lines `l1, l2, l3` that already form a triangle, return a path that describes the triangle (which can then be filled, etc.).
 * @param l1 First line
 * @param l2 Second line
 * @param l3 Third line
 * @returns Path data for triangle
 */
export const triangle = (l1: Line, l2: Line, l3: Line): PathData =>
  compDict.triangle.body(
    context,
    toPenroseShape(l1) as PenroseLine<Num>,
    toPenroseShape(l2) as PenroseLine<Num>,
    toPenroseShape(l3) as PenroseLine<Num>,
  ).value.contents;

/**
 * Return the average of floats `x` and `y`.
 * @param x `x`
 * @param y `y`
 * @returns Average of x and y
 */
export const average2 = (x: Num, y: Num): Num =>
  compDict.average2.body(context, x, y).value.contents;

/**
 * Return the average of the floats in the list `xs`.
 * @param xs `xs`
 * @returns Average of xs
 */
export const average = (xs: VecN): Num =>
  compDict.average.body(context, xs).value.contents;

/**
 * Return the normalized version of vector `v`.
 * @param v `v`
 * @returns Normalized vector
 */
export const unit = (v: VecN): VecN =>
  compDict.unit.body(context, v).value.contents;

/**
 * Return a uniform random value between minVal and maxValue.
 * @param minVal minimum value
 * @param maxVal maximum value
 * @returns Random value
 */
export const random = (minVal: Num, maxVal: Num): Num =>
  compDict.random.body(context, minVal, maxVal).value.contents;

/**
 * Return a uniform random value between 0 and 1
 * @returns Random value between 0 and 1
 */
export const unitRandom = (): Num =>
  compDict.unitRandom.body(context).value.contents;

/**
 * Return a random value sampled from the uniform distribution on the unit disk.
 * @returns Random point on unit disk
 */
export const diskRandom = (): Vec2 =>
  compDict.diskRandom.body(context).value.contents as Vec2;

/**
 * Return a random value sampled from the uniform distribution on the unit circle.
 * @returns Random point on unit circle
 */
export const circleRandom = (): Vec2 =>
  compDict.circleRandom.body(context).value.contents as Vec2;

/**
 * Return a random value sampled from the uniform distribution on the unit sphere.
 * @returns Random point on unit sphere
 */
export const sphereRandom = (): Vec3 =>
  compDict.sphereRandom.body(context).value.contents as Vec3;

/**
 * Return a random value sampled from a normal distribution with mean 0 and standard deviation 1.
 * @returns Random value from normal distribution
 */
export const normalRandom = (): Num =>
  compDict.normalRandom.body(context).value.contents;

/**
 * Return a random point sampled from the uniform distribution on a triangle with vertices a, b, c.
 * @param a First vertex
 * @param b Second vertex
 * @param c Third vertex
 * @returns Random point in triangle
 */
export const triangleRandom = (a: Vec2, b: Vec2, c: Vec2): Vec2 =>
  compDict.triangleRandom.body(context, a, b, c).value.contents as Vec2;

/**
 * Sample a random color once, with opacity `alpha` and colorType `colorType` (`"rgb"` or `"hsv"`).
 * @param alpha Opacity
 * @param colorType Color model
 * @returns Random color
 */
export const sampleColor = (alpha: Num, colorType: "rgb" | "hsv"): Color =>
  fromPenroseColor(compDict.sampleColor.body(context, alpha, colorType).value);

/**
 * Return a uniform index value between minIndex and maxIndex.
 * @param minIndex minimum index
 * @param maxIndex maximum index
 * @returns Random index
 */
export const randomIndex = (minIndex: Num, maxIndex: Num): number =>
  compDict.randomIndex.body(context, minIndex, maxIndex).value.contents;

/**
 * Set the opacity of a color `color` to `frac`.
 * @param color Color
 * @param frac Opacity
 * @returns Color with new opacity
 */
export const setOpacity = (color: Color, frac: Num): Color =>
  fromPenroseColor(
    compDict.setOpacity.body(context, toPenroseColor(color), frac).value,
  );

/**
 * Multiply a matrix `m` and a vector `v` (where `v` is implicitly treated as a column vector).
 * @param m A matrix
 * @param v A vector
 * @returns Result of matrix-vector multiplication
 */
export const mul = (m: VecN[], v: VecN): VecN =>
  compDict.mul.body(context, m, v).value.contents;

/**
 * Return the barycenter of the triangle with vertices `a`, `b`, `c`.
 * @param a First vertex
 * @param b Second vertex
 * @param c Third vertex
 * @returns Barycenter
 */
export const barycenter = (a: Vec2, b: Vec2, c: Vec2): Vec2 =>
  compDict.barycenter.body(context, a, b, c).value.contents as Vec2;

/**
 * Return the circumcenter of the triangle with vertices `p`, `q`, `r`.
 * @param p First vertex
 * @param q Second vertex
 * @param r Third vertex
 * @returns Circumcenter
 */
export const circumcenter = (p: Vec2, q: Vec2, r: Vec2): Vec2 =>
  compDict.circumcenter.body(context, p, q, r).value.contents as Vec2;

/**
 * Return the circumradius of the triangle with vertices `p`, `q`, `r`.
 * @param p First vertex
 * @param q Second vertex
 * @param r Third vertex
 * @returns Circumradius
 */
export const circumradius = (p: Vec2, q: Vec2, r: Vec2): Num =>
  compDict.circumradius.body(context, p, q, r).value.contents;

/**
 * Return the incenter of the triangle with vertices `p`, `q`, `r`.
 * @param p First vertex
 * @param q Second vertex
 * @param r Third vertex
 * @returns Incenter
 */
export const incenter = (p: Vec2, q: Vec2, r: Vec2): Vec2 =>
  compDict.incenter.body(context, p, q, r).value.contents as Vec2;

/**
 * Return the inradius of the triangle with vertices `p`, `q`, `r`.
 * @param p First vertex
 * @param q Second vertex
 * @param r Third vertex
 * @returns Inradius
 */
export const inradius = (p: Vec2, q: Vec2, r: Vec2): Num =>
  compDict.inradius.body(context, p, q, r).value.contents;

/**
 * Return the square of the number `x`.
 * @param x `x`
 * @returns Square of x
 */
export const sqr = (x: Num): Num =>
  compDict.sqr.body(context, x).value.contents;

/**
 * Return the square root of the number `x`. (NOTE: if `x < 0`, you may get `NaN`s)
 * @param x `x`
 * @returns Square root of x
 */
export const sqrt = (x: Num): Num =>
  compDict.sqrt.body(context, x).value.contents;

/**
 * Return the max of the numbers `x`, `y`.
 * @param x `x`
 * @param y `y`
 * @returns Maximum of x and y
 */
export const max = (x: Num, y: Num): Num =>
  compDict.max.body(context, x, y).value.contents;

/**
 * Return the min of the numbers `x`, `y`.
 * @param x `x`
 * @param y `y`
 * @returns Minimum of x and y
 */
export const min = (x: Num, y: Num): Num =>
  compDict.min.body(context, x, y).value.contents;

/**
 * Return the absolute value of the number `x`.
 * @param x `x`
 * @returns Absolute value of x
 */
export const abs = (x: Num): Num =>
  compDict.abs.body(context, x).value.contents;

/**
 * Convert the angle `theta` from degrees to radians.
 * @param theta `theta`
 * @returns Angle in radians
 */
export const toRadians = (theta: Num): Num =>
  compDict.toRadians.body(context, theta).value.contents;

/**
 * Convert the angle `theta` from radians to degrees.
 * @param theta The angle in radians
 * @returns The angle in degrees
 */
export const toDegrees = (theta: Num): Num =>
  compDict.toDegrees.body(context, theta).value.contents;

/**
 * Return the Euclidean norm of the vector `v`.
 * @param v A vector
 * @returns The Euclidean norm of v
 */
export const norm = (v: VecN): Num =>
  compDict.norm.body(context, v).value.contents;

/**
 * Return the Euclidean norm squared of the vector `v`.
 * @param v A vector
 * @returns The Euclidean norm squared of v
 */
export const normsq = (v: VecN): Num =>
  compDict.normsq.body(context, v).value.contents;

/**
 * Return the Euclidean distance between the vectors `v` and `w`.
 * @param v A vector
 * @param w A vector
 * @returns The Euclidean distance between v and w
 */
export const vdist = (v: VecN, w: VecN): Num =>
  compDict.vdist.body(context, v, w).value.contents;

/**
 * Returns the scalar-vector product.
 * @param s A scalar
 * @param v A vector
 * @returns The scalar-vector product
 */
export const vmul = (s: Num, v: VecN): VecN =>
  compDict.vmul.body(context, s, v).value.contents;

/**
 * Return the Euclidean distance squared between the vectors `v` and `w`.
 * @param v A vector
 * @param w A vector
 * @returns The Euclidean distance squared between v and w
 */
export const vdistsq = (v: VecN, w: VecN): Num =>
  compDict.vdistsq.body(context, v, w).value.contents;

/**
 * Return the angle made by the vector `v` with the positive x-axis.
 * @param v A vector
 * @returns The angle made by v with the positive x-axis
 */
export const angleOf = (v: VecN): Num =>
  compDict.angleOf.body(context, v).value.contents;

/**
 * Base e of the natural logarithm.
 * @returns The value of e
 */
export const MathE = (): Num => compDict.MathE.body(context).value.contents;

/**
 * Ratio of the circumference of a circle to its diameter.
 * @returns The value of PI
 */
export const MathPI = (): Num => compDict.MathPI.body(context).value.contents;

/**
 * Rotate a 2D vector `v` by 90 degrees counterclockwise.
 * @param v A vector
 * @returns The rotated vector
 */
export const rot90 = (v: Vec2): Vec2 =>
  compDict.rot90.body(context, v).value.contents as Vec2;

/**
 * Rotate a 2D vector `v` by theta degrees counterclockwise.
 * @param v A vector
 * @param theta degrees to rotate counterclockwise
 * @returns The rotated vector
 */
export const rotateBy = (v: Vec2, theta: Num): Vec2 =>
  compDict.rotateBy.body(context, v, theta).value.contents as Vec2;

/**
 * Return the signed distance between a shape and a point
 * @param s A shape (Rect, Circle, Polygon, Line, or Polyline)
 * @param p A point
 * @returns The signed distance
 */
export const signedDistance = (s: Shape, p: Vec2): Num =>
  compDict.signedDistance.body(context, toPenroseShape(s), p).value.contents;

/**
 * Returns the distance between a rect and a point
 * @param rect A rectangle represented by its corner points
 * @param pt A point
 * @returns The signed distance
 */
export const signedDistanceRect = (rect: Vec2[], pt: Vec2): Num =>
  compDict.signedDistanceRect.body(context, rect, pt).value.contents;

/**
 * Returns the distance between a circle and a point
 * @param c center of circle
 * @param r radius of circle
 * @param pt the point
 * @returns The signed distance
 */
export const signedDistanceCircle = (c: Vec2, r: Num, pt: Vec2): Num =>
  compDict.signedDistanceCircle.body(context, c, r, pt).value.contents;

/**
 * Returns the distance between a polygon and a point
 * @param pts points of the polygon
 * @param pt the point
 * @returns The signed distance
 */
export const signedDistancePolygon = (pts: Vec2[], pt: Vec2): Num =>
  compDict.signedDistancePolygon.body(context, pts, pt).value.contents;

/**
 * Returns the distance between an ellipse and a point
 * @param c center of ellipse
 * @param rx horizontal radius of ellipse
 * @param ry vertical radius of ellipse
 * @param pt the point
 * @returns The signed distance
 */
export const signedDistanceEllipse = (
  c: Vec2,
  rx: Num,
  ry: Num,
  pt: Vec2,
): Num =>
  compDict.signedDistanceEllipse.body(context, c, rx, ry, pt).value.contents;

/**
 * Returns the distance between a line and a point
 * @param start start of line
 * @param end end of line
 * @param pt the point
 * @returns The signed distance
 */
export const signedDistanceLine = (start: Vec2, end: Vec2, pt: Vec2): Num =>
  compDict.signedDistanceLine.body(context, start, end, pt).value.contents;

/**
 * Returns the distance between a line and a polyline
 * @param pts points of the polyline
 * @param pt the point
 * @returns The signed distance
 */
export const signedDistancePolyline = (pts: Vec2[], pt: Vec2): Num =>
  compDict.signedDistancePolyline.body(context, pts, pt).value.contents;

/**
 * Returns the signed distance between a group of shapes and a point
 * @param shapes A list of shapes
 * @param pt A point
 * @returns The signed distance
 */
export const signedDistanceGroup = (shapes: Shape[], pt: Vec2): Num =>
  compDict.signedDistanceGroup.body(
    context,
    shapes.map((s) => toPenroseShape(s)),
    pt,
  ).value.contents;

/**
 * Construct a unit vector u in the direction of the given angle theta (in radians).
 * @param theta direction
 * @returns A unit vector
 */
export const unitVector = (theta: Num): Vec2 =>
  compDict.unitVector.body(context, theta).value.contents as Vec2;

/**
 * Given a point p and vector v, find the first point where the ray r(t)=p+tv intersects the given shape S.
 * If there are no intersections, returns p.
 * @param S A shape (Circle, Rect, Line, Polyline, Polygon, Ellipse, or Group)
 * @param p A point
 * @param v A vector
 * @returns The intersection point
 */
export const rayIntersect = (S: Exclude<Shape, Path>, p: Vec2, v: Vec2): Vec2 =>
  compDict.rayIntersect.body(
    context,
    toPenroseShape(S) as Exclude<PenroseShape<Num>, PenrosePath<Num>>,
    p,
    v,
  ).value.contents as Vec2;

/**
 * Given a point p and vector v, returns the distance to the first point where the ray r(t)=p+tv intersects the shape S.
 * If there are no intersections, returns Infinity.
 * @param S A shape (Circle, Rect, Line, Polyline, Polygon, Ellipse, or Group)
 * @param p A point
 * @param v A vector
 * @returns The distance to intersection
 */
export const rayIntersectDistance = (
  S: Exclude<Shape, Path>,
  p: Vec2,
  v: Vec2,
): Num =>
  compDict.rayIntersectDistance.body(
    context,
    toPenroseShape(S) as Exclude<PenroseShape<Num>, PenrosePath<Num>>,
    p,
    v,
  ).value.contents;

/**
 * Ray intersection with a circle
 * @param c center of circle
 * @param r radius of circle
 * @param p A point
 * @param v A vector
 * @returns The intersection point
 */
export const rayIntersectCircle = (c: Vec2, r: Num, p: Vec2, v: Vec2): Vec2 =>
  compDict.rayIntersectCircle.body(context, c, r, p, v).value.contents as Vec2;

/**
 * Distance to ray intersection with a circle
 * @param c center of circle
 * @param r radius of circle
 * @param p A point
 * @param v A vector
 * @returns The distance to intersection
 */
export const rayIntersectCircleDistance = (
  c: Vec2,
  r: Num,
  p: Vec2,
  v: Vec2,
): Num =>
  compDict.rayIntersectCircleDistance.body(context, c, r, p, v).value.contents;

/**
 * Ray intersection with an ellipse
 * @param c center of ellipse
 * @param rx horizontal radius of ellipse
 * @param ry vertical radius of ellipse
 * @param p A point
 * @param v A vector
 * @returns The intersection point
 */
export const rayIntersectEllipse = (
  c: Vec2,
  rx: Num,
  ry: Num,
  p: Vec2,
  v: Vec2,
): Vec2 =>
  compDict.rayIntersectEllipse.body(context, c, rx, ry, p, v).value
    .contents as Vec2;

/**
 * Distance to ray intersection with an ellipse
 * @param c center of ellipse
 * @param rx horizontal radius of ellipse
 * @param ry vertical radius of ellipse
 * @param p A point
 * @param v A vector
 * @returns The distance to intersection
 */
export const rayIntersectEllipseDistance = (
  c: Vec2,
  rx: Num,
  ry: Num,
  p: Vec2,
  v: Vec2,
): Num =>
  compDict.rayIntersectEllipseDistance.body(context, c, rx, ry, p, v).value
    .contents;

/**
 * Ray intersection with a line
 * @param start start point of the line
 * @param end end point of the line
 * @param p A point
 * @param v A vector
 * @returns The intersection point
 */
export const rayIntersectLine = (
  start: Vec2,
  end: Vec2,
  p: Vec2,
  v: Vec2,
): Vec2 =>
  compDict.rayIntersectLine.body(context, start, end, p, v).value
    .contents as Vec2;

/**
 * Distance to ray intersection with a line
 * @param start start point of the line
 * @param end end point of the line
 * @param p A point
 * @param v A vector
 * @returns The distance to intersection
 */
export const rayIntersectLineDistance = (
  start: Vec2,
  end: Vec2,
  p: Vec2,
  v: Vec2,
): Num =>
  compDict.rayIntersectLineDistance.body(context, start, end, p, v).value
    .contents;

/**
 * Ray intersection with a rectangle
 * @param rect The top-right, top-left, bottom-left, bottom-right points (in that order) of the rectangle
 * @param p A point
 * @param v A vector
 * @returns The intersection point
 */
export const rayIntersectRect = (rect: Vec2[], p: Vec2, v: Vec2): Vec2 =>
  compDict.rayIntersectRect.body(context, rect, p, v).value.contents as Vec2;

/**
 * Distance to ray intersection with a rectangle
 * @param rect The top-right, top-left, bottom-left, bottom-right points (in that order) of the rectangle
 * @param p A point
 * @param v A vector
 * @returns The distance to intersection
 */
export const rayIntersectRectDistance = (rect: Vec2[], p: Vec2, v: Vec2): Num =>
  compDict.rayIntersectRectDistance.body(context, rect, p, v).value.contents;

/**
 * Ray intersection with a polygon or polyline
 * @param pts The points of the polygon or polyline
 * @param closed Whether the shape is closed (true for polygon, false for polyline)
 * @param p A point
 * @param v A vector
 * @returns The intersection point
 */
export const rayIntersectPoly = (
  pts: Vec2[],
  closed: boolean,
  p: Vec2,
  v: Vec2,
): Vec2 =>
  compDict.rayIntersectPoly.body(context, pts, closed, p, v).value
    .contents as Vec2;

/**
 * Distance to ray intersection with a polygon or polyline
 * @param pts The points of the polygon or polyline
 * @param closed Whether the shape is closed (true for polygon, false for polyline)
 * @param p A point
 * @param v A vector
 * @returns The distance to intersection
 */
export const rayIntersectPolyDistance = (
  pts: Vec2[],
  closed: boolean,
  p: Vec2,
  v: Vec2,
): Num =>
  compDict.rayIntersectPolyDistance.body(context, pts, closed, p, v).value
    .contents;

/**
 * Ray intersection with a group of shapes
 * @param shapes A list of shapes
 * @param p A point
 * @param v A vector
 * @returns The intersection point
 */
export const rayIntersectGroup = (shapes: Shape[], p: Vec2, v: Vec2): Vec2 =>
  compDict.rayIntersectGroup.body(
    context,
    shapes.map((s) => toPenroseShape(s)),
    p,
    v,
  ).value.contents as Vec2;

/**
 * Distance to ray intersection with a group of shapes
 * @param shapes A list of shapes
 * @param p A point
 * @param v A vector
 * @returns The distance to intersection
 */
export const rayIntersectGroupDistance = (
  shapes: Shape[],
  p: Vec2,
  v: Vec2,
): Num =>
  compDict.rayIntersectGroupDistance.body(
    context,
    shapes.map((s) => toPenroseShape(s)),
    p,
    v,
  ).value.contents;

/**
 * Given a point p and vector v, find the unit normal at the first point where the ray r(t)=p+tv intersects the given shape S.
 * If there are no intersections, returns (0,0).
 * @param S A shape (Circle, Rect, Line, Polyline, Polygon, Ellipse, or Group)
 * @param p A point
 * @param v A vector
 * @returns The unit normal at the intersection point
 */
export const rayIntersectNormal = (
  S: Exclude<Shape, Path>,
  p: Vec2,
  v: Vec2,
): Vec2 =>
  compDict.rayIntersectNormal.body(
    context,
    toPenroseShape(S) as Exclude<PenroseShape<Num>, PenrosePath<Num>>,
    p,
    v,
  ).value.contents as Vec2;

/**
 * Normal at ray intersection with a circle
 * @param c center of circle
 * @param r radius of circle
 * @param p A point
 * @param v A vector
 * @returns The unit normal at the intersection point
 */
export const rayIntersectNormalCircle = (
  c: Vec2,
  r: Num,
  p: Vec2,
  v: Vec2,
): Vec2 =>
  compDict.rayIntersectNormalCircle.body(context, c, r, p, v).value
    .contents as Vec2;

/**
 * Normal at ray intersection with an ellipse
 * @param c center of ellipse
 * @param rx horizontal radius of ellipse
 * @param ry vertical radius of ellipse
 * @param p A point
 * @param v A vector
 * @returns The unit normal at the intersection point
 */
export const rayIntersectNormalEllipse = (
  c: Vec2,
  rx: Num,
  ry: Num,
  p: Vec2,
  v: Vec2,
): Vec2 =>
  compDict.rayIntersectNormalEllipse.body(context, c, rx, ry, p, v).value
    .contents as Vec2;

/**
 * Normal at ray intersection with a line
 * @param start start point of the line
 * @param end end point of the line
 * @param p A point
 * @param v A vector
 * @returns The unit normal at the intersection point
 */
export const rayIntersectNormalLine = (
  start: Vec2,
  end: Vec2,
  p: Vec2,
  v: Vec2,
): Vec2 =>
  compDict.rayIntersectNormalLine.body(context, start, end, p, v).value
    .contents as Vec2;

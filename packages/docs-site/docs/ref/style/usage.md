# Style Usage

Given a _domain_ schema (specifying the domain of the diagram) and a _substance_ program (specifying _what_ to draw), the _style_ schema describes the recipe of drawing the objects and relations on a canvas.

A _style_ schema is composed of _blocks_, of which there are two types:

- _namespaces_, which can be used to help program organization;
- _style blocks_, which carry out the meat of the diagram generation.

## Namespaces

The syntax for a namespace is as follows:

```
namespace_name {
    ... (the namespace body)
}
```

Refer to [this section](usage#block-body) for a detailed explanation of what may appear in the body of a namespace.

Values declared within a namespace can be read outside of the namespace using the "dot" operator:

```
namespace_name.field_name
```

Hence they are also called _global_ variables. Overwriting these values is not allowed.

### Canvas Preamble Block

Each _style_ schema _must_ contain a _canvas preamble block_, a special type of namespace which describes the width and height of the canvas. For example, preamble block

```
canvas {
    width = 800
    height = 700
}
```

tells Penrose that the drawing canvas should have a width of 800 pixels and a height of 700 pixels.

## Selector Blocks

Selector Blocks are the most important component in a _style_ schema, since they actually describe _how_ to draw elements of a diagram. The syntax for Selector Blocks is as follows:

```
forall list_object_declarations
where list_relations
with list_object_declarations {
    ... (Selector Block Body)
}
```

where

- `list_object_declarations` is a **semicolon**-separated list of object declarations, similar to the object declarations in the _substance_ schema. Each object declaration has syntax `type_name object_name`. The names declared in `list_object_declarations` are referred to as _style variables_.
- `list_relations` is a **semicolon**-separated list of constraints (about objects in `list_object_declaration`) that must be satisfied in order for this style block to be triggered.

One might observe that both the `forall` clause and the `with` clause take in list of object declarations. The two clauses are treated equivalently in Selector Blocks. Variables declared in these clauses can be accessed within the body of the Selector Block.

If the `where` or `with` clause is empty, it needs to be omitted.

In the set-theory example, a style block may look like

```
forall Set x {
    ...
}
```

or

```
forall Set x; Set y
where IsSubset (x, y) {
    ...
}
```

or

```
forall Set x
where IsSubst(x, y)
with Set y {

}
```

### Matching style block against substance program in general

Penrose functions by matching a style block against a _substance_ program. In a nutshell, given a style block

```
forall Set x; Set y
where IsSubset (x, y) {
    ...
}
```

the Penrose compiler searches through the _substance_ program to find sets of objects consistent with `Set x; Set y` such that `IsSubset(x, y)` is satisfied. This is done through generating mappings from _style variables_ to _substance variables_, which are the objects in the _substance_ program.

For instance, consider a simple set-theory _substance_ program that works with the previous style block:

```
Set A, B, C
IsSubset (A, B)
IsSubset (B, C)
```

By matching the style block against the _substance_ program, we essentially consider six possible mappings (note that repeated elements are not allowed), some of which are valid and some are invalid:

| Mapping          | `IsSubset(x, y)` becomes | Satisfied by _substance_ |
| :--------------- | :----------------------- | ------------------------ |
| `x -> A; y -> B` | `IsSubset(A, B)`         | Yes                      |
| `x -> A; y -> C` | `IsSubset(A, C)`         | No                       |
| `x -> B; y -> A` | `IsSubset(B, A)`         | No                       |
| `x -> B; y -> C` | `IsSubset(B, C)`         | Yes                      |
| `x -> C; y -> A` | `IsSubset(C, A)`         | No                       |
| `x -> C; y -> B` | `IsSubset(C, B)`         | No                       |

Here, Penrose filters out mappings which do not satisfy the constraints listed in the _style_ block, and keeps a list of _good_ mappings (in this example, two mappings are kept). For each _good_ mapping, the body of the _style_ block (`list_body_expressions`) is executed, where each instance of the _style_ variables (`x` and `y`) is substituted with the corresponding _substance_ variables (once with `A` and `B`, once with `B` and `C`).

### Object Declarations

In the list of object declarations in a _style_ block, we can declare two types of objects, which are matched differently by the Penrose compiler.

#### Substance objects

We can declare a _substance_ object, whose object name is surrounded by backticks. For instance,

```
forall Set `A` {
    ...
}
```

can only be mapped to the _substance_ object with the exact same name (`A`) provided that the types match (subtyping allowed). In other words, given _substance_ program

```
Set A, B, C
```

matching the _style_ block against the _substance_ block yields only one valid mapping: `` `A` -> A ``.

#### Style objects

If an object name is not surrounded by backticks, then this object is a _style_ object with a _style_ variable. As seen before, the Penrose compiler will try to map _style_ variables to any _substance_ objects, provided that their types match (subtyping allowed).

### Allowed Relations

A _style_ block supports three types of relations, two of which can also be seen in the _substance_ program.

#### Predicate Applications

Just like in the _substance_ program, each predicate application has syntax

```
predicate_name (argument_list)
```

where elements of `argument_list` can refer to objects declared in `list_object_declarations`, or be other predicate applications. The types must still match, allowing subtyping.

Optionally, one can give an alias to a predicate application:

```
predicate_name (argument_list) as alias_name
```

If such an alias is set, then `alias_name` will be accessible in the style block body, and it will always refer to the version of the predicate application within the _substance_ program.

##### Symmetry

If a predicate is declared as symmetric, then it gets special treatment.
Suppose we have the following domain schema:

```
type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
symmetric predicate Bond (Atom, Atom)
```

and the following style block:

```
forall Hydrogen h; Oxygen o
where Bond (h, o) {
    ...
}
```

The style block will successfully match the following substance schema:

```
Hydrogen H
Oxygen O
Bond (O, H)
```

where `Bond (h, o)` in the style block matches against `Bond (O, H)` in the substance schema. Because Bond is declared symmetric, when Penrose looks for `Bond (h, o)`, it also looks for `Bond (o, h)` and finds a match. In other words, the matching algorithm handles the equivalence between `Bond (h, o)` and `Bond (o, h)` correctly.

#### Function and Constructor Applications

Each function or constructor application has syntax

```
object_name := function_name (argument_list)
```

We do not allow aliasing for function and constructor applications. Arguments in `argument_list` must have types that match the domain argument types, similar to the substance schema.

#### Object Property Relations

Aside from predicate applications and function (constructor) applications, Penrose also supports a predicate-like relation that checks whether an object has a certain property, say `label`. For instance, we may write

```
forall Set s
where s has label {
    ... some code that uses s.label
}
```

If a certain `Set A` in the _substance_ program does not have a label (perhaps due to `NoLabel` declarations), then `s` will not be mapped to `A`, thus preventing an access of nonexistent properties.

We can further distinguish between math labels and text labels (see [substance labeling](../substance/usage#labeling-statements)): `where p has math label` matches math labels, whereas `where p has text label` matches text labels.

### Matching Deduplication

The matching algorithm is designed to avoid duplicated mappings. If two mappings give us the same set of matched objects (in the _substance_ program) and the equivalent set of matched substance relations (predicate applications and function or constructor applications), then the algorithm only triggers on one of them.

For instance, say Penrose tries to match the _style_ block

```
forall Set x; Set y {
    ...
}
```

against _substance_ program

```
Set A, B
```

Then, only one of mappings `x -> A; y -> B` and `x -> B; y -> A` triggers the Style block.

### Reserved Variables

Within a _style_ block body, some variable names are reserved for metadata purposes:

- `match_total` is an integer that refers to the number of times that this _style_ blocks will be triggered (or matched) in total; and
- `match_id` is the 1-indexed ordinal of this current matching.

These values can directly be read or overwritten within the style block body if needed.

## Selector Block Body

The body of a block contains declarations of variables, shapes, and the relationship between objects.

### Assignments

We can assign an expression to a field:

```
type_annotation field = expression
```

where

- `type_annotation` is an optional field denoting the type of the variable,
- `field` is a path to the variable being assigned, and
- `expression` is the expression to be assigned to `field`.

`field` can either be

- A single identifier, which denotes a local assignment, not accessible outside of this matching; or
- An object name (defined in `list_object_declarations`) or predicate application alias, followed by a dot operator and an identifier, which denotes an assignment bound to a _substance_ instance of object or predicate application after we substitute in the mapping. These assignments are accessible if the same _substance_ object or predicate application is matched again.

For example, consider the following _style_ block:

```
forall MyType t1; MyType t2
where MyPredicate (t1, t2) as r1 {
	x = ... // this is a local assignment not accessible outside of this substitution or this block
	t1.a = ... // this is bound to the substance instance of `MyType t1`
	r1.c = ... // this is bound to the substance instance of `MyPredicate (t1, t2)`
}
```

Refer to [this section](usage#expressions-and-their-types) for a detailed explanation of the available expressions and their associated types.

### Override and Deletion

The _style_ language allows users to modify fields that are previously declared. The `override` keyword changes the value of the field. As an example,

```

forall Set X {
    shape X.shape = Circle {
        x: X.x
        r: 100
    }
}

forall Set `A` {
    override `A`.shape.r = 200
}
```

the radius of the circle for every `Set` is `100`, except if the `Set` has name `A`, then the radius is `200`.

Deletion of fields works similarly, with the `delete` keyword. This feature can be helpful for, e.g., removing visual elements for a subtype. For instance,

```
-- by default, draw a circle for all instances of type T
forall T x {
    x.widget = Circle { ... }
}

-- but don't draw this circle for instances of a subtype S <: T
forall S x {
    delete x.widget
}
```

Note that one must be careful not to reference deleted attributes in a later generic block. For instance, the following block will produce an error if invoked for an instance of `S`:

```
forall T x {
    shape x.newWidget = Circle {
        center : x.widget.center -- not defined for instances of S
    }
}
```

### Constraints and Objectives

A good diagram must satisfy some basic constraints, while trying to optimize upon some objectives (specifying diagram beauty). We declare these constraints and objectives within the style blocks. A constraint declaration has syntax

```
ensure constraint_name (argument_list)
```

and an objective declaration has syntax

```
encourage objective_name (argument_list)
```

where `argument_list` may refer to constant values, global / local variables, and other variables bound to _substnace_ instances of objects and predicate applications. A full list of available constraints and objectives can be found [here](./functions).

We also provide syntax sugar expressions for some commonly-used objectives and constraints. In particular,

- `a > b` is the syntax sugar for the constraint / objective `greaterThan(a, b)`,
- `a == b` is the syntax sugar for the constraint / objective `equal(a, b)`, and
- `a < b` is the syntax sugar for the constraint / objective `lessThan(a, b)`.

### Layering

We can specify the layering between two shapes (particularly useful when two shapes overlap) using layering statements: either

```
layer shape_1 above shape_2
```

or

```
layer shape_1 below shape_2
```

where `shape_1` and `shape_2` can be variables assigned to shapes.

We have special handling of layering statements for `Group` shapes, found [here](./shapes/group.md).

## Collector Blocks

Selector blocks match _style_ variables against _substance_ variables to produce multiple matches. Each match is independent from another. This characteristic makes it difficult to implement features that require aggregations over multiple matches. For example, we can't compute the sum of `center` fields of _all_ Substance variables that a selector matches on.

Collector Blocks enables these types of aggregations by introducing collections of _substance_ variables. The syntax is as follows:

```
collect <COLLECT> into <INTO>
where <WHERE>
with <WITH>
foreach <FOREACH> { ... }
```

The `where`, `with`, and `foreach` clauses are optional, and if they are empty, they must be omitted.

- `<COLLECT>` is an object declaration. The `<COLLECT> ` object is accessible, via the name given in the `<INTO>` clause, in the _style_ block body.
- `<INTO>` is the name assigned to the collection. The collection includes all the Substance objects that `<COLLECT>` matches to. Within the Style block, `<INTO>` conceptually represents a list of Substance objects.
- The `<WHERE>` clause has the same meaning as in standard `forall` Style selectors.
- `<WITH>` is a semicolon-separated list of object declarations. Objects in the `<WITH>` clause aren't collected, but may be used in `<WHERE>`.
- `<FOREACH>` is a semicolon-separated list of object declarations. Objects in the `<FOREACH>` clause are also not collected, but they arrange the `<COLLECT>` objects into groups. The entire `Collector` block runs once for each distinct match of the `<FOREACH>` clause. The `<FOREACH>` list can contain multiple declarations.

For example, suppose we have the Substance program that defines one set `s1` that contains elements `e1, e2`, and another set `s2` that contains elements `e3, e4, e5`, as follows:

```
Set s1
Element e1, e2
In(e1, s1)
In(e2, s1)

Set s2
Element e3, e4, e5
In(e3, s2)
In(e4, s2)
In(e5, s2)
```

We can write a Collector Block:

```
collect Element e into es
where In(e, s)
foreach Set s {
    ...
}
```

Under the above Substance program, this Collector Block would run twice:

- once with mapping `es -> [e1, e2]` and `s -> s1`
- once with mapping `es -> [e3, e4, e5]` and `s -> s2`.

Notice that we have splitted the set of all elements `[e1, e2, e3, e4, e5]` into two groups based on the `Set` object that they are contained within.

Within the body of the Collector Blocks, we can do everything that can be done in Selector Blocks, with the one exception that we can only access Style variables in `<INTO>` and `<FOREACH>`.

### Collection Access Expression

Within the Collector Block, the name in the `<INTO>` clause conceptually means a list of Substance objects. We can access the fields of each element in the collection using the "Collection Access" expression with syntax:

```
listof <FIELD NAME> from <COLLECTION NAME>
```

This expression takes the `<FIELD NAME>` field of each Substance variable in the collection `<COLLECTION NAME>`, and compiles these fields into an appropriate list.

For example, suppose `elements` is the name in the `<INTO>` block, conceptually representing some list of Substance objects `[e1, e2, ..., en]`. Then, we would expect the expression `listof field from elements` to produce a list `[e1.field, e2.field, ..., en.field]`.

Due to technical constraints, we cannot access _any_ field of each element of the collection. In the above example, if `e1.field` gives a color, then we cannot really put the color into a list, since Penrose does not support lists of colors. We restrict allowable types to as follows:

| Type of `field`    | Collects into                 |
| ------------------ | ----------------------------- |
| `FloatV` (number)  | `VectorV` (vector)            |
| `VectorV` (vector) | `MatrixV` (matrix)            |
| `ListV` (list)     | `LListV` (list of lists)      |
| `TupV` (2-tuple)   | `PtListV` (list of 2d points) |
| some shape         | `ShapeListV` (list of shapes) |

If, in the above example, `e1.field`, `e2.field`, etc. are numbers, then the expresion `listof field from elements` gives us a vector that contains all the values. We can directly plug the vector into accumulation functions such as `average` and `sum`.

## Expressions and their Types

The list of supported Style types is:

- `scalar`
- `int`
- `bool`
- `string`
- `path`
- `color`
- `file`
- `style`
- `shape`
- `vec2`, `vec3`, `vec4`
- `mat2x2`, `mat3x3`, `mat4x4`
- `function`
- `objective`
- `constraint`

These are what may appear in the optional `type_annotation` field of field assignments.

### Shapes

Shape declarations have syntax

```
shape_name {
    property_name_1 : value_1
    property_name_2 : value_2
    ...
}
```

Once declared, the value of each property can be accessed using

```
path_to_shape.property_name
```

For example,

```
forall Set x {
    -- declares a circle with radius 50
    x.shape = Circle {
        r : 50
    }
    -- set its center to be (50, 100)
    x.shape.center[0] = 50
    x.shape.center[1] = 100
}
```

Each property of a shape has a default value. A full list of available shapes and their properties (and their default values) can be found in the Shape Library.

### Unknown Scalar

The `?` expression evaluates to a scalar whose value is automatically determined by the Penrose engine.

### Strings

Strings have type `string` and string literals are delimited by double quotes. Strings can be concatenated using the `+` operator. For instance, to put parentheses around the label associated of `x`, write

```
string fancyLabel = "(" + x.label + ")"
```

### Vectors and matrices

Style supports dense n-dimensional vector and matrix types, and standard operations on these types. These types behave largely like small, dense matrix types found in other languages (such as GLSL), with some specific differences noted below. Note that these types are meant largely for manipulating small 2D, 3D, and 4D vectors/matrices in the context of standard graphics operations (transformations, perspective projection, etc.), and may not perform well for larger matrix manipulations. Like all other objects in Style, the value of any vector or matrix entry can be declared as unknown (`?`) and determined automatically via optimization by the layout engine.

#### Vector and matrix types

Style is designed to support n-dimensional dense vectors of type `vecN`, and square n-dimensional matrices of type `matNxN`, where in both cases `N` is an integer greater than or equal to 2. E.g., types commonly used for diagramming are `vec2` and `mat3x3`. Some library functions may be available only for vectors or matrices of a specific size (e.g., the function `cross(u,v)`, which computes a 3D cross product, assumes that both `u` and `v` have type `vec3`).

#### Initializing vectors and matrices

A vector is constructed by specifying its components. For instance,

```
vec2 u = (1.23, 4.56)
```

constructs a 2-dimension vector with `x`-component `1.23` and `y`-component `4.56`. As noted above, unknown values can be used as components, e.g.,

```
vec2 p = (?, 0.0)
```

specifies a point `p` that sits on the `x`-axis with an unknown `x`-coordinate which is determined by the optimizer, according to any constraints and objectives involving `p`. More advanced initializers (e.g., initializing a 3-vector from a 2-vector and a scalar) are currently not supported, but are planned for future language versions. In most cases, the same functionality can currently be emulated by directly referencing components of a vector, e.g.,

```
vec3 a = ( b[0], b[1], 1.0 )
```

A matrix is constructed by specifying a list of vectors. Each vector corresponds to a row (not a column) of the matrix. For instance,

```
mat2x2 A = ((1,2),(3,4))
```

initializes a 2x2 matrix where the top row has entries 1, 2 and the bottom row has entries 3, 4. Rows can also reference existing vectors, e.g.,

```
vec2 a1 = (1, 2)
vec2 a2 = (3, 4)
mat2x2 A = (a1, a2)
```

builds the same matrix as above. As with vectors, matrix entries can be unknown. E.g.,

```
scalar d = ?
mat3x3 D = ((d, 0, 0), (0, d, 0), (0, 0, d))
```

describes a 3x3 diagonal matrix, where all three diagonal entries take the same, undetermined value `d`.

#### Vector and matrix element access

Individual elements of a `vecN` can be accessed using square brackets, and an index `i` between `0` and `N`-1 (inclusive). For instance,

```
vec3 u = (1, 2, 3)
scalar y = u[1]
```

will extract the `y`-coordinate of `u` (i.e. `y=2`). Matrix entries are similarly accessed:

```
mat2x2 M = ((?, ?), (?, ?))
scalar trM = M[0][0] + M[1][1]
```

constructs an expression for the trace of `M`. In this case, since the elements of `M` are declared as unknown scalars, the value of the trace will depend on the entry values of the optimized matrix.

### Colors

Colors have type `color`, and include an alpha (i.e., opacity) channel. Colors can be specified via two different color models:

- `rgba(r, g, b, a)` defines a color via the RGB color model, with red, green, blue, and alpha values in the range [0, 1].
  - If the required color is fixed and known, one can also use the hexadecimal representation `#rrggbbaa`, which gets converted to the corresponding color in `rgba(r, g, b, a)`. If the alpha value is not provided in the hexadecimal representation, it defaults to 1.0.
- `hsva(h, s, v, a)` defines a color via the HSV color model, with hue in the range [0, 360], saturation and value in the range [0, 100], and alpha in the range [0, 1].

To specify that a color should be omitted altogether, you can also use `none()`. E.g.,

```
fillColor: none()
strokeColor: none()
```

Note that `none()` is different from using a 100% transparent color: it really prevents the fill or stroke from being drawn altogether.

### Computation Functions

Computation functions take in multiple values, and return a new value. The aforementioned functions `rgba`, `hsva`, and `none` are examples of computation functions. Other computation functions also exist, including many mathematical functions.

See [the list of computation functions](functions#computation-functions).

### Operations between Numerical Expressions

Aside from concatenation of strings using the operator `+`, the _style_ language supports the operations listed below on scalars, vectors, and matrices. Here we assume that `c` and `d` have type `scalar`, `u` and `v` have type `vecN`, and `A` and `B` have type `matNxN` (all for the same `N`).

Note that _elementside_ multiplication `.*` and division `./` get applied independently to each entry of a vector or matrix. For instance, if `u = (6, 8, 9)` and `v = (3, 2, 3)`, then the elementwise dicision operation `u ./ v` yields the vector `(2, 4, 3)` (i.e. six divided by three, eight divided by two, and nine divided by three).

#### Scalar-Scalar

- `c + d` - sum of `c` and `d`
- `c - d` - difference of `c` and `d`
- `c * d` - multiplies `c` and `d`
- `c / d` - divides `c` by `d`

#### Scalar-Vector

- `c * v` — scales `v` by `c` (from the left)
- `v * c` — scales `v` by `c` (from the right)
- `v / c` — divides `v` by `c`

#### Scalar-Matrix

- `c * A` — scales `A` by `c` (from the left)
- `A * c` — scales `A` by `c` (from the right)
- `A / c` — divides `A` by `c`

#### Vector-Vector

- `u + v` — sum of `u` and `v`
- `u - v` — difference of `u` and `v`
- `u .* v` — elementwise product of `u` and `v`
- `u ./ v` — elementwise quotient of `u` and `v`

#### Vector-Matrix

- `A*u` — matrix-vector product Au
- `u*A` — matrix vector product uᵀA

#### Matrix-Matrix

- `A * B` — matrix-matrix product AB
- `A + B` — sum of `A` and `B`
- `A - B` — difference of `A` and `B`
- `A .* B` — elementwise product of `A` and `B`
- `A ./ B` — elementwise quotient of `A` and `B`
- `A'` — matrix transpose Aᵀ

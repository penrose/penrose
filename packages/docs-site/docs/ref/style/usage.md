# Style Usage

Given a Domain schema (specifying the domain of the diagram), a Substance file specifies how the objects and relationships in any given Substance program should get translated into graphical icons and geometric relationships on a 2D canvas.

A Style program is composed of _blocks_, of which there are two types:

- _namespaces_, which can be used to help program organization;
- _style blocks_, which carry out the meat of the diagram generation.

## Namespaces

The syntax for a namespace is as follows:

```style
namespace_name {
    -- ... (the namespace body)
}
```

Refer to [this section](usage#block-body) for a detailed explanation of what may appear in the body of a namespace.

Values declared within a namespace can be read outside of the namespace using the "dot" operator:

```style
namespace_name.field_name
```

Hence they are also called _global_ variables. Overwriting these values is not allowed.

### Canvas Preamble Block

Each Style program _must_ contain a _canvas preamble block_, a special type of namespace which describes the width and height of the canvas. For example, preamble block

```style
canvas {
    width = 800
    height = 700
}
```

tells Penrose that the drawing canvas should have a width of 800 pixels and a height of 700 pixels.

## Selector Blocks

Selector Blocks are the most important component in a Style program, since they actually describe _how_ to draw elements of a diagram. The syntax for Selector Blocks is as follows:

```style
forall list_object_declarations
where list_relations
with list_object_declarations {
    ... (Selector Block Body)
}
```

where

- `list_object_declarations` is a **semicolon**-separated list of object declarations, similar to the object declarations in the Substance program. Each object declaration has syntax `type_name object_name`. The names declared in `list_object_declarations` are referred to as _style variables_.
- `list_relations` is a **semicolon**-separated list of constraints (about objects in `list_object_declaration`) that must be satisfied in order for this style block to be triggered.

The `forall`, `where`, and `with` clauses form the Selector Block Header. One might observe that both the `forall` clause and the `with` clause take in list of object declarations. The two clauses are treated equivalently in the header. Variables declared in these clauses can be accessed within the Selector Block Body.

The first clause of a Selector Block Header must be `forall`; the other clauses `where` and `with` can be written in any order. The header does not allow for empty lists - that is, `list_object_declarations` and `list_relations` in the `where` and `with` clauses must not be empty. If it is desirable to not have any elements in a clause, the entire clause must be omitted.

In the set-theory example, a style block may look like

```style
forall Set x {
}
```

or

```style
forall Set x; Set y
where IsSubset (x, y) {
}
```

or

```style
forall Set x
where IsSubst(x, y)
with Set y {
}
```

### Matching style block against substance program in general

Penrose functions by matching a Selector Block Header against a Substance program. In a nutshell, given a style block

```style
forall Set x; Set y
where IsSubset (x, y)
```

the Penrose compiler searches through the Substance program to find sets of objects consistent with `Set x; Set y` such that `IsSubset(x, y)` is satisfied. This is done through generating mappings from _style variables_ to _substance variables_, which are the objects in the Substance program.

For instance, consider a simple set-theory Substance program that works with the previous style block:

```substance
Set A, B, C
IsSubset (A, B)
IsSubset (B, C)
```

By matching the style block against the Substance program, we essentially consider six possible mappings (note that repeated elements are, by default, disallowed; see next section), some of which are valid and some are invalid:

| Mapping          | `IsSubset(x, y)` becomes | Satisfied by Substance |
| :--------------- | :----------------------- | ---------------------- |
| `x -> A; y -> B` | `IsSubset(A, B)`         | Yes                    |
| `x -> A; y -> C` | `IsSubset(A, C)`         | No                     |
| `x -> B; y -> A` | `IsSubset(B, A)`         | No                     |
| `x -> B; y -> C` | `IsSubset(B, C)`         | Yes                    |
| `x -> C; y -> A` | `IsSubset(C, A)`         | No                     |
| `x -> C; y -> B` | `IsSubset(C, B)`         | No                     |

Here, Penrose filters out mappings which do not satisfy the constraints listed in the Style block, and keeps a list of _good_ mappings (in this example, two mappings are kept). For each _good_ mapping, the body of the Style block (`list_body_expressions`) is executed, where each instance of the Style variables (`x` and `y`) is substituted with the corresponding Substance variables (once with `A` and `B`, once with `B` and `C`).

### Repeatable vs Non-Repeatable Matching

Penrose, by default, performs "non-repeatable" matching. That is, if there are two style variables declared in a Selector Block Header (in `with` or `where` clause), then the two style variables must correspond to different substance variables.

As an example, the header

```style
forall Node a; Node b
where Edge(a, b)
```

does not generate a valid matching against the substance

```substance
Node X
Edge(X, X)
```

This is a design feature because many times, we would want different visualization for self-edges like this. To override this behavior, one can add the `repeatable` keyword:

```style
forall repeatable Node a; Node b
where Edge(a, b)
```

and this header will allow `a` and `b` to both map to `X`.

### Object Declarations

In the list of object declarations in a Style block, we can declare two types of objects, which are matched differently by the Penrose compiler.

#### Substance objects

We can declare a Substance object, whose object name is surrounded by backticks. For instance,

```style
forall Set `A` {
}
```

can only be mapped to the Substance object with the exact same name (`A`) provided that the types match (subtyping allowed). In other words, given Substance program

```substance
Set A, B, C
```

matching the Style block against the Substance block yields only one valid mapping: `` `A` -> A ``.

#### Style objects

If an object name is not surrounded by backticks, then this object is a Style object with a Style variable. As seen before, the Penrose compiler will try to map Style variables to any Substance objects, provided that their types match (subtyping allowed).

### Allowed Relations

A Style block supports three types of relations, two of which can also be seen in the Substance program.

#### Predicate Applications

Just like in the Substance program, each predicate application has syntax

```substance
predicate_name (argument_list)
```

where elements of `argument_list` can refer to objects declared in `list_object_declarations`, or be other predicate applications. The types must still match, allowing subtyping.

Optionally, one can give an alias to a predicate application:

```substance
predicate_name (argument_list) as alias_name
```

If such an alias is set, then `alias_name` will be accessible in the style block body, and it will always refer to the version of the predicate application within the Substance program.

##### Symmetry

If a predicate is declared as symmetric, then it gets special treatment.
Suppose we have the following Domain schema:

```domain
type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
symmetric predicate Bond (Atom, Atom)
```

and the following style block:

```style
forall Hydrogen h; Oxygen o
where Bond (h, o) {
    ...
}
```

The style block will successfully match the following substance schema:

```substance
Hydrogen H
Oxygen O
Bond (O, H)
```

where `Bond (h, o)` in the style block matches against `Bond (O, H)` in the substance schema. Because Bond is declared symmetric, when Penrose looks for `Bond (h, o)`, it also looks for `Bond (o, h)` and finds a match. In other words, the matching algorithm handles the equivalence between `Bond (h, o)` and `Bond (o, h)` correctly.

#### Function and Constructor Applications

Each function or constructor application has syntax

```substance
object_name := function_name (argument_list)
```

We do not allow aliasing for function and constructor applications. Arguments in `argument_list` must have types that match the Domain argument types, similar to the substance schema.

#### Object Property Relations

Aside from predicate applications and function (constructor) applications, Penrose also supports a predicate-like relation that checks whether an object has a certain property, say `label`. For instance, we may write

```style
forall Set s
where s has label {
    ... some code that uses s.label
}
```

If a certain `Set A` in the Substance program does not have a label (perhaps due to `NoLabel` declarations), then `s` will not be mapped to `A`, thus preventing an access of nonexistent properties.

We can further distinguish between math labels and text labels (see [substance labeling](../substance/usage#labeling-statements)): `where p has math label` matches math labels, whereas `where p has text label` matches text labels.

### Matching Deduplication

The matching algorithm is designed to avoid duplicated mappings. If two mappings give us the same set of matched objects (in the Substance program) and the equivalent set of matched substance relations (predicate applications and function or constructor applications), then the algorithm only triggers on one of them.

For instance, say Penrose tries to match the Style block

```style
forall Set x; Set y {
}
```

against Substance program

```substance
Set A, B
```

Then, only one of mappings `x -> A; y -> B` and `x -> B; y -> A` triggers the Style block.

### Reserved Variables

Within a Style block body, some variable names are reserved for metadata purposes:

- `match_total` is an integer that refers to the number of times that this Style blocks will be triggered (or matched) in total; and
- `match_id` is the 1-indexed ordinal of this current matching.

These values can directly be read or overwritten within the style block body if needed.

## Selector Block Body

The body of a block contains declarations of variables, shapes, and the relationship between objects.

### Assignments

We can assign an expression to a field:

```style
type_annotation field = expression
```

where

- `type_annotation` is an optional field denoting the type of the variable,
- `field` is a path to the variable being assigned, and
- `expression` is the expression to be assigned to `field`.

`field` can either be

- A single identifier, which denotes a local assignment, not accessible outside of this matching; or
- An object name (defined in `list_object_declarations`) or predicate application alias, followed by a dot operator and an identifier, which denotes an assignment bound to a Substance instance of object or predicate application after we substitute in the mapping. These assignments are accessible if the same Substance object or predicate application is matched again.

For example, consider the following Style block:

```style
forall MyType t1; MyType t2
where MyPredicate (t1, t2) as r1 {
	x = -- this is a local assignment not accessible outside of this substitution or this block
	t1.a = -- this is bound to the substance instance of `MyType t1`
	r1.c =  -- this is bound to the substance instance of `MyPredicate (t1, t2)`
}
```

Refer to [this section](usage#expressions-and-their-types) for a detailed explanation of the available expressions and their associated types.

### Override and Deletion

The Style language allows users to modify fields that are previously declared. The `override` keyword changes the value of the field. As an example,

```style
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

```style
-- by default, draw a circle for all instances of type T
forall T x {
    x.widget = Circle { }
}

-- but don't draw this circle for instances of a subtype S <: T
forall S x {
    delete x.widget
}
```

Note that one must be careful not to reference deleted attributes in a later generic block. For instance, the following block will produce an error if invoked for an instance of `S`:

```style
forall T x {
    shape x.newWidget = Circle {
        center : x.widget.center -- not defined for instances of S
    }
}
```

### Constraints and Objectives

A good diagram must satisfy some basic constraints, while trying to optimize upon some objectives (specifying diagram beauty). We declare these constraints and objectives within the style blocks. A constraint declaration has syntax

```style
ensure constraint_name (argument_list)
```

and an objective declaration has syntax

```style
encourage objective_name (argument_list)
```

where `argument_list` may refer to constant values, global / local variables, and other variables bound to _substnace_ instances of objects and predicate applications. A full list of available constraints and objectives can be found [here](./functions).

We also provide syntax sugar expressions for some commonly-used objectives and constraints. In particular,

- `a > b` is the syntax sugar for the constraint / objective `greaterThan(a, b)`,
- `a == b` is the syntax sugar for the constraint / objective `equal(a, b)`, and
- `a < b` is the syntax sugar for the constraint / objective `lessThan(a, b)`.

### Layering

We can specify the layering between two shapes (particularly useful when two shapes overlap) using layering statements: either

```style
layer shape_1 above shape_2
```

or

```style
layer shape_1 below shape_2
```

where `shape_1` and `shape_2` can be variables assigned to shapes.

We have special handling of layering statements for `Group` shapes, found [here](./shapes/group.md).

## Collector Blocks

Selector blocks match Style variables against Substance variables to produce multiple matches. Each match is independent from another. This characteristic makes it difficult to implement features that require aggregations over multiple matches. For example, we can't compute the sum of `center` fields of _all_ Substance variables that a selector matches on.

Collector Blocks enables these types of aggregations by introducing collections of Substance variables. The syntax is as follows:

```style
collect <COLLECT> into <INTO>
where <WHERE>
with <WITH>
foreach <FOREACH> { }
```

The `where`, `with`, and `foreach` clauses are optional, and if they are empty, they must be omitted.

- `<COLLECT>` is an object declaration. The `<COLLECT> ` object is accessible, via the name given in the `<INTO>` clause, in the Style block body.
- `<INTO>` is the name assigned to the collection. The collection includes all the Substance objects that `<COLLECT>` matches to. Within the Style block, `<INTO>` conceptually represents a list of Substance objects.
- The `<WHERE>` clause has the same meaning as in standard `forall` Style selectors.
- `<WITH>` is a semicolon-separated list of object declarations. Objects in the `<WITH>` clause aren't collected, but may be used in `<WHERE>`.
- `<FOREACH>` is a semicolon-separated list of object declarations. Objects in the `<FOREACH>` clause are also not collected, but they arrange the `<COLLECT>` objects into groups. The entire `Collector` block runs once for each distinct match of the `<FOREACH>` clause. The `<FOREACH>` list can contain multiple declarations.

For example, suppose we have the Substance program that defines one set `s1` that contains elements `e1, e2`, and another set `s2` that contains elements `e3, e4, e5`, as follows:

```substance
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

```style
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

### Repeatable vs Non-Repeatable Matching

Collector Blocks run the same underlying matching algorithm as described [here](usage#matching-style-block-against-substance-program-in-general). As such, by default, it disallows two style variables that map to the same substance variable. To override this behavior, just as in [here](usage#repeatable-vs-non-repeatable-matching), one can add the `repeatable` keyword:

```style
collect repeatable Set s into ss
where IsSubset(s, a)
with Set a
```

### Collection Access Expression

Within the Collector Block, the name in the `<INTO>` clause conceptually means a list of Substance objects. We can access the fields of each element in the collection using the "Collection Access" expression with syntax:

```style
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

### Collection Count Expression

We may want to count the number of elements in a collection of Substance variables. This is particularly useful when computing the spacing required between two objects. Using the aforementioned `listof` expressions, we can write

```style
collect Object o into os {
    total = count(listof someprop from os)
}
```

The `count` function takes a vector of numbers and returns the number of elements in the vector. Though this method works, it relies on the assumption that the `someprop` field exists in each `Object o` and that such a field is a number. To simplify this, we support the `numberof` expression:

```style
numberof <COLLECTION NAME>
```

This expression simply returns the number of elements in `<COLLECTION NAME>`.

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

```style
shape_name {
    property_name_1 : value_1
    property_name_2 : value_2
}
```

Once declared, the value of each property can be accessed using

```style
path_to_shape.property_name
```

For example,

```style
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

The `?` expression evaluates to a scalar whose value is automatically determined by the Penrose engine. The Penrose engine picks a random initial value in place of `?` and runs an optimization process to adjust it to a more suitable value.

One can also provide an explicit initial value to guide the optimization process using a pair of square brackets next to the `?` expression. As an example, `?[3.14]` tells the Penrose engine to use the value `3.14` as a starting point and optimizing based on that `3.14` value. Values in these square brackets must be fixed and explicitly given, so `?[1 + 1]` is disallowed.

### Strings

Strings have type `string` and string literals are delimited by double quotes. Strings can be concatenated using the `+` operator. For instance, to put parentheses around the label associated of `x`, write

```style
string fancyLabel = "(" + x.label + ")"
```

### Colors

Colors have type `color`, and include an alpha (i.e., opacity) channel. Colors can be specified via two different color models:

- `rgba(r, g, b, a)` defines a color via the RGB color model, with red, green, blue, and alpha values in the range [0, 1].
  - If the required color is fixed and known, one can also use the hexadecimal representation `#rrggbbaa`, which gets converted to the corresponding color in `rgba(r, g, b, a)`. If the alpha value is not provided in the hexadecimal representation, it defaults to 1.0.
- `hsva(h, s, v, a)` defines a color via the HSV color model, with hue in the range [0, 360], saturation and value in the range [0, 100], and alpha in the range [0, 1].

To specify that a color should be omitted altogether, you can also use `none()`. E.g.,

```style
fillColor: none()
strokeColor: none()
```

Note that `none()` is different from using a 100% transparent color: it really prevents the fill or stroke from being drawn altogether.

### Computation Functions

Computation functions take in multiple values, and return a new value. The aforementioned functions `rgba`, `hsva`, and `none` are examples of computation functions. Other computation functions also exist, including many mathematical functions.

See [the list of computation functions](functions#computation-functions).

### Operations between Numerical Expressions

Aside from concatenation of strings using the operator `+`, the Style language supports standard arithmetic operations on scalars, vectors, and matrices. For instance, in the case of `scalar` variables we can use the following operators:

:::warning

Because of an internal tokenizer bug, expressions like `+1` are always parsed as one single token `+1` denoting the integer "positive one", instead of two tokens `+` and `1`, regardless of their locations in the program. Similarly, `-1` is always parsed as a single `-1` instead of `-` and `1`.

As such, expressions like `2+1` are always interpreted as `2` and `+1` instead of the expected `2`, `+`, and `1`; the same occurs for `2-1` which is interpreted as `2` and `-1` instead of the expected `2`, `-`, and `1`. In other words, they are interpreted as two numbers side-by-side instead of a number, an operator, and another number. This bug causes errors like

```error
Error: Syntax error at line 16 col 8:

    y = 2+1
         ^
Unexpected float_literal token: "+1".
```

since the `+` operator is absorbed into the token `+1` so the parser can no longer find the `+` operator.

This bug has been documented [here](https://github.com/penrose/penrose/issues/1516).

The workaround to this bug is to always put spaces around the `+` and `-` operators, writing expressions like `2 + 1`, `n - 1`, etc., unless signed numbers like `-1` and `+3` are specifically required.

:::

- `c + d` - sum of `c` and `d`
- `c - d` - difference of `c` and `d`

- `c * d` - multiplies `c` and `d`
- `c / d` - divides `c` by `d`

See the documentation on [Vectors and Matrices](vectors-matrices.md) for further information about operations on vector and matrix types.

### Style-Variable-Level Expressions

Some expressions directly take Style variables (in the header of selector or collector blocks) as arguments.

Aside from the "[collection access expression](usage#collection-access-expression)" and "[collection count expression](usage#collection-count-expression)", we currently support the `nameof` expression that takes in any Style variable, and returns the name of the Substance variable that this Style variable maps to. This can be useful for displaying errors in the Substance program on the canvas.

In the following example, we declare a set that is both empty and non-empty. This is logically inconsistent, so the Style program renders an error message onto the canvas containing the Substance name of the set that is inconsistent.

::: code-group

```domain
type Set

predicate IsEmpty(Set)
predicate IsNonEmpty(Set)
```

```substance
Set A
IsEmpty(A)
IsNonEmpty(A)
```

```style
-- canvas specifications omitted
forall Set x
where IsEmpty(x); IsNonEmpty(x) {
    errorText = Text {
        string: "Set " + (nameof x) + " is both empty and non-empty, which is inconsistent"
    }
}
```

:::

# Expressions

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

## Shapes

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

## Unknown Scalar

The `?` expression evaluates to a scalar whose value is automatically determined by the Penrose engine. The Penrose engine picks a random initial value in place of `?` and runs an optimization process to adjust it to a more suitable value.

One can also provide an explicit initial value to guide the optimization process using a pair of square brackets next to the `?` expression. As an example, `?[3.14]` tells the Penrose engine to use the value `3.14` as a starting point and optimizing based on that `3.14` value. Values in these square brackets must be fixed and explicitly given, so `?[1 + 1]` is disallowed.

## Strings

Strings have type `string` and string literals are delimited by double quotes. Strings can be concatenated using the `+` operator. For instance, to put parentheses around the label associated of `x`, write

```style
string fancyLabel = "(" + x.label + ")"
```

## Colors

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

## Computation Functions

Computation functions take in multiple values, and return a new value. The aforementioned functions `rgba`, `hsva`, and `none` are examples of computation functions. Other computation functions also exist, including many mathematical functions.

See [the list of computation functions](functions#computation-functions).

## Operations between Numerical Expressions

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

## Style-Variable-Level Expressions

Some expressions directly take Style variables (in the header of selector or collector blocks) as arguments.

Aside from the "[collection access expression](./collectors.md#collection-access-expression)" and "[collection count expression](./collectors.md#collection-count-expression)", we currently support the `nameof` expression that takes in any Style variable, and returns the name of the [Substance] variable that this Style variable maps to. This can be useful for displaying errors in the [Substance] program on the canvas.

In the following example, we declare a set that is both empty and non-empty. This is logically inconsistent, so the Style program renders an error message onto the canvas containing the [Substance] name of the set that is inconsistent.

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

[Substance]: ../substance/overview.md

# Shapes

Style currently supports a variety of shapes, listed as sub-entries under this page. In the attribute lists below, the `vec2` type describes a 2D vector `u`, whose scalar components can be accessed via `u[0]` and `u[1]`.

## Using Shapes in Style

In Style, a shape instance is declared inside a rule using a statement of the form

```style
x.myShape = Shape {
   attribute1: value1
   attribute2: value2
   -- ...
}
```

For instance, to associate all Substance objects `x` of type `Point` with a red circle of radius 5, you would write

```style
forall Point x {
   x.myCircle = Circle {
      radius: 5
      fillColor: rgba(1,0,0,1)
   }
}
```

Notice that this definition, the `center:` attribute of the circle is not set. Undefined parameters like these has default values which may or may not be adjusted upon optimization. Information about which default parameters are automatically adjusted can be found within each shape's specification.

## Keeping shapes on the canvas

All shapes have a property `ensureOnCanvas`, which by default is set to `true`. This special constraint ensures that the shape remains within the bounding rectangle of the canvas (which must be declared at the top of each Style file). This constraint can be omitted by setting `ensureOnCanvas: false`.

## Strict Typing on Shape Parameters

Penrose enforces strict types on shape parameters. For example, we require the `r` field of a `Circle` shape to be a numerical value (type `FloatV`). If one writes,

```style
myShape = Circle {
   r : true
}
```

or (by assignment or overriding)

```style
myShape = Circle {}
override myShape.r = true
```

Penrose will report an error as follows.

```error
Shape property myShape.r expects type FloatV and does not accept type BoolV.
```

The expected types of each field of each shape can be found within each shape's specification.

### Implicit Casting

For convenience, types which are similar can be casted into each other, implicitly. If type `A` implicitly casts into type `B`, then a shape field that expects value of type `A` will also accept values of type `B`. Currently, we support the following:

- `ListV` and `VectorV` are implicitly casted into each other.
- `MatrixV`, `LListV`, and `PtListV` are implicitly casted into each other.
- `TupV` can be casted into `ListV` and `VectorV`, but _not_ vice versa.

Conversion of types into strings (typed `StrV`) is not currently supported.

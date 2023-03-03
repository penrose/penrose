# Shapes

Style currently supports a variety of shapes, listed as sub-entries under this page. Note that the system is constantly evolving and this page may be out of date. Updated attributes for individual shapes can be found in the individual `Shapes.ts` files in the [`/shapes` subdirectory](https://github.com/penrose/penrose/tree/main/packages/core/src/shapes). In the attribute lists below, the `vec2` type describes a 2D vector `u`, whose scalar components can be accessed via `u[0]` and `u[1]`.

## Using shapes in Style

In Style, a shape instance is declared inside a rule using a statement of the form

```
x.myShape = Shape {
   attribute1: value1
   attribute2: value2
   ...
}
```

For instance, to associate all Substance objects `x` of type `Point` with a red circle of radius 5, you would write

```
forall Point x {
   x.myCircle = Circle {
      radius: 5
      fillColor: rgba(1,0,0,1)
   }
}
```

Notice that this definition, the `center:` attribute of the circle is not set. Undefined parameters like these will be automatically optimized by Penrose.

## Keeping shapes on the canvas

All shapes have a property `ensureOnCanvas`, which by default is set to `true`. This special constraint ensures that the shape remains within the bounding rectangle of the canvas (which must be declared at the top of each Style file). This constraint can be omitted by setting `ensureOnCanvas: false`.

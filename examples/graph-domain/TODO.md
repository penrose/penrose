# Changes to implement new feature: specify the initial value of a varying variable in Style

- add `VaryingInit(i)` to the grammar as part of `AnnoFloat`

- in `substituteBlockExpr`, substitute out occurrences of `VARYING_INIT(i)` (the computation) for `VaryingInit(i)` (the `AnnoFloat`)

[- note that this is a hack; instead of shoehorning it into `substituteBlockExpr`, it should be done more cleanly as a compiler pass on the Style block AST at some point. doesn't really matter when this is done as long as it's before the varying float initialization in `genState`]

- in `genState`, for shape properties, `VaryingInit(i)` is initialized as `i` (not sampled) and its path is included as a varying path
  - otherwise, for fields and access paths, it is initialized as the const val

# TODO

- test varying_init in properties and fields (concrete syntax: `VARYING_INIT(i)` for `i: float`) more thoroughly

- add `VARYING_INIT(i)` documentation/use to the wiki

# Docs

If you want to declare a variable as varying and initialize it at a constant float value, you can do it with `VARYING_INIT(i)` (where `i` is the float value you want). Example use:

```
Vertex V {
       V.val = VARYING_INIT(100.) // Will be initialized at 100., then optimized

       V.shape = Rectangle {
         center: (V.val, VARYING_INIT(-200.)) // Second coordinate will be initialized at -200., then optimized
       }
```

This can only be used by itself in either fields or vectors, not in expressions (e.g. `VARYING_INIT(i) + VARYING_INIT(j)` is not allowed).

# Possible issues/improvements

- better if the parser parses a special syntax for this (like `? at i`) so we don't have to do the hacky `substituteBlockExpr` replacement

- `i` can only be a constant float, not an expression

- `i` could be outside the range that the variable is usually sampled in, resulting in incorrect or unsolvable problems

- not checking existence of VARYING_INIT_FN_NAME is a hack, would be removed w/ special syntax

- `VARYING_INIT(i)` will not behave properly if used in expressions like `VARYING_INIT(i) + VARYING_INIT(j)` because we don't look for varying paths in exprs. The evaluator will encounter it and throw an error.

- not super thoroughly tested

- maybe initialize in a range with a distribution, to be more general

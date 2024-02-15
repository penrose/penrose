# Collectors

[Selectors](./selectors.md) match Style variables against [Substance] variables to produce multiple matches. Each match is independent from another. This characteristic makes it difficult to implement features that require aggregations over multiple matches. For example, we can't compute the sum of `center` fields of _all_ [Substance] variables that a selector matches on.

Collectors enable these types of aggregations by introducing collections of [Substance] variables. The syntax is as follows:

```style
collect <COLLECT> into <INTO>
where <WHERE>
with <WITH>
foreach <FOREACH> { }
```

The `where`, `with`, and `foreach` clauses are optional, and if they are empty, they must be omitted.

- `<COLLECT>` is an object declaration. The `<COLLECT> ` object is accessible, via the name given in the `<INTO>` clause, in the Style block body.
- `<INTO>` is the name assigned to the collection. The collection includes all the [Substance] objects that `<COLLECT>` matches to. Within the Style block, `<INTO>` conceptually represents a list of [Substance] objects.
- The `<WHERE>` clause has the same meaning as in standard `forall` Style selectors.
- `<WITH>` is a semicolon-separated list of object declarations. Objects in the `<WITH>` clause aren't collected, but may be used in `<WHERE>`.
- `<FOREACH>` is a semicolon-separated list of object declarations. Objects in the `<FOREACH>` clause are also not collected, but they arrange the `<COLLECT>` objects into groups. The entire `collect` block runs once for each distinct match of the `<FOREACH>` clause. The `<FOREACH>` list can contain multiple declarations.

For example, suppose we have the [Substance] program that defines one set `s1` that contains elements `e1, e2`, and another set `s2` that contains elements `e3, e4, e5`, as follows:

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

We can write a Collector:

```style
collect Element e into es
where In(e, s)
foreach Set s {
    ...
}
```

Under the above [Substance] program, this Collector would run twice:

- once with mapping `es -> [e1, e2]` and `s -> s1`
- once with mapping `es -> [e3, e4, e5]` and `s -> s2`.

Notice that we have splitted the set of all elements `[e1, e2, e3, e4, e5]` into two groups based on the `Set` object that they are contained within.

Within the body of the Collectors, we can do everything that can be done in Selector Blocks, with the one exception that we can only access Style variables in `<INTO>` and `<FOREACH>`.

## Repeatable vs Non-Repeatable Matching

Collectors run the same underlying matching algorithm as described [here](./selectors.md#matching-style-block-against-substance-program-in-general). As such, by default, it disallows two style variables that map to the same substance variable. To override this behavior, just as in [here](./selectors.md#repeatable-vs-non-repeatable-matching), one can add the `repeatable` keyword:

```style
collect repeatable Set s into ss
where IsSubset(s, a)
with Set a
```

## Collection Access Expression

Within the Collector, the name in the `<INTO>` clause conceptually means a list of [Substance] objects. We can access the fields of each element in the collection using the "Collection Access" expression with syntax:

```style
listof <FIELD NAME> from <COLLECTION NAME>
```

This expression takes the `<FIELD NAME>` field of each [Substance] variable in the collection `<COLLECTION NAME>`, and compiles these fields into an appropriate list.

For example, suppose `elements` is the name in the `<INTO>` block, conceptually representing some list of [Substance] objects `[e1, e2, ..., en]`. Then, we would expect the expression `listof field from elements` to produce a list `[e1.field, e2.field, ..., en.field]`.

Due to technical constraints, we cannot access _any_ field of each element of the collection. In the above example, if `e1.field` gives a color, then we cannot really put the color into a list, since Penrose does not support lists of colors. We restrict allowable types to as follows:

| Type of `field`    | Collects into                 |
| ------------------ | ----------------------------- |
| `FloatV` (number)  | `VectorV` (vector)            |
| `VectorV` (vector) | `MatrixV` (matrix)            |
| `ListV` (list)     | `LListV` (list of lists)      |
| `TupV` (2-tuple)   | `PtListV` (list of 2d points) |
| some shape         | `ShapeListV` (list of shapes) |

If, in the above example, `e1.field`, `e2.field`, etc. are numbers, then the expresion `listof field from elements` gives us a vector that contains all the values. We can directly plug the vector into accumulation functions such as `average` and `sum`.

## Collection Count Expression

We may want to count the number of elements in a collection of [Substance] variables. This is particularly useful when computing the spacing required between two objects. Using the aforementioned `listof` expressions, we can write

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

[Substance]: ../substance/overview.md

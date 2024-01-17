# Substance

The Substance program tells Penrose _what_ objects and relations to draw. In the set-theory example, for example, we can have the following Substance program:

```substance
Set A, B, C

IsSubset (A, C)
IsSubset (B, C)
NotIntersecting (A, B)

AutoLabel All
```

The first line declares the objects that are to be drawn, the last line tells Penrose to automatically label the objects based on their names. All other lines invoke the predicates defined in the Domain schema to declare relations between objects.

Notably, like the Domain schema, the Substance program does not contain any instructions about how, say, a `Set` must be rendered, or how the relation `IsSubset` should be reflected in the diagram. The Substance program only declares the existence of these objects and relations, whereas the [Style] program shows _how_ these objects and relations can be drawn.

A Substance program may contain two types of statements: [single statements](./statements.md) or [indexed statements](./indexed-statements.md).

## Comments

Comments are ignored by the Penrose engine. In the Substance program, comments are declared using double dashes:

```substance
-- this is a comment
```

[Style]: ../style/overview.md

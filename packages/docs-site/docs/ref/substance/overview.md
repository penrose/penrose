# Substance

The _substance_ program tells Penrose _what_ objects and relations to draw. In the set-theory example, for example, we can have the following _substance_ program:

```substance
Set A, B, C

IsSubset (A, C)
IsSubset (B, C)
NotIntersecting (A, B)

AutoLabel All
```

The first line declares the objects that are to be drawn, the last line tells Penrose to automatically label the objects based on their names. All other lines invoke the predicates defined in the _domain_ schema to declare relations between objects.

Notably, like the _domain_ schema, the _substance_ program does not contain any instructions about how, say, a `Set` must be rendered, or how the relation `IsSubset` should be reflected in the diagram. The _substance_ program only declares the existence of these objects and relations, whereas the _style_ program shows _how_ these objects and relations can be drawn.

A _substance_ program may contain two types of statements: [single statements](./statements.md) or [indexed statements](./indexed-statements.md).

## Comments

Comments are ignored by the Penrose engine. In the _substance_ program, comments are declared using double dashes:

```substance
-- this is a comment
```

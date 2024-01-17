# Domain

A _domain_ schema describes the types of objects, as well as relations between these objects, that Penrose diagrams work with. For example, say we want to use Penrose to draw a Venn diagram that represents the relationship between mathematical sets. We can declare the following domain schema:

```domain
type Set

predicate NotIntersecting(Set s1, Set s2)
predicate Intersecting (Set s1, Set s2)
predicate IsSubset (Set s1, Set s2)
```

This schema tells Penrose that the diagrams works with and illustrates _Set_ objects and relations such as _NotIntersecting_, _Intersecting_ and _IsSubset_.

Notably, the _domain_ schema are not _instructions_ of how to draw the _Set_ objects and relations between them. Such instructions of the specific diagram elements are provided by the _style_ schema, not the _domain_ schema. This allows multiple visual representations to be applied to objects from the same domain.

There are four types of statements that can appear in the _domain_ schema: [Type](./types.md), [Predicate](./predicates.md), [Function, and Constructor](./functions.md) declarations.

## Comments

_Comments_ are lines in the schema that are ignored by the Penrose parser. They are for documentation purposes, and are not involved in the generation of the diagrams. In the _domain_ schema, they start with two dashes:

```domain
-- this is a comment
```

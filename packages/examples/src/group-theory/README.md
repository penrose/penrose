# Group theory

This example is stub for visualizations of groups from abstract algebra. A group is specified in a Substance file via its multiplication table, and perhaps other metadata (like its generators). See `Group.dsl` for basic type definitions, and the `groups` subdirectory for example Substance files. Currently, two visual representations are provided for groups:

- `MultiplicationTable.sty` displays the [multiplication table](https://en.wikipedia.org/wiki/Cayley_table)
- `CayleyGraph.sty` draws a group as a [Cayley graph](https://en.wikipedia.org/wiki/Cayley_graph).  This Style assumes the group generators have also been specified in the Substance program, via the `IsGenerator` predicate.  Tagging the identity element with `IsIdentity` will also highlight this element.

The Cayley graph style can also be used to visualize the orbits induced by (right-)multiplication by any group element `g`, by adding the predicate `IsGenerator(g)` to the Substance file (and removing any other `IsGenerator` predicates).  From these examples, it should be easy to build other standard styles / visual representations used for groups.

Users with access to Mathematica or similar computer algebra packages may find it (much) easier to synthesize input Substance programs from existing descriptions, rather than keying in the multiplication table by hand.  To this end, a _Mathematica_ notebook `GroupToSubstance.nb` is provided that can synthesize an input Substance program from Mathematica's large existing library of groups.


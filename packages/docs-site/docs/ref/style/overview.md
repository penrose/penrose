# Style

Given a Domain schema (specifying the domain of the diagram), a [Substance] file specifies how the objects and relationships in any given [Substance] program should get translated into graphical icons and geometric relationships on a 2D canvas.

A Style program is composed of _blocks_, of which there are three types:

- [Namespaces] are usaully used to specify constants such as [canvas dimensions][canvas-dimensions], colors, and other constant values within the diagram.
- [Selector] blocks match on [Substance] statements and specifies shapes and diagram layout.
- [Collector] blocks are similar to [Selector], except that it aggreggates the match results into collections.

[Substance]: ../substance/overview.md
[Namespaces]: ./namespaces.md
[Selector]: ./selectors.md
[Collector]: ./collectors.md
[canvas-dimensions]: ./namespaces.md#canvas-preamble-block

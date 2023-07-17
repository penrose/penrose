# Overview

## Why write a program instead of dragging shapes around?

There are plenty of visual design tools. While popular, these tools tend to lack composability, generality, and reusability. Creators spend a large amount of time moving shapes around trying to get everything "just right." The more complex the diagram, the longer it takes to make. And for the next diagram, they get to do the same work all over again.

Graphical specification also demands that you already know how to visualize a particular abstract idea, and it ties mathematical content to one specific visual representation.

Penrose, instead, provides the level of abstraction needed to separate _content_ from _representation_, which is extremely powerful.

Penrose already understands the diagram's domain and how to style the diagram. You just define the objects and relationships. Penrose goes to work by converting the three programs into an optimization problem that it solves using symbolic differentiation. If you need to add a new shape, it's not a painstaking exercise -- Penrose automatically creates a new diagram that meets all of your constraints.

## Why "Penrose"?

We named Penrose after Sir Roger Penrose, who, in addition to possessing a euphonious name, is known for his Escher-inspired illustrations of impossible objects, Penrose notations, and Penrose diagrams. "Pen" and "rose" also encapsulate our goal of turning notations ✍️ to beautiful diagrams 🌹.

## The Penrose trio

A diagram made in Penrose involves a _trio_ of three programs:

- A **Domain (.domain) program** describes for a given domain the types of objects, predicates, and functions that comprise diagrams in this domain. These will be referred to in the Substance and Style programs, which are domain-specific.
- A **Substance (.substance) program** defines the _objects_ and _relationships_ in the diagram. You define and name which domain objects will be in the diagram and use the predicates and functions defined in Domain to define the relationships among the objects. At this point, you aren't too concerned with how to draw the diagram: that is the job of the Style program.
- A **Style (.style) program** tells Penrose how to display the objects and relationships.

Often, you only need to write the Substance program. To create a new diagram, you simply create a short and simple Substance program that describes the domain objects and their relationships. Often these are so short, you may not even realize you are writing a program. Here's an example:

```substance
Set A, B, C, D, E
AutoLabel All
```

The beauty of Penrose is you get to "stand atop giants" by using the existing Style and Domain programs for many common domains. You only need to modify a Style or Domain program if you want to add or change the way the domain is displayed (Style) or if you need to define new relationships or domain object types (Domain). Check out our [gallery](/examples) for domains that are supported out of the box, but you can create your own, too. Our [Tutorial](/docs/tutorial/welcome) teaches you how to work with all three languages.

## Language references

The language reference pages define the syntax and introduce language features for Domain, Substance, and Style:

- [Domain language reference](/docs/ref/domain/usage)
- [Substance language reference](/docs/ref/substance/usage)
- [Style language reference](/docs/ref/style/usage)
  - For documentation on our library of constraints, objectives and functions, refer to the [Function Library](/docs/ref/style/functions) page.

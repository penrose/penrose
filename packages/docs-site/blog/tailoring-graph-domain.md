---
title: "Tailoring Penrose domains to your needs"
date: 2023-07-27
authors:
  - author: Rijul Jain
    github: "rjainrjain"
blog: true
---

<script setup>

import hamiltonianCycle from "@penrose/examples/dist/graph-domain/other-examples/hamiltonian-cycle.trio.js";
import { defineComponent, ref } from "vue";
import StagedDiagram from "../src/components/StagedDiagram.vue";
import DemoWrapper from "../src/components/DemoWrapper.vue";
import Embed from "../src/components/Embed.vue";

const highlighted = {
  trio: {
    substance: hamiltonianCycle.substance,
    style: hamiltonianCycle.style[0].contents,
    domain: hamiltonianCycle.domain,
    variation: hamiltonianCycle.variation,
    //animate: false,
  },
  imageResolver: hamiltonianCycle.style[0].resolver,
}

const unhighlightedSubstance = "Vertex a, b, c, d, e, f, g\n\n-- Hamiltonian cycle\nArc(a, b)\nArc(b, c)\nArc(c, d)\nArc(d, e)\nArc(e, f)\nArc(f, g)\nArc(g, a)\n\n-- Additional arcs that are not in the Hamiltonian cycle\nArc(a, c)\nArc(a, d)\nArc(b, e)\nArc(b, f)\nArc(c, g)\nArc(d, f)\n\nAutoLabel All";

const unhighlighted = {
  trio: {
    substance: unhighlightedSubstance,
    style: hamiltonianCycle.style[0].contents,
    domain: hamiltonianCycle.domain,
    variation: hamiltonianCycle.variation,
    //animate: false,
  },
  imageResolver: hamiltonianCycle.style[0].resolver,
}

</script>

<BlogMeta  />

Or, a tour of a simple domain for directed graphs and how to use Penrose's Domain and Style languages to extend it.

---

To facilitate creating diagrams quickly, Penrose has many existing Domain and Style programs, which you may want to tailor to fit your own diagramming needs. We walk through an example of such modification by extending the `simple-directed-graph` domain to support highlighting specific vertices and arcs, as seen in the difference between the diagrams below.

<div style="display: grid;   grid-template-columns: repeat(auto-fit, minmax(320px, 1fr)); grid-gap: 20px">
<Embed :trio=unhighlighted.trio  />
<Embed :trio=highlighted.trio />
</div>

---

## Directed graph domain: just vertices and arcs

The simplest Domain schema that describes a directed graph, composed of vertices and directed edges (or, arcs) can be expressed in the following manner:

```domain
type Vertex
predicate Arc(Vertex a, Vertex b)
```

Given a `Vertex` type and an `Arc` predicate, we can now create vertices and draw arcs between them in the Substance language. Let's write a Substance program in this domain; one, for example, depicting an intricate Hamiltonian graph. The Substance code could be written as follows:

```substance
Vertex a, b, c, d, e, f, g

-- Hamiltonian cycle
Arc(a, b)
Arc(b, c)
Arc(c, d)
Arc(d, e)
Arc(e, f)
Arc(f, g)
Arc(g, a)

-- Additional edges that are not in the Hamiltonian cycle
Arc(a, c)
Arc(a, d)
Arc(b, e)
Arc(b, f)
Arc(c, g)
Arc(d, f)
```

Here's what the resulting diagram should look like:

<div style="display: flex; justify-content: center">
  <div style="width: 100%; max-width: 320px">
    <Embed :trio=unhighlighted.trio />
  </div>
</div>

## Extending the Domain and understanding the Style

In the graph shown above, it's rather unclear where exactly the Hamiltonian circuit is, so we might want to highlight its path. This would mean highlighting certain vertices and arcs. Let's therefore add predicates to the Domain which allow us to do so.

```domain
predicate HighlightVertex(Vertex a)
predicate HighlightArc(Vertex a, Vertex b)
```

This addition defines two new predicates which take existing vertices and (vertices which form) arcs and highlights them. Now we need to describe how to display this relation in a diagram – in this case, highlighting would involve changing the color of the vertices and nodes. We can inspect the Style program to identify which parts we need to add on to or otherwise modify. The following excerpt of the `simple-directed-graph` Style program, with some sections omitted for clarity, focuses on the relevant aspects:

```style
forall Vertex v {
  v.dot = Circle {
--  [...]
    fillColor : color.black
  }

  v.text = Text {
--  [...]
    fillColor: color.black
--  [...]
  }
--[...]
}
```

For a vertex, we need to change the `fillColor` of both the actual `dot` and the `text`.

And the relevant portion of the Style program for arcs is below:

```style
forall Vertex u; Vertex v where Arc(u, v) as e {

--  [...]

  e.arrow = Path {
--  [...]
    strokeColor: color.black
  }

--  [...]

  e.pointer = Path {
--  [...]
    fillColor: color.black
  }

--  [...]

}
```

We need to modify the `strokeColor` of the `arrow` and the `fillColor` of the `pointer`.

## Modifying the Style

Given this knowledge, we can modify the Style program to fit our needs. First, we may find it useful to define the new color signifying a highlighted vertex or arc, so we add `redOrange` to the `color` object in the Style program:

```style
color {
  black = #000000
  white = #ffffff
  redOrange = #FE4A49 -- added for highlighting
}
```

Then, we add more constraints that account for `HighlightVertex` and `HighlightArc`, as shown below.

```style
forall Vertex v where HighlightVertex(v) {
    override v.dot.fillColor = color.redOrange
    override v.text.fillColor = color.redOrange
}

forall Vertex a, b where Arc(a,b) as e; HighlightArc(a,b) {
    override e.arrow.strokeColor = color.redOrange
    override e.pointer.fillColor = color.redOrange
}
```

Where there is a `Vertex` and a call to the `HighlightVertex` predicate, we override the `fillColor` of its components; we perform the corresponding extensions for `Arc` and `HighlightArc`.

## The final outcome!

Now we can add to the Substance program to display the highlighting we wanted:

```substance
-- Highlight start/end vertex of cycle
HighlightVertex(a)

-- Highlight all arcs in cycle
HighlightArc(a, b)
HighlightArc(b, c)
HighlightArc(c, d)
HighlightArc(d, e)
HighlightArc(e, f)
HighlightArc(f, g)
HighlightArc(g, a)
```

Ultimately, the highlighted diagram looks like this:

<div style="display: flex; justify-content: center">
  <div style="width: 100%; max-width: 320px">
    <Embed :trio=highlighted.trio />
  </div>
</div>

In this process, we've modified the `simple-directed-graph` Domain and Style programs to add new capabilities. You can see this in action in the [Hamiltonian graph](https://penrose.cs.cmu.edu/try/?examples=graph-domain/other-examples/hamiltonian-cycle) example in the Penrose gallery. We hope this helps and inspires you to modify the Domain and Style programs of Penrose's existing domains to fit your own diagramming needs!

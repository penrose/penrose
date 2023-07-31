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
type Vertex
predicate Arc(Vertex a, Vertex b)
predicate HighlightVertex(Vertex a) // [!code focus]
predicate HighlightArc(Vertex a, Vertex b) // [!code focus]
```

This addition defines two new predicates which take existing vertices and (vertices which form) arcs and highlights them. Now we need to describe how to display this relation in a diagram – in this case, highlighting would involve changing the color of the vertices and nodes. We can inspect the Style program to identify which parts we need to add on to or otherwise modify. The following excerpt of the `simple-directed-graph` Style program:

```style
forall Vertex v {
  v.dot = Circle { // [!code focus]
    center: (? in dots, ? in dots)
    r: num.radius
    fillColor : color.black // [!code focus]
  } // [!code focus]

  v.text = Text { // [!code focus]
    string: v.label
    fillColor: color.black // [!code focus]
    fontFamily: "serif"
    fontSize: "18px"
    strokeColor: color.white
    strokeWidth: 4
    paintOrder: "stroke"
  } // [!code focus]
  v.halfSize = (v.text.width / 2, v.text.height / 2)
  v.bottomLeft = v.text.center - v.halfSize
  v.topRight = v.text.center + v.halfSize

  v.text above v.dot

  encourage shapeDistance(v.dot, v.text) == num.labelDist in text
}
```

For a vertex, we need to change the `fillColor` of both the actual `dot` and the `text`.

And the relevant portion of the Style program for arcs is below:

```style
forall Vertex u; Vertex v where Arc(u, v) as e {
  a = u.dot.center
  b = v.dot.center
  t = normalize(b - a) -- tangent
  n = rot90(t) -- normal
  m = (a + b) / 2 -- midpoint

  e.start = a
  e.end = b
  e.offset = ? in dots
  e.arrow = Path { // [!code focus]
    d: quadraticCurveFromPoints("open", [a, m + e.offset * n, b])
    strokeColor: color.black // [!code focus]
  } // [!code focus]

  e.step = ? in arrows
  e.pointerCenter = m + (e.offset / 2) * n + e.step * t
  p = e.pointerCenter
  x = num.pointerX
  y = num.pointerY
  e.pointer = Path { // [!code focus]
    d: pathFromPoints("closed", [p - x * t + y * n, p + x * t, p - x * t - y * n])
    strokeColor: none()
    fillColor: color.black // [!code focus]
  } // [!code focus]

  e.arrow below u.dot
  e.arrow below v.dot
  e.pointer below e.arrow

  encourage vdist(u.dot.center, v.dot.center) < num.edgeDist in dots
  encourage minimal(sqr(e.offset)) in dots
  encourage minimal(sqr(e.step))
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

Then, we match on the specific cases of `Vertex` and `Arc` with predicates `HighlightVertex` and `HighlightArc`, as shown below.

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

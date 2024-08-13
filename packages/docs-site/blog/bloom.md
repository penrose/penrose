---
title: "Bloom"
date: 2024-08-16
authors:
  - author: Griffin Teller
    github: "griffinteller"
  - author: Wode Ni
    github: "wodeni"
  - author: Sam Estep
    github: "samestep"
---

<script setup>
import { applyReactInVue } from "veaury"
import EigenReact from "../src/bloom-examples/eigen.tsx";
import CirclesReact from "../src/bloom-examples/circles.tsx";
import RaysReact from "../src/bloom-examples/rays.tsx";
import ReflectionReact from "../src/bloom-examples/reflection.tsx";
import CirclePackingReact from "../src/bloom-examples/circle_packing.tsx";
const Eigen = applyReactInVue(EigenReact);
const Circles = applyReactInVue(CirclesReact);
const RaysComponent = applyReactInVue(RaysReact);
const Reflection = applyReactInVue(ReflectionReact);
const CirclePacking = applyReactInVue(CirclePackingReact);
</script>

<BlogMeta />

Optimization-driven interactive diagramming, directly in JavaScript

_powered by Penrose_

---

We are excited to announce Bloom, an open-source JavaScript library for interactive diagram creation.
Bloom builds on top of Penrose to provide the same declarative language, while adding new constructs for interaction
and site integration.

<Eigen/>

### Overview

Interactive diagrams are a powerful tool for teaching, learning, and
exploring. But creating them is often a time-consuming and ad-hoc process,
requiring endless event-handler juggling or even a small engine around
each diagram.

Bloom provides an alternative: first, describe your diagram using a
library of shape declarations and differentiable constraints, objectives,
and operations. Then, Bloom compiles it into efficient WebAssembly, and
optimizes the diagram to satisfy your constraints live during user
interaction.

Below is a short Bloom program drawing a couple of circles. Try
uncommenting the <code>disjoint</code> constraint, and watch what happens!

<Circles />

### Optimization-Driven

Much of the language we use to describe diagrams involves either
constraints we would like to satisfy, or objectives we would like to
maximize. For example, we might specify that two polygons must be
“touching”, or that two circles are “near” to each other. Penrose lets the user describe
their diagrams using an extensive library of these descriptions, and then
optimizes the diagram to satisfy them. Bloom builds on this, allowing you
to describe these diagrams directly in JavaScript, and continuously
optimizing while the user interacts with objects in your scene.

In the following, we would like a ray to bounce specularly off the
mirror, no matter where the user drags each endpoint. We _could_
calculate the _exact_ intersection such that the $y$ coordinate of the
ray is reversed, but why not let the optimizer do the work?

<Reflection />

Here’s another example: to prevent objects from overlapping as we drag
them around, you would typically need to specify a complex interaction of
forces, update rules, and collision detection. With Bloom, you can let the
optimizer handle this with the <code>disjoint</code> constraint:

<div style="display: flex; flex-direction: row; justify-content: space-evenly; height: 30em">
<CirclePacking />
</div>

### Declarative

Diagramming in Bloom closely mirrors the process in Penrose:

1. Define the types of objects (“substances”) and predicates between
   objects you’re going to use:

```typescript
const Line = type();
const Point = type();
const IntersectAt = predicate();
```

2. Instantiate these substances and predicates according to what should
   show up in your diagram:

```typescript
const line1 = Line();
const line2 = Line();
const intersection = Point();

IntersectAt(line1, line2, intersection);
```

3. Define styling rules for substances, groups of them, and predicates:

```typescript
forall({ l: Line }, ({ l }) => {
  l.icon = line();
}

forall({ p: Point }, ({ p }) => {
  p.icon = circle({
    r: 5,
    fillColor: [0, 0, 0, 1],
  });
});

forallWhere(
  { l1: Line, l2: Line, p: Point },
  ({ l1, l2, p }) => IntersectAt.test(l1, l2, p),
  ({ l1, l2, p }) => {
  ensure(constraints.collinearOrdered(
    l1.icon.start, p.icon.center, l1.icon.end
  ));
  ensure(constraints.collinearOrdered(
    l2.icon.start, p.icon.center, l2.icon.end
  ));
});
```

Unlike in Penrose, there is no enforced separation between these steps.
The whole world of JavaScript is yours to exploit! You can conditionally
define substances, use loops in styling selectors, and much more.

We recommend sticking to the process above, however : )

### React Integration

Bloom integrates tightly with React, providing rendering components and
custom hooks to streamline diagram development. Compiled Bloom diagrams
also provide a programmatic API for live site integration:

<RaysComponent />

### Getting started

That's just the beginning: check out our docs for an extended tutorial. We're excited to se what you build!

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
import Eigen from "../src/bloom-examples/Eigen.vue";
import Reflection from "../src/bloom-examples/Reflection.vue";
import Circles from "../src/bloom-examples/Circles.vue";
import CirclePackingDisjoint from "../src/bloom-examples/CirclePackingDisjoint.vue";
import CirclePackingPadded from "../src/bloom-examples/CirclePackingPadded.vue";
import CirclePackingEqual from "../src/bloom-examples/CirclePackingEqual.vue";
import Rays from "../src/bloom-examples/Rays.vue";
import Pool from "../src/bloom-examples/Pool.vue";
</script>

[//]: # '<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.css" integrity="sha384-NFTC4wvyQKLwuJ8Ez9AvPNBv8zcC2XaQzXSMvtORKw28BdJbB2QE8Ka+OyrIHcQJ" crossorigin="anonymous">'

<BlogMeta />

[//]: # "# Bloom: Interactive, Optimization-Driven Diagramming in JavaScript"

<br/>

We're excited to announce Bloom, an open-source JavaScript library for optimization-driven interactive
diagram creation. Bloom makes it simple to describe complex, dynamic behavior using a rich vocabulary of optimization constraints,
and the declarative language behind Penrose. Lets checkout some examples!

## Examples

Below, we've placed some balls in a circle. Try dragging one around to
see how the others respond:

<div style="width: 70%; height: 25em; margin-left: auto; margin-right: auto;">
<CirclePackingDisjoint />
</div>

Notice how the circles push each other around? This doesn't require any collision detection, event handler set up,
or physics engine to implement. Instead, you only need to specify that the balls shouldn't overlap!

```ts
forall({ c1: Circle, c2: Circle }, ({ c1, c2 }) => {
  ensure(constraints.disjoint(c1.icon, c2.icon));
});
```

In the next example, we want a ray to bounce specularly off a mirror&mdash;that is, the ray should hit the mirror and exit the
mirror with exactly the same angle of elevation. Try dragging the start and endpoints around, and watch how this property
is maintained:

<Reflection />

How might you implement this? With Bloom, there's no need to calculate the exact point at which the reflection is specular.
Instead, just say what you mean: that the angle of incidence should equal the angle of reflection:

```ts
const r1y = ray1.normVec[1];
const r2y = ray2.normVec[1];
ensure(constraints.equal(r1y, mul(-1, r2y)));
```

Bloom also provides a tight integration with React. This next diagram displays

- two vectors, $a_1$ and $a_2$
- a point $v_1$
- a point $Av_1$, connected to $v_1$ by a dotted line

Try dragging the gray handles to see how the transformation changes, along with the LaTeX on the right.

<Eigen />

If you're familiar with a little linear algebra, you'll notice that the vectors $a_1$ and $a_2$ form the columns of
matrix $A$, which is applied to $v_1$ to from $Av_1$. The eigenspaces of $A$ are shown as lines $s_1$ and $s_2$.
All of this data, calculated and stored internal to the diagram, is easily synced to the LaTeX on the right using
Bloom's system of shared diagram values and custom hooks.

## Optimization-Driven Diagramming

We believe that the most natural way to express complex interactive behavior is through optimization. Let's take another
look at our first example:

<div style="width: 70%; height: 25em; margin-left: auto; margin-right: auto;">
<CirclePackingDisjoint />
</div>

The elements of this diagram include:

- A circular enclosure
- A set of 10 draggable circles, which cannot overlap and cannot exit the enclosure

As we saw before, ensuring that the circles do not overlap is as simple as specifying a single constraint for each pair of circles:

```ts
forall({ c1: Circle, c2: Circle }, ({ c1, c2 }) => {
  ensure(constraints.disjoint(c1.icon, c2.icon));
});
```

Without optimization, implementing this behavior would be a nasty challenge. At minimum, you'd need to:

- Set up event handlers to translate circles on drag
- Detect collisions between circles
- Implement a physics engine to resolve collisions continuously

Moreover, the simplicity of the constraint-based approach allows for easy modification. Suppose we instead wanted instead
for these balls to repel each other within a certain padding:

<div style="width: 70%; height: 25em; margin-left: auto; margin-right: auto;">
<CirclePackingPadded />
</div>

```ts
forall({ c1: Circle, c2: Circle }, ({ c1, c2 }) => {
  ensure(constraints.disjoint(c1.icon, c2.icon, 20));
});
```

Or even that they should all touch the enclosure:

<div style="width: 70%; height: 25em; margin-left: auto; margin-right: auto;">
<CirclePackingEqual/>
</div>

```ts
forall({ c: Circle }, ({ c }) => {
  ensure(
    constraints.equal(
      ops.vnorm(c.icon.center),
      sub(enclosure.icon.r, circleRad),
    ),
  );
});

forall({ c1: Circle, c2: Circle }, ({ c1, c2 }) => {
  ensure(constraints.disjoint(c1.icon, c2.icon, 20));
});
```

Neither change requires a substantial rewrite of of the diagram's behavior; instead, we can simply modify the constraints to reflect
our new requirements.

## Getting Started

Bloom is still in the early stages of development, but we're excited to share it with you. If you're interested in learning more,
you can take a look at our [docs] (**Note to us: we need to replace this with a real link when we deploy**). We're excited to see what you build!

<Rays />

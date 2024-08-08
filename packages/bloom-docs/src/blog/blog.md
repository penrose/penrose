# Bloom

_Declarative interactive diagramming, directly in JavaScript_

_powered by Penrose_

---

We are excited to announce Bloom, an open-source TypeScript library for interactive diagram creation.
Bloom uses simple, readable constructs and differentiable programming to generate
beautiful and extensible diagrams. Try dragging the circles below!

-- circles diagram --

### Declarative

Bloom encourages you to describe your diagram's underlying objects, relationships, and interactions
before worrying about styling. Try adding a new circle below:

```typescript
const Circle = type();

const circle1 = Circle();
const circle2 = Circle();
const circle3 = Circle();
```

`Circle` is a new `type`, or class or diagram objects, and calling `Circle()` instantiates a `substance` of type `Circle`.
To actually draw these substances, Bloom provides simple selectors:

```typescript
forall({ b: Ball }, ({ b }) => {
  b.icon = circle({
    r: 10,
    fillColor: [1, 0, 0, 1],
    drag: true,
  });
});
```

### Optimization-Driven

Let's make sure our circles can't overlap:

```typescript
forall({ c1: Circle, c2: Circle }, ({ c1, c2 }) => {
  ensure(constraints.disjoint(c1.icon, c2.icon));
});
```

That's all there is to it! You can explore Penrose's large collection of optimization goals [here](https://penrose.cs.cmu.edu/docs/ref/style/functions).

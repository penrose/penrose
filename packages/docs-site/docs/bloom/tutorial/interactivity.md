# Interactivity

---

### Dragging

You can make a shape draggable by setting `drag: true` on a compatible shape:

```ts
ellipse({
  drag: true,
});
```

A shape can be marked as drag if:

- The shape type is `circle`, `ellipse`, `rectangle`, `image`, `text`, or `equation`, and
  - Both `center[0]` and `center[1]` are inputs
- The shape type is `line` and
  - All coordinates of `start` and `end` are inputs
- The shape type is `polygon` or `polyline` and
  - All coordinates of all points in points are inputs

The coordinates that must be inputs are exactly those that would be translated by dragging; if they weren’t inputs,
there would be no value for Bloom to translate! This can be inconvenient, however. Imagine we have two squares that are
always 50 pixels apart horizontally, but both should be draggable:

```ts
const s1 = rectangle({
  width: 25,
  height: 25,
  drag: true,
});

// not actually draggable!
const s2 = rectangle({
  width: 25,
  height: 25,
  center: [add(s1.center[0], 50), s1.center[1]],
  drag: true,
});
```

If you were to build this, you would find that `s2` could not be dragged because of the call to add. An easy workaround
is to make both `center`s have their own inputs (the default), and add optimizer constraints to maintain the 50 pixels
separation:

```ts
const s1 = rectangle({
  width: 25,
  height: 25,
  drag: true,
});

const s2 = rectangle({
  width: 25,
  height: 25,
  drag: true,
});

ensure(constraints.equal(add(s1.center[0], 50), s2.center[0]));
ensure(constraints.equal(s1.center[1], s2.center[1]));
```

The `bindToInput` method creates syntactic sugar for this pattern. The following builds exactly the same diagram:

```ts
const s1 = rectangle({
  width: 25,
  height: 25,
  drag: true,
});

const s2 = rectangle({
  width: 25,
  height: 25,
  center: [bindToInput(add(s1.center[0], 50)), bindToInput(s1.center[1])],
  drag: true,
});
```

### Drag Constraints

It’s common to want to constrain a draggable object to a subset of the canvas. Common examples include dragging a point
along a line, or an object constrained within a box. Try dragging the point below around the circle:

– mini planets example –

This is fundamentally a different kind of constraint than the ones we gave to the optimizer; where before we were
only specifying what kinds of diagrams the optimizer should converge to, now we want to specify where the user
is allowed to set a shape’s position. Additionally, when the user does drag their mouse outside of the legal region,
we need some way to define where the shape should actually go&mdash;a kind of projection. Bloom allows you express
both of these behaviors with a `dragConstraint`:

```ts
const orbitRad = 200;

const c1 = circle({
  r: 5,
  drag: true,
  dragConstraint: ([x, y]) => {
    const norm = Math.sqrt(x * x + y * y);
    return [(x / norm) * orbitRad, (y / norm) * orbitRad];
  },
});

ensure(constraints.equal(ops.vnorm(c1.center), orbitRad));
```

`dragConstraint` is a function mapping the mouse position to the desired position of the shape. In our case,
whenever the length of the vector from the origin to the mouse is not equal to `orbitRad` (i.e., we have dragged off
the circle), we scale the vector from the origin to the mouse to give us a parallel point on the circle.

[//]: # "– mini planets with arrows from center to circle –"

You might notice we also included an optimizer constraint with `ensure`: without this, the initial position of the c
ircle, before the user has dragged it, might not be on the circle (since this position is determined by the optimizer).

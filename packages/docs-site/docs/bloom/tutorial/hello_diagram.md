<script setup>
import HelloDiagram from "../../../src/bloom-examples/HelloDiagram.vue";
</script>

# Hello, Diagram

---

This first chapter will give a high level overview of the process of diagramming and rendering.
Here’s what we’re working towards:

<HelloDiagram />

(Try dragging the circles around!)

### Building the Diagram

Let's pick up where we left off:

```javascript
// main.js

import * as bloom from "https://penrose.cs.cmu.edu/bloom.min.js";

const db = new bloom.DiagramBuilder(bloom.canvas(400, 400), "abcd", 1);

// Diagramming goes here!

const diagram = await db.build();
```

The first step in using Bloom is to create a
`DiagramBuilder` object. This object contains methods allowing you to declare types, substances, style selectors,
shapes, constraints, and everything else you need to build your diagram. It takes up to three arguments:

- `canvas`: a `Canvas` object specifying the local coordinate system (and thus the aspect ratio) of your diagram.
- `variation`: an optional `string` providing a seed for random sampling
- `lassoStrength`: an optional `number` (default 0) specifying the strength with which the diagram should encourage
  continuity. If you notice your diagram acting 'jumpy', you might consider increasing this value.

When you're done building your diagram, await `.build()` to get the diagram object.

You might find it convenient to destructure the methods of `DiagramBuilder` and a few from `bloom`:

```javascript
const { type, predicate, forall, forallWhere, ensure, circle, line } = db;
```

Destructuring the `DiagramBuilder` methods isn't strictly necessary, but for the remainder of
these tutorials we assume that every method we need has been destructured. We do recommend keeping a reference to the
original `db` variable like above, in case you want to pass it to a helper function.

On to the diagramming! Our diagram needs to have two circles, and an arrow connecting them. While we could declare those
shapes right away, Bloom encourages you to first declare the abstract objects in your scene, and define rules to style
those objects. This leads to better reusability, better readability, and often fewer bugs.

```javascript
const Point = type();
const Arrow = type();
const Connects = predicate();

const p1 = Point();
const p2 = Point();
const arrow = Arrow();
Connects(arrow, p1, p2);
```

This code declares to Bloom that we have two kinds of objects in our scene: `Point`s and `Arrow`s. We also have a possible
relationship `Connects`. We then instantiate two points into two substances, `p1` and `p2`, an arrow `arrow`, and
specify that `arrow` connects `p1` to `p2`. Predicates are untyped, so we could have put any objects in any order into
`Connects`; we just need to make sure we’re consistent when we style this relationship later.

The final step is to ‘style’ these constructs:

```javascript
const pointRad = 30;
const pointMargin = 10;

forall({ p: Point }, ({ p }) => {
  p.icon = circle({
    r: pointRad,
    drag: true,
  });
});

forallWhere(
  { a: Arrow, p: Point, q: Point },
  ({ a, p, q }) => Connects.test(a, p, q),
  ({ a, p, q }) => {
    const pq = bloom.ops.vsub(q.icon.center, p.icon.center); // vector from p to q
    const pqNorm = bloom.ops.vnormalize(pq); // direction from p to q
    const pStart = bloom.ops.vmul(pointRad + pointMargin, pqNorm); // vector from p to line start
    const start = bloom.ops.vadd(p.icon.center, pStart); // line start
    const end = bloom.ops.vsub(q.icon.center, pStart); // line end

    a.icon = line({
      start: start,
      end: end,
      endArrowhead: "straight",
    });

    ensure(
      bloom.constraints.greaterThan(
        bloom.ops.vdist(p.icon.center, q.icon.center),
        2 * (pointRad + pointMargin) + 20,
      ),
    );
  },
);
```

Let’s go through this piece by piece.

```ts
forall({ p : Point }, ({ p }) => {
```

The `forall` method takes in a ‘selector’ of substances: in the first case, we’re selecting over all individual `Point`s
and naming them `p` (via `{ p : Point }`). We then pass a function taking in an ‘assignment’ to this selection, which in
this case we expect to run on every Point, and from there we’re free to style however we like. In this case, we create
a circle of radius pointRad, and store it in a field of the point we call `p`.

```ts
p.icon = circle({
  r: pointRad,
  drag: true,
});
```

`icon` is not a special name in any way; we could have named it `shape` or circle or anything we like. In fact, simply
for the purposes of drawing the circle, we don’t even need to store it. The following would still draw a circle for
every point:

```ts
forall({ p: Point }, ({ p }) => {
  circle({
    r: pointRad,
    drag: true,
  });
});
```

However, storing the circle in p.icon allows us to access the circle in future selections. Also note that there are
lots more fields of circle we didn’t fill in, such as `center`, `fillColor`, etc. All shape fields are optional in Bloom,
and if left out either have a default value or are randomly sampled. You can check out these defaults (and the available
shapes) in the [Penrose documentation](/docs/ref/style/shapes-overview). These fields also correspond closely with the SVG spec.

```ts
forallWhere(
  { a: Arrow, p: Point, q: Point },
  ({ a, p, q }) => Connects.test(a, p, q),
  ({ a, p, q }) => {
```

`forallWhere` selects over substances just like `forall`, except only assignments which satisfy a boolean predicate
are passed to your styling function. In this case, we test whether we previously declared that `a`, `p`, `q` have the
relationship `Connects`. We declared this for exactly one such triple, so we can expect our styling function to run
only once with `a === arrow`, `p === p1`, and `q === p2`.

```ts
const pq = bloom.ops.vsub(q.icon.center, p.icon.center); // vector from p to q
const pqNorm = bloom.ops.vnormalize(pq); // direction from p to q
const pStart = bloom.ops.vmul(pointRad + pointMargin, pqNorm); // vector from p to line start
const start = bloom.ops.vadd(p.icon.center, pStart); // line start
const end = bloom.ops.vsub(q.icon.center, pStart); // line end
```

If you refer back to the diagram at the top of this article, you can see we want our arrow to lie on the line
connecting the centers of `p` and `q`, but with a padding between the circle and the endpoints of the arrow. Finding the
start and endpoint of this arrow requires a little linear algebra:

- Take the vector connecting the two centers (`pq`), and normalize it to get the direction vector (`pqNorm`)
- Multiply the direction vector by `pointRad` + `pointMargin` to get the vector from `p` to the start of the arrow `pStart`.
- Add `pStart` to `p` gets the start of the arrow, and subtract `pStart` from `q` get the end of the arrow.

All of these vector operations are available in the `ops` dictionary exported by Bloom.

Drawing the arrow is now self-explanatory:

```ts
a.icon = line({
  start: start,
  end: end,
  endArrowhead: "straight",
});
```

The final aspect to consider is that we don’t want the user to drag the points too close to each other, or else the
arrow disappears. We can tell the optimizer to ensure this is the case by demanding that the distance between the
points is greater than the sum of their radii and padding:

```ts
ensure(
  bloom.constraints.greaterThan(
    bloom.ops.vdist(p.icon.center, q.icon.center),
    2 * (pointRad + pointMargin) + 20,
  ),
);
```

We’ll talk more about the optimizer and constraints in the next chapter.

### Displaying the Diagram

To display your diagram, we should first create a `div` element for the diagram in `index.html`:

```html
!-- index.html -->

<body>
  <div
    id="diagram-container"
    style="width: 50em; height: 50em; margin: auto; border: 3px solid black"
  ></div>
  <script type="module" src="main.js"></script>
</body>
```

The diagram will expand to fill its containing block, so you can adjust the `width` and `height` to fit your needs.
Now we can get an interactive element from the diagram and append it to the container:

```javascript
// main.js

const diagram = await db.build();

const interactiveElement = diagram.getInteractiveElement();
document.getElementById("diagram-container").appendChild(interactiveElement);
```

If you open `index.html` in your browser, you should be able to see and interact with the diagram!

<script setup>
import HelloDiagram from "../../../src/bloom-examples/HelloDiagram.vue";
</script>

# Hello, Diagram

---

This first chapter will give a high level overview of the process of diagramming and rendering.
Here’s what we’re working towards:

<HelloDiagram />

### Building the Diagram

First, create a new component `MyDiagram.tsx` in the `src/` directory.

You’ll almost certainly want to wrap the process of building your diagram it’s own function, which we’ll call later
when we render the diagram:

```ts
const buildMyDiagram = async () => {
  const db = new DiagramBuilder(canvas(400, 200), "abcd", 1);

  const { type, predicate, circle, line, build, forall, forallWhere, ensure } =
    db;

  // diagramming goes here!

  return await build();
};
```

The first step in using Bloom is to create a
`DiagramBuilder` object. This object contains methods allowing you to declare types, substances, style selectors,
shapes, constraints, and everything else you need to build your diagram. It takes in three arguments:

- `canvas`: a `Canvas` object specifying the local coordinate system (and thus the aspect ratio) of your diagram.
- `variation`: a `string` providing a seed for random sampling
- `lassoStrength`: an optional `number` (default 0) specifying the strength with which the diagram should encourage
  continuity. If you notice your diagram acting 'jumpy', you might consider increasing this value.

When we're done, we call `.build()` to get the diagram object.

It’s not strictly necessary to destructure the `DiagramBuilder` methods, but it’s convenient, and for the remainder of
these tutorials we assume that every method we need has been destructured. We do recommend keeping a reference to the
original `db` variable like above, in case you want to pass it to a helper function.

On to the diagramming! Our diagram needs to have two circles, and an arrow connecting them. While we could declare those
shapes right away, Bloom encourages you to first declare the abstract objects in your scene, and define rules to style
those objects. This leads to better reusability, better readability, and often fewer bugs.

```ts
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

```ts
const pointRad = 50;
const pointMargin = 10;

forall({ p : Point }, ({ p }) => {
  p.icon = circle({
  r: pointRad,
  )};
});

forallWhere(
  { a: Arrow, p: Point, q: Point },
  ({ a, p, q }) => Connects.test(a, p, q),
  ({ a, p, q }) => {

  const pq = ops.vsub(q.icon.center, p.icon.center); // vector from p to q
  const pqNorm = ops.vnormalize(pq); // direction from p to q
  const pStart = ops.vmul(pointRad + pointMargin, pqNorm); // vector from p to line start
  const start = ops.vadd(p.icon.center, pStart); // line start
  const end = ops.vsub(q.icon.center, pStart);  // line end

  a.icon = line({
    start,
    end,
    endArrowhead: "straight",
  });

  ensure(constraints.greaterThan(
    ops.vdist(p.icon.center, q.icon.center),
    2 * (pointRad + pointMargin)
  );
});
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
)};
```

`icon` is not a special name in any way; we could have named it `shape` or circle or anything we like. In fact, simply
for the purposes of drawing the circle, we don’t even need to store it. The following would still draw a circle for
every point:

```ts
forall({ p : Point }, ({ p }) => {
  circle({
    r: pointRad,
    drag: true,
  )};`
});
```

However, storing the circle in p.icon allows us to access the circle in future selections. Also note that there are
lots more fields of circle we didn’t fill in, such as `center`, `fillColor`, etc. All shape fields are optional in Bloom,
and if left out either have a default value or are randomly sampled. You can check out these defaults (and the available
shapes) in the Penrose documentation. These fields also correspond closely with the SVG spec.

```ts
forallWhere(
{ a: Arrow, p: Point, q: Point },
({ a, p, q }) => Connects.test(a, p, q),
({ a, p, q }) => {
```

`forallWhere` selects over substances just like forall, except only assignments which satisfy a boolean predicate
are passed to your styling function. In this case, we test whether we previously declared that `a`, `p`, `q` have the
relationship `Connects`. We declared this for exactly one such triple, so we can expect our styling function to run
only once with `a === arrow`, `p === p1`, and `q === p2`.

```ts
const pq = ops.vsub(q.icon.center, p.icon.center); // vector from p to q
const pqNorm = ops.vnormalize(pq); // direction from p to q
const pStart = ops.vmul(pointRad + pointMargin, pqNorm); // vector from p to line start
const start = ops.vadd(p.icon.center, pStart); // line start
const end = ops.vsub(q.icon.center, pStart); // line end
```

If you refer back to the diagram at the top of this article, you can see we want our arrow to lie on the line
connecting the centers of `p` and `q`, but with a padding between the circle and the endpoints of the arrow.
So we take the vector connecting them (`pq`), normalize it to get the unit length direction (`pqNorm`), multiply it by
`pointRad` + `pointMargin` to get the vector from `p` to the start of the arrow `pStart`. Adding `pStart` to `p` gets us the start
of the arrow, and subtracting `pStart` from `q` gets us the end. All of these vector operations are part of the Penrose API,
in the exported `ops` dictionary.

Drawing the arrow is now self-explanatory:

```ts
a.icon = line({
  start,
  end,
  endArrowhead: "straight",
});
```

The final aspect to consider is that we don’t want the user to drag the points too close to each other, or else the
arrow disappears. We can tell the optimizer to ensure this is the case by demanding that the distance between the
points is greater than the sum of their radii and padding:

```ts
ensure(constraints.greaterThan(
  ops.vdist(p.icon.center, q.icon.center),
  2 * (pointRad + pointMargin)
);
```

We’ll talk more about the optimizer and constraints in the next chapter.

### Rendering the Diagram

To render your diagram in React, you'll need to create a new component that uses the `useDiagram` hook to build your
diagram and the `Renderer` component to display it. Here's an example:

```ts
export const MyDiagramComponent = () => {
  const diagram = useDiagram(buildMyDiagram);
  return <Renderer diagram={diagram} />;
};
```

`useDiagram` runs your building function and stores the built diagram as a React state (it also sets up some callbacks
or live site integration, so simply memoizing your diagram won’t always do what you expect).
`Renderer` starts a render loop and sets up interaction callbacks for your diagram.

You can use this component wherever you would like in your React app, but for the purposes of this tutorial,
you might edit `App.tsx` to render your diagram directly:

```ts
import { MyDiagramComponent } from "./MyDiagram.js";

const App = () => {
  return <MyDiagramComponent />;
};
```

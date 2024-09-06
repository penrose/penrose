<script setup>
import ProceduralDiagram from "../../../src/bloom-examples/ProceduralDiagram.vue";
</script>

# Procedural Diagramming and Optimization

---

### Inputs, Vars, and Nums

Imagine we’re drawing two circles, the second of which has twice the radius of the first:

```ts
const c1 = circle({}); // radius `r` (and other fields) randomly sampled
// `{}` may be omitted
const c2 = circle({
  r: c1.r * 2,
});
```

Looks good. But when we go to typecheck this, we get an error:

```
TS2362: The left-hand side of an arithmetic operation must be of type 'any', 'number' or an enum type.
```

Doh! What’s happening here? The error message seems to suggest that `c1.r` is not a number–but isn’t it? To explain this,
we first have to talk about how Bloom handles interactivity and optimization.

Building a diagram defines a procedure to render your diagram: draw a circle with these settings, then do some math,
then draw a line, etc. When you call `DiagramBuilder.prototype.build`, it compiles this procedure into WebAssembly which takes
in a collection of `inputs`, and outputs a diagram.

[//]: # "<ProceduralDiagram />"

This provides a clean interface to edit your diagram: just vary the inputs! When you first render your diagram, they
are filled with random values. When you drag a shape, the inputs corresponding to the center are changed and the diagram
is re-rendered. When you ensure that two shapes are touching, the optimizer tries to adjust the inputs to satisfy your
request.

This brings us back to the problem with `c1.r * 2`: when you’re building your diagram, not every value is known.
Some fields are filled in with default numbers (like `circle.strokeWidth`, which is by default `0`), but most are filled in
with type `Var`, which represent future inputs to the diagram to be optimized and interacted with at render time.

Most of the math that you’ll do in Bloom will operate on `Num`s, which is a data type including

- `number`s (e.g. `5`, `67 + 4.8`)
- `Var`s (e.g. `c1.r`)
- Results of operations on other `Num`s (e.g. `add(5, c1.r)`)

Both Bloom and Penrose provide a whole host of operations on `Num`s, which you can find in our
<a href="/bloom-docs/index.html" target="_blank">reference</a>.

### Creating your own inputs

While inputs are created every time a shape field is randomly sampled, you can also create your own with the input method:

```ts
const myVar = input();
```

Why might you want to do this? One common instance is when you want a shape field to have one element be randomly
selected and optimized, but another element constant:

```ts
rectangle({
  width: 100,
  height: 50,
  center: [input(), 0],
});
```

This rectangle will always have `y === 0`, but its x-coordinate will be sampled (and optimized). You might also need
to explicitly declare inputs if you want specific settings:

```ts
const myVar = input({ name: "myVar", init: 42, optimized: false });
```

`name` allows the input to be changed at render-time with `Diagram.prototype.setInput`, or retrieved with
`Diagram.prototype.getInput` (though we highly recommend using `SharedInput` instead, which we’ll cover in the
next chapter). Specifying `init` disables random sampling, and sets the input (at least initially) to the specified
value. If `optimized` is set to `false`, then the input will not be changed by the optimizer to try and satisfy
constraints; this is particularly useful when you want an object to stay where you dragged it. All three fields are
optional, as well as the entire settings object.

```ts
// this circle can be dragged, but may be moved by the optimizer after the user releases it
circle({
  drag: true,
  center: [input(), input()], // the default, not necessary to write out
});

// this circle will be "pinned" where the user drops it
circle({
  drag: true,
  center: [input({ optimized: false }), input({ optimized: false })],
});
```

<script setup>
import Eigen from "../../../src/bloom-examples/Eigen.vue";
</script>

# React Integration

---

<Eigen />

In the above example, we have a React component rendering a Bloom diagram on the left, and some Mathjax on the right.
Somehow, we need to sync values internal to the diagram to mathjax, live during rendering. Bloom provides several
solutions. As a reminder, React integration is available through the `@penrose/bloom` NPM package.

### Basics

### Named Inputs

When you create inputs, you can optionally name them:

```ts
const a1 = [input({ name: "a1.x" }), input({ name: "a1.y" })];
```

This name must be unique across all inputs in the diagram. Naming inputs gives you the option of getting and
setting them at render time:

```ts
const diagram = useDiagram(buildEigenvectorsExample);

const a1x = diagram.getInput("a1.x");
const a1y = diagram.getInput("a1.y");

// render mathjax ...
```

What happens when `a1` is dragged? As written, this will not force a re-render by React, since get is only run on
render! One solution is to store the retrieved values in a state and use Bloom’s effect system:

```ts
const diagram = useDiagram(buildEigenvectorsExample);
const [a1x, setA1x] = useState(0);
const [a1y, setA1y] = useState(0);

useEffect(() => {
  diagram.addEffect("a1.x", setA1x);
  diagram.addEffect("a1.y", setA1y);
}, [diagram]);
```

This works fine, but for many purposes `SharedInputs` are simpler to use, which we'll explore next.

### Shared Inputs

SharedInputs remove much of the boilerplate associated with querying and setting inputs directly.
At a high level, shared inputs are objects which DiagramBuilders can instantiate into regular old inputs,
but which can be get and set from anywhere in your program. Moreover, they provide a simple method for syncing
values between two or more diagrams.

A typical usage pattern might look as follows:

```ts
const buildMyDiagram = async (mySharedInput) => {
  // .. diagram things
  const myInput = sharedInput(mySharedInput);
  // ... more diagram things
};

const MyDiagramComponent = () => {
  const mySharedInput = useSharedInput();
  const diagram = useDiagram(
    useCallback(() => buildMyDiagram(mySharedInput), [mySharedInput]),
  );

  const myValue = mySharedInput.get();

  // render something with `myValue` ...
};
```

(Here, useCallback is necessary to prevent the anonymous function wrapping `buildMyDiagram` from changing every
render, and therefore triggering `useDiagram` to rebuild the diagram every render);

A couple things are going on here:

- The method `sharedInput` of `DiagramBuilder` takes a `SharedInput` and creates an otherwise normal diagram input, which
  can be used exactly like calling `input()`.
- `useSharedInput` creates a new `SharedInput`, and triggers a re-render whenever it is changed (due to optimization, dragging, or setting).
  `SharedInput.prototype.get` retrieves the current value of the input.

The inputs can be passed into multiple diagrams, in which case the value of the shared input will be synced
between the diagrams.

# Penrose Style → Bloom JSX Migration Guide

This guide is written for coding agents porting Penrose trio programs
(`.domain` / `.substance` / `.style`) to Bloom JSX (`.tsx`).

---

## 1. Lazy vs. Eager Semantics

**Penrose Style is lazy.** Every field access and arithmetic expression in a
`.style` file is an expression tree that the optimizer evaluates at render time.
Writing `n.icon.center` does not read a concrete value — it emits a symbolic
reference to whatever the optimizer assigns `n.icon.center` to.

**Bloom JSX is eager.** Shapes are real JavaScript objects created *immediately*
when JSX is evaluated. Field access returns the current value (often a `Var`
node, not a number). This has two practical consequences:

### 1a. Shape creation happens at call time

```tsx
// ✅ Correct — shape is created inside forall where builder is active
forall({ n: Node }, ({ n }) => {
  n.icon = <circle r={40} />;
});

// ❌ Wrong — JSX runs outside builder context; throws at runtime
const shared = <circle r={40} />;
forall({ n: Node }, ({ n }) => {
  n.icon = shared; // same object reused across all n — optimizer sees only one shape
});
```

### 1b. Read shape fields only after the forall that assigns them

```tsx
const n = Node();
forall({ n: Node }, ({ n }) => {
  n.icon = <circle r={40} />;
});

// ✅ Safe — n.icon is set; n.icon.center is a Vec2 of Var nodes
encourage(objectives.notTooClose(n.icon, someOtherShape));

// ❌ Wrong — n.icon is undefined before forall runs
encourage(objectives.notTooClose(n.icon, someOtherShape));
forall({ n: Node }, ({ n }) => { n.icon = <circle r={40} />; });
```

---

## 2. Use Rose Operators When Working with Optimizer Variables

Bloom's `Num` type is `number | Var | Unary | Binary | ...` — a plain TypeScript
union, **not a class with overloaded operators**. Whether a shape field is a
`number` or a `Var` (optimizer variable) depends on how it was set:

- **Explicitly set fields** stay as the type you provide. If you wrote
  `<circle r={42} />` or `circle({ center: [100, 200] })`, those fields hold
  plain `number`s and JS arithmetic on them works fine.
- **Unspecified fields** are filled by `sampleShape` and become `Var` nodes —
  the optimizer varies these. JS arithmetic on `Var` objects produces `NaN` or
  nonsense because `Var` is a plain object, not a number.

**In practice:** use JS operators freely on values you set to concrete constants.
Use the Penrose math API when you need to compute something from an
optimizer-controlled value (e.g., placing a label relative to an auto-placed
circle center):

```tsx
// ✅ Fine — both values are concrete numbers you set explicitly
const cx = 100, cy = 200;
<circle center={[cx + 50, cy]} r={40} />

// ✅ Fine — using shape fields that you explicitly set as numbers
const n = Node();
n.icon = circle({ center: [100, 200], r: 40 });
const labelY = n.icon.center[1] - 20; // center[1] is 200 (a number)

// ❌ Wrong — r was not set, so it's a Var; JS * gives NaN
forall({ n: Node }, ({ n }) => {
  n.icon = <circle />; // r is auto-sampled → Var
  n.label = <circle r={n.icon.r * 2} />; // NaN!
});

// ✅ Correct — use mul() to combine optimizer variables
import { mul, ops } from "@penrose/core";
forall({ n: Node }, ({ n }) => {
  n.icon = <circle />;
  n.label = <circle r={mul(n.icon.r, 2)} />;
});
```

Key Penrose math API:

| Operation | ❌ JS (on Var) | ✅ Penrose API |
|-----------|----------------|---------------|
| scalar add | `a + b` | `add(a, b)` |
| scalar multiply | `a * b` | `mul(a, b)` |
| scalar subtract | `a - b` | `sub(a, b)` |
| scalar negate | `-a` | `neg(a)` |
| vector add | `u.map((x,i) => x + v[i])` | `ops.vadd(u, v)` |
| vector scale | `u.map(x => x * s)` | `ops.vmul(s, u)` |
| vector dot product | `u.reduce(...)` | `ops.vdot(u, v)` |
| vector norm | `Math.sqrt(...)` | `ops.vnorm(u)` |
| absolute value | `Math.abs(x)` | `abs(x)` |
| square root | `Math.sqrt(x)` | `sqrt(x)` |

Import from `@penrose/core`:

```tsx
import { add, mul, sub, neg, abs, sqrt, ops } from "@penrose/core";
```

Comparison operators (`<`, `>`, `===`) on `Var` values compare object identity,
not numerical value. For constraint/objective purposes use `constraints.*` and
`objectives.*` from `@penrose/bloom`.

---

## 3. Replace Penrose Style Hacks with Real JS Loops and Conditionals

Penrose Style has no loops or `if` statements. Style programmers work around
this with multi-selector `forall` and predicate-guarded blocks. In Bloom JSX
these restrictions don't exist — write ordinary JavaScript.

### 3a. Loops

```tsx
// Penrose Style (awkward workaround — requires a "chain" predicate)
// forall { n: Node; m: Node where Linked(n, m) } { ... }

// Bloom JSX — plain loop over pairs
const nodes = Array.from({ length: 10 }, () => Node());
for (let i = 0; i < nodes.length - 1; i++) {
  const a = nodes[i], b = nodes[i + 1];
  forall({ a: Node, b: Node }, ({ a, b }) => {
    a.edge = <line start={a.icon.center} end={b.icon.center} />;
  });
}
```

### 3b. Conditionals

```tsx
// Penrose Style (no real if — use predicate selectors)
// forall { n: Node where Highlighted(n) } { n.icon.fill-color = ... }

// Bloom JSX — use a JS if
const nodes = Array.from({ length: 5 }, () => Node());
nodes.forEach((n, i) => {
  forall({ n: Node }, ({ n }) => {
    n.icon = (
      <circle
        r={30}
        fill-color={i === 2 ? [1, 0.5, 0, 1] : [0.3, 0.3, 0.8, 1]}
      />
    );
  });
});
```

### 3c. Computed props (replace Style arithmetic on layout values)

```tsx
// Penrose Style
// n.icon.center[0] = canvas.width / 2  (symbolic — computed by optimizer)

// Bloom JSX
// Use JS to compute fixed values, or let the optimizer place them
const cx = 200; // concrete pixel value
forall({ n: Node }, ({ n }) => {
  n.icon = <circle r={40} center={[cx, db.makeInput({ init: 200, name: "cy" })]} />;
});
```

---

## 4. Use the Registry Variation Seed

Every entry in `packages/examples/src/registry.json` can carry a `variation`
field — a string that seeds the optimizer's random initialization. When
replicating a diagram from a trio, pass the same seed so that the starting
layout matches:

```json
// registry.json
"bloom/my-example": {
  "trio": false,
  "tsx": true,
  "name": "My Example (Bloom JSX)",
  "variation": "XY7k3mNp"
}
```

Read the seed from the registry entry and pass it to `DiagramBuilder`:

```tsx
/** @jsxImportSource @penrose/bloom */
import { canvas, DiagramBuilder } from "@penrose/bloom";

// The variation string is injected by the test harness / gallery runner.
// Accept it as a parameter so the diagram is reproducible.
export default async (variation = "XY7k3mNp"): Promise<string> => {
  const db = new DiagramBuilder(canvas(400, 400), variation);
  // ...
  const diagram = await db.build();
  const { svg } = await diagram.render();
  return svg;
};
```

When the corresponding trio `.trio.json` contains `"variation": "..."`, copy
that exact string. When no trio exists, pick any string and record it in the
registry so the layout is stable across test runs.

If you don't pass a variation (or pass an empty string), the optimizer starts
from a random seed every run, making snapshot tests non-deterministic.

---

## Quick Checklist

- [ ] `/** @jsxImportSource @penrose/bloom */` at the top of every `.tsx` file
- [ ] All shape JSX inside `forall` callbacks (or immediately after `new DiagramBuilder`)
- [ ] No JS arithmetic (`+`, `-`, `*`) on `Num` values — use `add`, `mul`, `ops.*`
- [ ] Loops and conditionals written in JS, not simulated with multi-selector foralls
- [ ] `variation` string passed to `DiagramBuilder` and recorded in `registry.json`
- [ ] Export a default `async (variation?: string): Promise<string>` function that returns the SVG string

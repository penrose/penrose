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

## 2. Unspecified Fields Are Optimized — Use Penrose Operators on Them

### What gets optimized

In Penrose Style, `?` means "the optimizer decides this value." Every shape field
that is not explicitly set in Bloom has the same semantics — it is auto-sampled
as a `Var` (optimizer variable) and will be varied to satisfy constraints and
objectives.

```tsx
// <circle /> — ALL positional fields (center, r) are Var: optimizer places them
// <circle r={40} /> — r is fixed at 40; center is still Var
// <circle r={40} center={[100, 200]} /> — both fixed; nothing is optimized
```

**Prefer leaving layout fields unspecified** and expressing intent through
`ensure`/`encourage`. Let the optimizer do the work.

### JS operators work on concrete numbers, not on Var

Bloom's `Num` type is `number | Var | Unary | Binary | ...` — a plain TypeScript
union, **not a class with overloaded operators**. Fields you set explicitly hold
whatever type you gave (plain `number` for literals). Unspecified fields hold
`Var` objects — and JS `+`, `-`, `*`, `/` on a `Var` produce `NaN` silently.

```tsx
// ✅ Fine — cx/cy are plain JS numbers; arithmetic stays concrete
const cx = 100, cy = 200;
<circle center={[cx + 50, cy]} r={40} />

// ❌ Wrong — n.icon.r is a Var (not set); JS * gives NaN
forall({ n: Node }, ({ n }) => {
  n.icon = <circle />;               // r → Var (optimizer decides)
  n.halo = <circle r={n.icon.r * 2} />; // NaN
});

// ✅ Correct — use Penrose API to combine optimizer variables
import { mul, ops } from "@penrose/core";
forall({ n: Node }, ({ n }) => {
  n.icon = <circle />;
  n.halo = <circle r={mul(n.icon.r, 2)} />;
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

### Which fields are sampled by default

Not all unset fields behave the same. Some have fixed defaults (e.g., stroke
properties), while others are **randomly sampled** — including `fillColor` on
the most common shapes. The full per-shape table is in
`packages/docs-site/docs/ref/style/shapes/`. The sampled fields (those that
vary with the random seed) are:

| Shape | Sampled fields |
|-------|---------------|
| Circle | `center`, `r`, `fillColor` |
| Ellipse | `center`, `rx`, `ry`, `fillColor` |
| Rectangle | `center`, `width`, `height`, `fillColor` |
| Line | `start`, `end` |
| Image | `center`, `width`, `height` |
| Polygon | `fillColor` |
| Text | `center` |
| Equation | `center` |
| Path / Polyline / Group | *(none — all fields have fixed defaults)* |

`strokeColor` has a fixed default on every shape and is **not** sampled.

**Implication for translation:** if the source style sets `fillColor` on a
circle, you should replicate it; leaving it unset will produce a different
(randomly-colored) diagram on every seed. If the source intentionally omits
fill color and lets it be sampled, leaving it unset is correct.

### Avoid inventing magic constants

When translating a trio, **do not introduce numeric constants that are not in the
source**. Arbitrary numbers like `center={[150, 0]}` or `r={45}` anchor shapes
to fixed pixel positions, defeating the optimizer.

If the source style uses a magic number (e.g., a padding constant like `20.0`),
treat it one of two ways:

1. **It's a layout hint the optimizer can replace** — drop it and express the
   intent as a constraint/objective instead. For example, `labelPadding = 10`
   in Style becomes `ensure(constraints.contains(icon, label, 10))` in Bloom,
   with the optimizer free to satisfy it.

2. **It's a genuine fixed value** (canvas size, number of items, a coordinate
   system constant) — use it verbatim as a JS constant, not as a shape field
   that should be optimized.

When in doubt, ask: *"Would changing this number change what diagram is
described, or just where it ends up on the canvas?"* If the latter, leave it
out and let the optimizer decide.

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
- [ ] Layout fields left unspecified where possible — let the optimizer place them
- [ ] No JS arithmetic on optimizer variables (`Var`); use `add`, `mul`, `ops.*` instead
- [ ] No magic constants invented during translation — only numbers present in the source
- [ ] Source magic numbers evaluated: use as constraint padding or drop in favour of the optimizer
- [ ] Loops and conditionals written in JS, not simulated with multi-selector foralls
- [ ] `variation` string passed to `DiagramBuilder` and recorded in `registry.json`
- [ ] Export a default `async (variation?: string): Promise<string>` function that returns the SVG string

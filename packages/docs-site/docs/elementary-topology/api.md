---
title: Reusable Domain, Substance, and TSX Style Programs
description: Define independent mathematical programs and reusable Penrose styles in TypeScript.
---

# Reusable TSX programs

The TypeScript API keeps the three Penrose programs separate. A **domain**
declares the mathematical vocabulary. A **substance** creates objects and asserts
facts. A **style** gives those objects shapes and constraints. Reuse one substance
with different styles, or one style with different substances.

## Declare a domain

```ts
// sets.ts
import { domain } from "@penrose/bloom";

const d = domain("sets");
const Set = d.type("Set");
const Point = d.type("Point");
const Subset = d.predicate("Subset", [Set, Set]);
const Member = d.predicate("Member", [Point, Set]);
export const sets = d.make({ Set, Point, Subset, Member });
```

Declarations are independent of a diagram builder. TypeScript checks predicate
argument types; runtime checks also protect JavaScript callers. Subtypes are
declared with `d.type("OpenSet", Set)` and appear in selectors for their parents.
Use `declareSetTheory(d)` from `@penrose/bloom/domains/set-theory` to compose the
shared set vocabulary into a larger domain.

Types can carry mathematical data:

```ts
const Point = d
  .type("Point")
  .withData<{ coordinate: readonly [number, number] }>();
const MarkedPoint = d.type("MarkedPoint", Point).withData<{ note: string }>();
const Function = d.type("Function").withData<{
  source: EntityOf<typeof Set>;
  target: EntityOf<typeof Set>;
}>();
```

Import `EntityOf` from `@penrose/bloom`. Metadata is copied and frozen when an
entity is constructed. References to existing objects in the same substance keep
their semantic identity. References from another substance are rejected.

## Write a substance program

```ts
// example.ts
import { sets } from "./sets";

const s = sets.substance();
const A = s.Set({ label: "A" });
const B = s.Set({ label: "B" });
const x = s.Point({ label: "x" });
s.Subset(A, B);
s.Member(x, A);
export const sub = s.make();
```

`make()` closes an immutable snapshot. It contains mathematical objects and
propositions, with no shapes or builder state. Predicate calls assert facts and
deduplicate identical propositions. For nested propositions, use
`s.Subset.expression(A, B)` to form an expression without asserting it, then
pass it to a predicate declared with the `proposition` argument marker.
Predicates have no automatic logical inference.

## Reuse a TSX style

The library's Euler/Venn style turns set and membership facts into Penrose
constraints. The mathematical domain exports the same interface for every
substance program:

```ts
import { canvas, diagram, eulerVennStyle, setTheory } from "@penrose/bloom";

const s = setTheory.substance();
const A = s.Set({ label: "A" });
const B = s.Set({ label: "B" });
s.Subset(A, B);

const drawing = await diagram({
  sub: s.make(),
  sty: eulerVennStyle(),
  canvas: canvas(400, 300),
  variation: "nested-sets",
});
while (await drawing.optimizationStep()) {}
const { svg } = await drawing.render();
document.body.append(svg);
```

To write a style, add `/** @jsxImportSource @penrose/bloom */` to a `.tsx` module.
The style below captures its mathematical domain and defines reusable visual
behavior:

```tsx
/** @jsxImportSource @penrose/bloom */
import type { Circle, Equation } from "@penrose/bloom";
import { sets } from "./sets";

export const pointStyle = sets.style((ctx) => {
  const visual = ctx.view(sets.Point, (point) => ({
    dot: (<circle r={3} fill="black" ensure-on-canvas />) as Circle,
    label: (<equation>{point.label}</equation>) as Equation,
  }));
  ctx.forall({ point: sets.Point }, ({ point }) => {
    // Access the shapes associated with this mathematical point.
    const { dot, label } = visual.get(point);
    ctx.layer(dot, label);
  });
});
```

Style callbacks and TSX components are synchronous. Shapes are created eagerly
inside the active, scoped builder. Each `diagram()` assembly creates fresh visual
views, so parallel builds do not mutate a shared substance. `sty` can be an array
of compatible styles applied in order. Set the canvas in the assembly, or in a
style's options; conflicting style canvases require an explicit assembly canvas.

Use `ctx.facts(predicate)` to iterate directed facts, including reflexive facts;
`ctx.test(predicate, ...args)` checks an assertion. `ctx.forall` and
`ctx.forallWhere` use ordered assignments of distinct objects. `ctx.view` gives
typed per-object shape data without adding visual properties to the substance.

## Mathematical modules

The modules currently implemented include:

| Domain             | Visual policies                                                                                                                         | Mathematical instances                                                                                                            |
| ------------------ | --------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| Set theory         | Euler/Venn circles, subset, membership, intersection, disjointness                                                                      | Arbitrary supported set relationships                                                                                             |
| Metric spaces      | Neighborhoods and containment, metric comparison, interval complements, sequence tails, function collars, continuity and limits         | Chapter 2; configurable metrics, points, radii and sequences                                                                      |
| Point-set topology | Derived sets, bases and subbases, relative topologies, quotient and product spaces, separation, extension, nets and covering properties | Chapters 3–7 and the new subspace and coordinate-slice illustrations; shared mathematical declarations and reusable view policies |

Import domain and style modules directly through `@penrose/bloom/domains/*` and
`@penrose/bloom/styles/*`, or use their named root exports. Mathematical example
factories are available through `@penrose/bloom/examples/*`.

## Reuse a book program for another illustration

The two torus figures reuse the same mathematical factory and style. A compact
product with both factor fibers requires additional facts, including an ordered
pair distinct from its factor point:

```ts
import { canvas, diagram } from "@penrose/bloom";
import { circleProductSubstance } from "@penrose/bloom/examples/quotient-spaces";
import { circleProductStyle } from "@penrose/bloom/styles/quotient-spaces";

const sub = circleProductSubstance(0.8, {
  fixedLabel: "p",
  bothFibers: true,
  compact: true,
});
const drawing = await diagram({
  sub,
  sty: circleProductStyle(),
  canvas: canvas(216, 134),
  variation: "another-circle-product",
  interactive: { jitter: 0 },
});
while (await drawing.optimizationStep()) {}
const { svg } = await drawing.render();
document.body.append(svg);
```

The Substance contains circles, singleton factors, product sets, topologies and
their relationships. Radii, camera projection, shading and label positions belong
to the style. The paired-fiber view uses the book's schematic fiber glyphs; it
preserves their marked incidence while allowing any fixed circle coordinate.

The [further illustrations](./further-illustrations) also reuse the coordinate
embedding factory from Figure 4.10 with a nonzero fixed coordinate and the other
coordinate varying. Their source panels show the actual generic declarations and
the arguments used to instantiate them.

## Native interactive layouts

`diagram({ interactive: { jitter: 0 }, ... })` retains the canonical source
arrangement while enabling native annotation handles. `interactive: true`, or
`interactive: { jitter: 4 }`, samples a small annotation displacement using the
diagram's `variation` seed. Construct a new diagram with another seed to re-sample;
construct it with zero jitter to restore the canonical arrangement.

The book reader reuses the site's Bloom `Renderer` and `useDiagram` widgets.
Dragging and keyboard arrow adjustments update Penrose inputs and run its
optimizer. A label and its white knockout move together. Figure 4.5 demonstrates
a grouped construction handle: its disk, hatching and center point move together.
Mathematical Substance snapshots remain immutable during interaction.

Metric neighborhoods retain the book's `N(x, ρ)` convention and strict
`D(x,y) < ρ` inequality. `planeDistance` and `inNeighborhood` expose the same
metric mathematics used by the styles. The function collar uses vertical
differences in the uniform metric; it does not restrict the domain to continuous
functions. A function sampler supplies illustrative geometry when no formula is
specified in the source.

## Shape coordinates and SVG

Penrose's `center`, `start`, `end`, and point arrays use a centered canvas with
upward-positive y. Native SVG `cx`/`cy`, line endpoint props, and rectangle
`x`/`y` use top-left, downward-positive coordinates and are converted into
optimizer geometry. SVG paint values such as named, hexadecimal, and RGB colors
are parsed into Penrose colors. Gradient references remain SVG attributes.

Use Penrose `PathData` arrays for paths and vector arrays for polygon points.
Unsupported native coordinate transformations or relative units produce explicit
errors. Draggable coordinates may each be affine expressions of one input; fixed,
nonlinear, or ambiguous shared-coordinate drag definitions are rejected.

`defs`, gradients, clipping elements, and other raw SVG elements can be written
in TSX. Each rendered diagram namespaces their identifiers and local references
so multiple diagrams can share a page safely. Grouped children retain raw
attributes and interactive metadata.

## Reproduce the local book preview

The source PDF and scan pages are local inputs excluded from Git. Import the
audited user-supplied edition with Python containing `pypdf` and Poppler on PATH:

```bash
python scripts/elementary-topology/import-source.py /path/to/ElementaryTopologyGemignani.pdf
yarn workspace @penrose/bloom build
yarn workspace @penrose/examples build
node scripts/elementary-topology/render-book.mjs packages/docs-site/public/elementary-topology/figures
yarn workspace @penrose/docs-site dev
```

The importer validates the source checksum and page count, renders mathematical
typesetting directly, and writes the local reader assets. The figure manifest
records source pages, reusable modules, review status, and placement rectangles.

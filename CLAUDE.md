# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Penrose

Penrose is a platform for creating diagrams by writing plain-text programs in three domain-specific languages. It compiles these programs, runs a constraint-based LBFGS optimizer with automatic differentiation, and renders SVG output.

## Commands

```bash
yarn start              # Start editor in watch mode
yarn build              # Build all packages (Nx)
yarn test               # Run all tests (Vitest)
yarn typecheck          # TypeScript type checking
yarn lint               # ESLint on core
yarn lint:fix           # ESLint with auto-fix
yarn format             # Prettier (write)
yarn format:check       # Prettier (check only)
yarn coverage           # Test coverage for core
yarn registry           # Run example registry tests
```

Run a single test file:
```bash
cd packages/core && yarn vitest run src/path/to/test.test.ts
```

Build a specific package:
```bash
yarn nx run <package>:build   # e.g. yarn nx run core:build
```

## Architecture

### Three-Program Model

Every Penrose diagram is defined by three programs:
- **Domain** (`.domain`) — declares types and predicates
- **Substance** (`.substance`) — instantiates objects and asserts relationships
- **Style** (`.style`) — maps substance to visual shapes and constraints

### Compilation Pipeline (`packages/core/src/`)

1. **Parser** (`parser/`) — Nearley grammars (`.ne` files) + Moo lexer produce ASTs for all three languages
2. **Compiler** (`compiler/`) — type-checks and lowers ASTs into an optimization state
3. **Engine** (`engine/`) — LBFGS optimizer runs on the state using custom automatic differentiation (`Autodiff.ts`, `ad.ts`)
4. **Renderer** (`renderer/`) — produces SVG from the optimized state; each shape type has its own file

### Key Packages

| Package | Purpose |
|---|---|
| `core` | Compiler, optimizer, renderer — the heart of Penrose |
| `editor` | React + Vite browser-based interactive editor (uses Recoil for state) |
| `components` | CodeMirror 6-based editor components with Lezer parsers for all three languages |
| `roger` | CLI tool for batch/headless rendering (Node + Canvas + jsdom) |
| `bloom` | React component library for embedding Penrose diagrams; supports JSX shape syntax |
| `solids` | SolidJS integration |
| `vscode` | VS Code extension with TextMate grammars for syntax highlighting |
| `examples` | Example `.domain`/`.substance`/`.style` programs and Bloom JSX examples |
| `docs-site` | VitePress documentation site (see page index below) |

### Bloom JSX Runtime (`packages/bloom/src/jsx-runtime.ts`)

Bloom supports JSX syntax for creating shapes. Add `/** @jsxImportSource @penrose/bloom */` at the top of any `.tsx` file to enable it.

```tsx
/** @jsxImportSource @penrose/bloom */
import { canvas, DiagramBuilder } from "@penrose/bloom";

const db = new DiagramBuilder(canvas(400, 400), "seed");
const { type, forall, ensure, build } = db;

const Node = type();
Node(); Node(); Node();

forall({ n: Node }, ({ n }) => {
  n.icon = <circle r={50} fill-color={[0.2, 0.4, 0.8, 1]} ensure-on-canvas />;
});

const diagram = await build();
const { svg } = await diagram.render();
```

**Prop names are SVG-native kebab-case** (`fill-color`, `stroke-width`, `corner-radius`, etc.) converted to camelCase at runtime.

**Two categories of JSX elements:**

1. **Known shape elements** — `circle`, `ellipse`, `rect` (→ Rectangle), `line`, `path`, `polygon`, `polyline`, `text`, `image`, `g` (→ Group), `equation`. These call the corresponding `DiagramBuilder` method and go through the LBFGS optimizer. Props that match known Bloom fields (see `penroseShapeFieldTypes` in `types.ts`) are passed to the builder; string props for unrecognized fields become `rawAttrs` applied after rendering (useful for things like `fill="url(#grad1)"`).

2. **Unknown SVG elements** — any other tag (`defs`, `linearGradient`, `stop`, `filter`, `clipPath`, etc.) returns a `RawSvgElement` and is registered with the active builder via `addRawSvgDef()`. These are injected directly into the SVG output before the optimized shapes. Parent elements absorb their children from the top-level list so only the outermost element is injected.

```tsx
// defs registered as side-effect; children dedup'd automatically
<defs>
  <linearGradient id="grad1" x1="0%" y1="0%" x2="100%" y2="0%">
    <stop offset="0%" stop-color="cornflowerblue" />
    <stop offset="100%" stop-color="tomato" />
  </linearGradient>
</defs>;

forall({ n: Node }, ({ n }) => {
  // fill="url(#grad1)" is a rawAttr — overrides Penrose's fillColor post-render
  n.icon = <circle r={50} fill="url(#grad1)" ensure-on-canvas />;
});
```

**Rendering pipeline for raw defs / rawAttrs:**
- `builder.build()` collects `rawSvgDefs: RawSvgElement[]` and `rawAttrsByName: Map<string, Record<string, string>>` from all registered defs and shapes
- `Diagram.render()` calls `stateToSVG()` (Penrose pipeline), then prepends raw defs into the SVG and applies `rawAttrs` overrides via the `titleCache` (shape name → SVG element map)

**Semantics are eager** — shapes are created immediately when JSX is evaluated, just like calling `builder.circle()`. Reference a shape's fields only after the `forall` block that assigns them.

**Functional components** are supported: any function `(props) => Shape | RawSvgElement` can be used as a JSX element.

**Active builder context** — `DiagramBuilder` sets itself as the active builder on construction and within each `forall` callback. The JSX factory calls `getActiveBuilder()` to find the right builder without requiring it to be passed explicitly. Use `setActiveBuilder()` if you need manual control.

**Key files:**
- `packages/bloom/src/jsx-runtime.ts` — JSX factory (`jsx`, `jsxs`, `jsxDEV`, `Fragment`) + `JSX.IntrinsicElements` type declarations; `isRawSvgElement` type guard
- `packages/bloom/src/core/types.ts` — `RawSvgElement` interface; `rawAttrs` field on `ShapeCommon`
- `packages/bloom/src/core/builder.ts` — `addRawSvgDef()` with child deduplication; `getActiveBuilder()` / `setActiveBuilder()`; `_activeBuilder` context managed in constructor and `internalForallWhere`
- `packages/bloom/src/core/utils.ts` — `appendRawSvgElements()` injects `RawSvgElement` trees into SVG
- `packages/bloom/package.json` — `exports` field maps `./jsx-runtime` and `./jsx-dev-runtime` to `dist/jsx-runtime.js`

**Testing bloom JSX:**
```bash
cd packages/bloom && npx vitest run src/jsx-runtime.test.tsx
```

**JSX example registry entries** live in `packages/examples/src/bloom/` and are registered with `trio: false, tsx: true` in `registry.json`. The codegen in `codegen.js` emits `tsx: true` for these entries. Parity/regression tests are in `packages/examples/src/jsx-parity.test.ts` with snapshots in `src/__snapshots__/`.

### Monorepo Tooling

- **Yarn Workspaces** + **Lerna** for package management and versioning
- **Nx** for build orchestration and caching
- Conventional commits (required for Lerna changelog generation)

## docs-site Page Index

The site lives in `packages/docs-site/` and is built with VitePress. Navigation is configured in `.vitepress/config.ts`.

**Top-level pages**
- `/examples` — `examples.md` — Gallery of example diagrams
- `/community` — `community.md` — Contribution guide
- `/blog` — `blog.md` — Blog index
- `/docs/team` — `docs/team.md` — Team page

**Tutorial** (`/docs/tutorial/`)
- `welcome` — Introduction
- `basics` — Core concepts
- `predicates` — Predicates & constraints
- `functions` — Functions

**Reference** (`/docs/ref/`)
- `ref` (overview) — Language reference index
- `ref/using` — Using Penrose
- `ref/domain/overview`, `types`, `predicates`, `functions`
- `ref/substance/overview`, `statements`, `indexed-statements`, `literal-expressions`
- `ref/style/overview`, `namespaces`, `selectors`, `selector-blocks`, `collectors`, `literals`, `expressions`, `value-types`, `vectors-matrices`, `functions`, `random-sampling`, `passthrough`
- `ref/style/shapes/` — `circle`, `ellipse`, `equation`, `group`, `image`, `line`, `path`, `polygon`, `polyline`, `rectangle`, `text`
- `ref/Interactivity` — Experimental interactivity
- `ref/api` — Language API
- `ref/optimization-api` — Optimization API
- `ref/vanilla-js`, `ref/bundle`, `ref/react`, `ref/solid` — Integration guides
- `ref/constraints` — Writing constraints & objectives

**Bloom** (`/docs/bloom/`)
- `bloom/tutorial/getting_started`, `hello_diagram`, `optimization`, `interactivity`
- `bloom/examples`

**Blog** (`/blog/`)
- `bloom` — Bloom announcement (Sep 2024)
- `tailoring-graph-domain` (Aug 2023)
- `v3` — Penrose 3.0 (Jul 2023)
- `staged-layout`, `new-language-features`, `wasm` (Jun 2023)

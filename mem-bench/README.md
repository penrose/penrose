# Editor memory-leak benchmark

Automatically measures how much memory the Penrose editor leaks **per
resample / recompile**, across several example projects, unattended.

It drives the real editor in headless Chrome, loads each example by its registry
id via `?examples=<id>`, repeatedly resamples (or recompiles) it, and after a
forced GC records the main-thread V8 heap each round. The reported leak rate is
the ordinary-least-squares **slope of the post-GC heap over the rounds**: a real
leak grows monotonically (high R²), while GC-able churn just scatters around a
flat line.

## Setup (once)

This folder is intentionally outside the yarn workspaces (`packages/*`) so its
heavy `puppeteer-core` dep stays isolated. Install it locally:

```sh
cd mem-bench && yarn install    # or: npm install
```

Chrome is used via `--chrome` (default `/usr/sbin/google-chrome-stable`).

## Running

You need a running editor. **Profile a production build** — the dev server has
extra allocation noise. From the repo root:

```sh
yarn workspace @penrose/editor build
npx vite preview --config packages/editor/vite.config.ts --port 4173
# -> serves http://localhost:4173/try/
```

Then, in another terminal:

```sh
cd mem-bench
node leak-bench.mjs                       # default: resample, 30 rounds, curated examples
node leak-bench.mjs --action both         # recompile + resample each round
node leak-bench.mjs --action compile --iterations 40
node leak-bench.mjs --examples array-models/insertionSort,dinoshade/dinoshade
node leak-bench.mjs --headful             # watch it drive the UI
node leak-bench.mjs --json out.json       # dump raw per-round series
```

## Options

| flag | default | meaning |
|------|---------|---------|
| `--url` | `http://localhost:4173/try/` | editor base URL |
| `--chrome` | `/usr/sbin/google-chrome-stable` | Chrome executable |
| `--action` | `resample` | `resample` \| `compile` \| `both` |
| `--examples` | curated set | comma-separated registry ids |
| `--iterations` | `30` | measured rounds per example |
| `--warmup` | `3` | unmeasured warmup rounds (settle JIT/caches) |
| `--settle` | `1200` | ms to wait after each action before measuring |
| `--interactive` | off | switch each diagram to EditMode and sweep the mouse over its interactive shapes every round — mounts InteractivityOverlay and exercises its per-element listeners (the `listeners/round` leak). Only examples with draggable shapes (e.g. `mobius/mobius`, `set-theory-domain/tree-euler`) move that metric. |
| `--headful` | off | show the browser |
| `--json` | — | also write raw results to a file |

Registry ids are the keys in `packages/examples/src/registry.json` (e.g.
`array-models/insertionSort`, `dinoshade/dinoshade`).

## Reading the output

```
example                                    kB/round     R²   Δheap kB  Δnodes
------------------------------------------------------------------------------
array-models/insertionSort                    248.3   0.98      7201      120  ⚠ leak
dinoshade/dinoshade                            96.1   0.95      2810       40  ⚠ leak
set-theory-domain/tree-euler                    4.2   0.11       130        0
```

- **kB/round** — the leak rate (heap slope). This is the headline number.
- **R²** — fit quality. High R² + positive slope = a real, steady leak; low R²
  means the "growth" is just GC scatter.
- **Δheap kB** — total heap growth from first to last measured round.
- **Δnodes** — DOM node growth (detached-node / element-retention signal).
- **⚠ leak** — flagged when >20 kB/round with R²>0.8.

Use it to compare branches: run on `main`, then on a fix branch, and watch the
kB/round drop.

---
title: "Switching to Wasm for 10x Speedup"
date: 2023-06-30
authors:
  - author: Sam Estep
    twitter: "@samestep"
    github: "samestep"
---

<BlogMeta />

We switched our optimizer from JS to Wasm, and everything is 10x faster!

---

When you write `encourage` and `ensure` statements in your Style program,
Penrose uses numerical optimization to find a layout of shapes that minimizes
your objectives while satisfying your constraints. The faster this is, the more
complicated diagrams you can make, and the more easily you can iterate on any
given diagram by tweaking it or trying out different random variations.

Because performance affects the user experience so much, we've recently been
putting a good amount of work into making Penrose faster. One really big win has
been [rewriting the optimizer in Rust and compiling it to WebAssembly][wasm pr],
which gave us an **order of magnitude performance improvement!**

## Overview

The locations, sizes, and even colors of shapes in a Penrose diagram are all
made up of numbers that the optimizer can play with to find a good diagram; we
call these "degrees of freedom". All the objectives and constraints get combined
together into a single numerical function meant to roughly encode how "good" or
"bad" the diagram is. We use [automatic differentiation][] on that function to
let us compute a "tweak" for each degree of freedom to make that number bigger
or smaller in a way that makes the overall diagram "better"; this is called the
gradient. Then we use the [L-BFGS][] algorithm to find a good solution by
running this gradient function over and over in a loop.

Up until this year, we had written the optimizer in TypeScript and did the
gradient computation by concatenating together a long string of JavaScript code
which we feed to the browser's JavaScript engine. At the end of last year, we
rewrote the optimizer in Rust and rewrote our code generation to produce
[WebAssembly][] instead of JavaScript. This made optimization about ten times
faster. The rest of this post describes how exactly we did that, and some of the
challenges we hit along the way.

## Impetus

In September I did [an experiment][experiment] to see what would happen if I
just went through the whole optimizer line by line (nothing clever) and rewrote
it from TypeScript to Rust. Then since the optimizer takes as input a function
to compute our objective and gradient values, I modified our compiler backend to
produce C instead of JavaScript, so that I could compile and link that together
with the Rust code for the optimizer itself.

It turns out that native is way faster. For example, consider this Penrose
diagram (in the style of a couple papers from [SIGGRAPH 2020][] and [SIGGRAPH
2022][]):

![walk on spheres diagram][walk on spheres]

Here's the difference in performance I got from running the JavaScript vs native
versions on my M1 MacBook Pro:

![walk on spheres performance][walk on spheres perf]

As you can see, even if you include the 2.66 seconds it takes to compile both
the C _and_ Rust code together, this is still over five times faster than
running the JavaScript code. If you use the more fair comparison where
everything is already compiled before we start measuring, the native version is
over twenty times faster.

Of course, for most of our diagrams, the time to compile the Rust and C code
dominates, but again, if you compare just the time to actually optimize the
diagram, the difference is still huge. Take a look at this Euclidean geometry
diagram, for instance:

![Euclidean geometry diagram][siggraph teaser]

![Euclidean geometry performance][siggraph teaser perf]

Given how good the speed of this native optimizer was, I next wanted to compile
the Rust code to WebAssembly to run it in the browser and see if we could bring
some of that speedup to the web.

## Attempt 1

Luckily for me, [Ben Titzer][], one of the creators of WebAssembly, works in our
department at CMU! So I went and asked him how I would configure a fixed
WebAssembly module for our optimizer to allow me to plug in a custom WebAssembly
module for the gradient computation. He told me to use a WebAssembly _table_,
into which I could put a pointer to the function that computes the gradient,
even if that function lives in a separate module.

It wasn't too hard to figure out how to do this in raw WebAssembly, but I was
struggling to get it working in Rust. Again luckily, my advisor happened to have
a connection to [Will Crichton][], who put us in touch with his brother
[Alex][alex crichton], who is one of the core Rust/Wasm people. Alex told me
that I could pass the Rust compiler `-Clink-arg=--export-table` to get it to
export an `__indirect_function_table`, which was what I wanted. But the table
wasn't showing up. Turns out, [`wasm-bindgen`][wasm-bindgen] was postprocessing
the WebAssembly produced by the Rust compiler, stripping out the table. So I
decided to just use the Rust compiler directly, without `wasm-bindgen`.

This turned out to be very... fun 🙃 and I found myself writing code like the
following:

```javascript
const rust = (await WebAssembly.instantiate(src)).instance.exports;

const withVec = (T, len, f) => {
  const align = T.BYTES_PER_ELEMENT;
  const size = len * align;
  const ptr = rust.__wbindgen_malloc(size, align);
  try {
    return f(new T(rust.memory.buffer, ptr, len));
  } finally {
    rust.__wbindgen_free(ptr, size, align);
  }
};

// ...

return withVec(Uint8Array, n, (vInputs) => {
  for (let i = 0; i < n; i++) {
    vInputs[i] = inputMetaToByte(inputs[i]);
  }
  return withVec(Float64Array, n, (vXs) => {
    vXs.set(varyingValues);
    rust.converge(
      index,
      vInputs.byteOffset,
      n,
      numObjEngs,
      numConstrEngs,
      vXs.byteOffset,
      n,
    );
    return Array.from(vXs);
  });
});
```

Exactly what you wanna see in JavaScript! And the best part is, there's a nasty
bug lurking. Can you spot it?

That's right: Wasm memory can [grow][]. So if you wrap its `buffer` in a
`TypedArray` and the memory grows later, that array becomes invalid. A more
correct version of `withVec` would look like this:

```javascript
const withVec = (T, len, f) => {
  const align = T.BYTES_PER_ELEMENT;
  const size = len * align;
  const ptr = rust.__wbindgen_malloc(size, align);
  try {
    return f(() => new T(rust.memory.buffer, ptr, len));
  } finally {
    rust.__wbindgen_free(ptr, size, align);
  }
};
```

But this only gets you so far. The Penrose optimizer uses a big `Params` type to
keep track of data across iterations of the numerical optimization algorithm,
and these `Params` contain some numbers, some vectors, and a couple rectangular
matrices (some of which aren't always present). Dealing with all these data
individually via `withVec` would be a huge pain, and this issue is pretty much
exactly what `wasm-bindgen` is designed to solve! So I wanted to go back and try
to use it after all.

## Attempt 2

Following Ben's advice, I was still trying to use the
`__indirect_function_table`. I took a look at the `wasm-bindgen` source code,
and I found a function called
[`unexported_unused_lld_things`][unexported_unused_lld_things]. So I opened a
[pull request][keep-lld-exports] adding a flag called `--keep-lld-exports` which
causes that function to not be called. Alex merged it a few hours later (!), so
I could just use the latest `wasm-bindgen` from GitHub `main` to access the
table while still using `wasm-bindgen`. (I quickly realized that using `cargo
install` to build `wasm-bindgen-cli` from source in every CI run is too slow, so
instead I stashed a Linux binary in a GitHub Gist to use until the next
release.)

By default, `wasm-bindgen` exports Rust types as JavaScript classes which point
to data living in the Wasm memory. This is great for efficiency because it means
data doesn't get unnecessarily copied back and forth, but the downside is that
the user on the JS side needs to remember to manually call `.free()` or some
other thing that consumes the object, or a memory leak springs up. For our
purposes I wanted our optimizer interface to just return plain old JavaScript
data, so I made use of the very nice [`serde-wasm-bindgen`][serde-wasm-bindgen]
and [ts-rs][] crates. These work great together as long as you use the
[`Serializer::json_compatible()`][json_compatible] preset.

To bundle this all up into an actual `@penrose/optimizer` package, I had to
write a several-step build process, running ts-rs via `cargo test`, and using
[esbuild's `binary` loader][esbuild binary] to embed the Wasm binary directly
into the JavaScript:

```shell
cargo test
cargo build --target=wasm32-unknown-unknown --release
wasm-bindgen --target=web --keep-lld-exports --out-dir=build target/wasm32-unknown-unknown/release/penrose_optimizer.wasm
esbuild ./source.ts --outfile=index.js --platform=neutral --bundle --loader:.wasm=binary --define:import.meta.url=null --sourcemap
```

`wasm-bindgen` has several available [targets][wasm-bindgen target]. Here you
can see I'm using `web` instead of `bundler`, despite the fact that esbuild is a
bundler. What gives? Well, the `bundler` target is designed to work well with
Webpack, which is very slow; we've worked hard to get rid of all Webpack usage
in Penrose, with the effect of cutting two-thirds off our build times. The `web`
target emits this function:

```javascript
async function init(input) {
  if (typeof input === "undefined") {
    input = new URL("penrose_optimizer_bg.wasm", import.meta.url);
  }
  const imports = getImports();

  if (
    typeof input === "string" ||
    (typeof Request === "function" && input instanceof Request) ||
    (typeof URL === "function" && input instanceof URL)
  ) {
    input = fetch(input);
  }

  initMemory(imports);

  const { instance, module } = await load(await input, imports);

  return finalizeInit(instance, module);
}

export default init;
```

What I wanted was to provide a single bundle that could be used both in the
browser and in Node, so this actually works great since I can write an
`instance.js` file to pass in the bytes directly, bypassing `fetch`:

```javascript
import init from "./build/penrose_optimizer";
import bytes from "./build/penrose_optimizer_bg.wasm";

export let maybeOptimizer = undefined;

export const optimizerReady = init(bytes).then((output) => {
  maybeOptimizer = output;
});
```

The problem is that esbuild doesn't identify and eliminate the dead code in
`init`, so smarter tools like [Vite][] (which we use for our website) see the
`new URL` and try to turn `penrose_optimizer_bg.wasm` into a resource. Our
bundle is in a different directory from the Wasm binary file itself, so Vite
throws a build error. The hacky solution to this problem is to pass
`--define:import.meta.url=null` to esbuild so that the `new URL` no longer fits
Vite's smart resource pattern-recognition.

The reason the above was `instance.js` instead of `instance.ts` is that
TypeScript still doesn't understand the `*.wasm` extension even if esbuild does,
so we also need a separate `instance.d.ts` file:

```typescript
import { InitOutput } from "./build/penrose_optimizer";

export declare let maybeOptimizer: InitOutput | undefined;

export declare const optimizerReady: Promise<void>;
```

Then at the top-level of `@penrose/optimizer`, we export a promise that can be
`await`ed to ensure the optimizer is `ready`:

```typescript
import { maybeOptimizer, optimizerReady } from "./instance";

let maybeIndex: number | undefined = undefined;

export const ready = optimizerReady.then(() => {
  penrose_init();
  maybeIndex = maybeOptimizer!.__indirect_function_table.length;
  maybeOptimizer!.__indirect_function_table.grow(1);
});
```

The annoying thing is that now we need to do this before every single place
where the optimizer is used:

```typescript
import { ready } from "@penrose/optimizer";

await ready;
```

We went back and forth on this architecture for a bit, and eventually ended up
deciding to solve this annoyance by [using top-level `await`][top-level await].
Not all of our downstream code supported top-level await because we were
previously using [Docusaurus][] for our website, so we had to migrate to
[VitePress][] first.

And the performance difference is striking!

### Before

```
|                                         0s   1s   2s   3s   4s   5s   6s   7s   8s   9s  10s  11s  12s  13s  14s  15s  16s  17s  18s  19s  20s  21s  22s  23s  24s  25s  26s  27s  28s  29s  30s  31s  32s  33s  34s  35s  36s  37s  38s  39s  40s  41s  42s  43s  44s  45s  46s  47s  48s  49s  50s  51s  52s  53s  54s  55s  56s  57s  58s  59s  60s  61s  62s  63s  64s  65s  66s  67s  68s  69s  70s  71s  72s  73s
|                                         |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |
| 3d-projection-fake-3d-linear-algebra    ▝▀▞▖
| allShapes-allShapes                     ▝▀▚▚▄▄
| arrowheads-arrowheads                   ▝▀▞▖
| circle-example-euclidean                ▝▀▀▀▀▀▞▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▖
| collinear-euclidean                     ▝▀▀▚▀▀▀▖
| congruent-triangles-euclidean           ▝▀▀▀▀▀▀▀▀▞▚
| continuousmap-continuousmap             ▝▀▚▚
| hypergraph-hypergraph                   ▝▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▞▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▖
| incenter-triangle-euclidean             ▝▀▀▀▚▀▀▀▀▀▀▀▀▀▀▚
| lagrange-bases-lagrange-bases           ▝▀▚▚
| midsegment-triangles-euclidean          ▝▀▀▀▚▚
| non-convex-non-convex                   ▝▀▀▀▀▞▀▀▀▀▚
| one-water-molecule-atoms-and-bonds      ▝▀▞▖
| parallel-lines-euclidean                ▝▀▀▚▀▀▀▀▀▀▀▀▀▚
| persistent-homology-persistent-homology ▝▀▀▀▀▀▀▀▀▞▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▄
| points-around-line-shape-distance       ▝▀▀▀▀▀▀▞▚
| points-around-polyline-shape-distance   ▝▀▀▀▀▀▀▀▀▀▀▀▀▞▀▚
| points-around-star-shape-distance       ▝▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▚▀▖
| siggraph-teaser-euclidean-teaser        ▝▀▀▀▀▀▞▀▀▚
| small-graph-disjoint-rect-line-horiz    ▝▀▀▀▀▀▀▀▀▞▚
| small-graph-disjoint-rects              ▝▀▞▖
| small-graph-disjoint-rects-large-canvas ▝▀▞▖
| small-graph-disjoint-rects-small-canvas ▝▀▚▚
| tree-tree                               ▝▀▀▞▖
| tree-venn                               ▝▀▀▀▚▞▀▖
| tree-venn-3d                            ▝▀▀▀▚▀▀▚▄▖
| two-vectors-perp-vectors-dashed         ▝▀▀▞▖
| vector-wedge-exterior-algebra           ▝▀▀▚▚
| wet-floor-atoms-and-bonds               ▝▀▀▚▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▖
| wos-laplace-estimator-walk-on-spheres   ▝▀▀▀▀▞▀▀▀▖
| wos-nested-estimator-walk-on-spheres    ▝▀▀▀▀▀▀▞▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▚
| wos-offcenter-estimator-walk-on-spheres ▝▀▀▀▚▀▀▀▀▖
| wos-poisson-estimator-walk-on-spheres   ▝▀▀▀▚▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▖
```

### After

```
|                                         0s   1s   2s   3s   4s   5s   6s   7s   8s
|                                         |    |    |    |    |    |    |    |    |
| 3d-projection-fake-3d-linear-algebra    ▝▀▞▖
| allShapes-allShapes                     ▝▀▚▞▄▄
| arrowheads-arrowheads                   ▝▀▞▖
| circle-example-euclidean                ▝▀▀▀▀▀▞▀▀▚
| collinear-euclidean                     ▝▀▀▚▚
| congruent-triangles-euclidean           ▝▀▀▀▀▀▀▀▀▞▖
| continuousmap-continuousmap             ▝▀▚▚
| hypergraph-hypergraph                   ▝▀▀▀▀▀▀▀▀▀▀▀▞▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▖
| incenter-triangle-euclidean             ▝▀▀▀▚▚
| lagrange-bases-lagrange-bases           ▝▀▀▞▖
| midsegment-triangles-euclidean          ▝▀▀▀▀▞▖
| non-convex-non-convex                   ▝▀▀▀▚▚
| one-water-molecule-atoms-and-bonds      ▝▚▚
| parallel-lines-euclidean                ▝▀▀▚▚
| persistent-homology-persistent-homology ▝▀▀▀▀▀▀▀▞▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▚
| points-around-line-shape-distance       ▝▀▀▀▞▖
| points-around-polyline-shape-distance   ▝▀▀▀▀▀▀▀▀▀▀▚▚
| points-around-star-shape-distance       ▝▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▚▚
| siggraph-teaser-euclidean-teaser        ▝▀▀▀▀▀▚▚
| small-graph-disjoint-rect-line-horiz    ▝▀▀▀▀▀▀▀▀▚▚
| small-graph-disjoint-rects              ▝▀▞▖
| small-graph-disjoint-rects-large-canvas ▝▀▞▖
| small-graph-disjoint-rects-small-canvas ▝▀▚▚
| tree-tree                               ▝▀▚▞▖
| tree-venn                               ▝▀▀▀▚▞▖
| tree-venn-3d                            ▝▀▀▀▞▄▖
| two-vectors-perp-vectors-dashed         ▝▀▚▚
| vector-wedge-exterior-algebra           ▝▀▚▚
| wet-floor-atoms-and-bonds               ▝▀▀▞▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▚
| wos-laplace-estimator-walk-on-spheres   ▝▀▀▀▀▀▀▀▞▖
| wos-nested-estimator-walk-on-spheres    ▝▀▀▀▀▀▀▀▀▀▀▀▀▀▚▀▀▀▀▀▀▀▚
| wos-offcenter-estimator-walk-on-spheres ▝▀▀▀▀▀▀▀▀▚▚
| wos-poisson-estimator-walk-on-spheres   ▝▀▀▀▀▀▀▀▀▚▀▀▖
```

## Attempt 3

At this point we've achieved our performance goal, but the `@penrose/optimizer`
module is basically impossible to use since it's so tightly coupled to our
autodiff codegen; for instance, it exports a `class Gradient` with this
docstring:

```typescript
/**
 * An instantiated WebAssembly function that can be used either to directly
 * compute outputs and gradients or to step a state via the optimizer.
 *
 * Each WebAssembly module used to construct an instance of this class must
 * follow some conventions using other things exported from this package:
 *
 * - The module name of every import must match `importModule`.
 *
 * - One import must be a memory whose name matches `importMemoryName`.
 *
 * - The rest of the imports must be functions in the same order as the the
 *   insertion order of `builtins`. The name of each of these imports must be a
 *   base-36 integer (`"0"`-`"9"`, `"a"`-`"z"`) corresponding to the index of
 *   the name in `builtins`. The type of the import is determined by its value
 *   in `builtins`, which is an element of `BuiltinType`:
 *
 *   - `"addToStackPointer"` takes one `i32` and returns one `i32`.
 *   - `"unary"` takes one `f64` and returns one `f64`.
 *   - `"binary"` takes two `f64`s and returns one `f64`.
 *   - `"polyRoots"` takes two `i32`s and returns nothing.
 *
 * - The module must export a function whose name matches `exportFunctionName`,
 *   which takes four `i32`s and returns one `f64`.
 */
```

Yikes. The reason for this is that I took Ben's original advice a bit too
literally, thinking that I needed to directly put the WebAssembly function
produced by our autodiff compiler directly into our Rust optimizer module's
indirect function table (for #performance of course). Premature optimization!
What I should have done instead is take advantage of `wasm-bindgen`'s various
features for calling JavaScript functions from Rust/Wasm. So last month I wrote
pull requests [#1338][decouple] and [#1368][cleanup] which expose a much more
reasonable `@penrose/optimizer` interface: it now just takes in an arbitrary
JavaScript function to compute the objective and gradient:

```typescript
/**
 * Given an objective function `f` and some constraint functions `g_j`, where we
 * want to minimize `f` subject to `g_j(x) \leq 0` for all `j`, this function
 * computes the following, where `\lambda` is `weight`:
 *
 * `\phi(x, \lambda) = f(x) + \lambda * \sum_j \max(g_j(x), 0)^2`
 *
 * The gradient with respect to `x` is stored in `grad`.
 *
 * @param x input vector
 * @param weight `\lambda`, the weight of the constraint term
 * @param grad mutable array to store gradient in
 * @returns the augmented objective value `\phi(x, \lambda)`
 */
export type Fn = (
  x: Float64Array,
  weight: number,
  grad: Float64Array,
) => number;

/**
 * Steps the optimizer until either convergence or `stop` returns `true`.
 * @param f to compute objective, constraints, and gradient
 * @param x mutable array to update at each step
 * @param state initial optimizer state
 * @param stop early stopping criterion
 * @returns optimizer state after stepping
 */
export const stepUntil = (
  f: Fn,
  x: Float64Array,
  state: Params,
  stop: () => boolean,
): Params => penrose_step_until(f, x, state, stop);
```

And our autodiff compiler just wraps its raw WebAssembly function to conform to
that interface. I think there might be a small performance drop (hard to notice
since the performance data we currently collect are fairly noisy), but the
tradeoff is worth it.

The other consequence of this is that we now no longer use the
`--keep-lld-exports` flag at all! So I guess I should have thought more about
the design and the tools available before going to submit upstream patches.

## Takeaways

While there is some debate about how fast Wasm actually is in practice (and
indeed, our Wasm optimizer is a few times slower than the native one), we found
a _significant_ performance gain compared to JavaScript. Definitely worth the
effort.

This project took a lot of trial and error, though! I also leaned heavily on
experts like Ben and Alex to get me through some of the trickier parts. I'm
super excited about all the work going on in the [Rust/Wasm][rustwasm] space;
there are a lot of great tools and resources already, and I'm hoping that going
forward we can continue making all of this more accessible to newcomers.

[automatic differentiation]: https://en.wikipedia.org/wiki/Automatic_differentiation
[alex crichton]: https://github.com/alexcrichton
[ben titzer]: https://s3d.cmu.edu/people/core-faculty/titzer-ben.html
[cleanup]: https://github.com/penrose/penrose/pull/1368
[decouple]: https://github.com/penrose/penrose/pull/1338
[docusaurus]: https://docusaurus.io/
[esbuild binary]: https://esbuild.github.io/content-types/#binary
[experiment]: https://github.com/penrose/experiments/tree/main/2022-optimizer-performance
[function constructor]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/Function
[grow]: https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/Memory/Grow
[json_compatible]: https://docs.rs/serde-wasm-bindgen/0.5.0/serde_wasm_bindgen/struct.Serializer.html#method.json_compatible
[keep-lld-exports]: https://github.com/rustwasm/wasm-bindgen/pull/3147
[l-bfgs]: https://en.wikipedia.org/wiki/Limited-memory_BFGS
[rustwasm]: https://rustwasm.github.io/
[serde-wasm-bindgen]: https://github.com/cloudflare/serde-wasm-bindgen
[siggraph 2020]: http://www.cs.cmu.edu/~kmcrane/Projects/MonteCarloGeometryProcessing/index.html
[siggraph 2022]: https://cs.dartmouth.edu/wjarosz/publications/sawhneyseyb22gridfree.html
[siggraph teaser]: https://raw.githubusercontent.com/penrose/penrose/3f0947795e975c33f7d8cfad0be467746221f005/diagrams/siggraph-teaser-euclidean-teaser.svg
[siggraph teaser perf]: https://raw.githubusercontent.com/penrose/experiments/bc833544044f82b381c643b8a4062146181b8ba0/2022-optimizer-performance/mac-arm/siggraph-teaser-euclidean-teaser.svg
[top-level await]: https://github.com/penrose/penrose/pull/1188
[ts-rs]: https://github.com/Aleph-Alpha/ts-rs
[unexported_unused_lld_things]: https://github.com/rustwasm/wasm-bindgen/blob/0.2.83/crates/cli-support/src/lib.rs#L610-L626
[vite]: https://vitejs.dev/
[vitepress]: https://vitepress.dev/
[walk on spheres]: https://raw.githubusercontent.com/penrose/penrose/3f0947795e975c33f7d8cfad0be467746221f005/diagrams/wos-nested-estimator-walk-on-spheres.svg
[walk on spheres perf]: https://raw.githubusercontent.com/penrose/experiments/bc833544044f82b381c643b8a4062146181b8ba0/2022-optimizer-performance/mac-arm/wos-nested-estimator-walk-on-spheres.svg
[wasm pr]: https://github.com/penrose/penrose/pull/1092
[wasm-bindgen]: https://github.com/rustwasm/wasm-bindgen
[wasm-bindgen target]: https://rustwasm.github.io/docs/wasm-bindgen/reference/deployment.html
[webassembly]: https://webassembly.org/
[will crichton]: https://willcrichton.net/

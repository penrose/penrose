<script setup>
import BlogMeta from "../../../../src/components/BlogMeta.vue";
</script>

# Rust Wasm 10x faster than JS

<BlogMeta github="samestep" date="2023-06-01" />

In January we merged pull request [#1092][wasm pr], which took the performance
bottleneck component of our system (the optimizer) and rewrote it from
TypeScript to Rust that we compile to WebAssembly. Now it's an order of
magnitude faster.

## Background

Penrose Style programs can include `ensure` and `encourage` statements, which
represent numerical constraints and objectives that Penrose will try to satisfy
and optimize (respectively) while laying out the diagram. All these objectives
and constraints get combined together into a numerical optimization problem, on
which we use an exterior-point method with [L-BFGS][] to find a solution. This
means that we need to combine all our objectives and constraints into a single
numerical function of our vector of degrees of freedom, and then compute the
gradient of that function repeatedly in a loop as part of the optimizer. Up
until this year, we had written the optimizer in TypeScript and did the gradient
computation by putting together a long string of JavaScript code and passing
that to [`new Function`][function constructor].

## Impetus

I wasn't working on Penrose last summer, but in the back of my mind I did start
wondering whether it would be faster if we ran it natively instead of in the
browser. So in September I did [an experiment][experiment] to see what would
happen if I just went through the whole optimizer line by line and rewrote it
from TypeScript to Rust. I didn't do anything clever, as you can see by my many
usages of `.clone()`; I just translated it as naïvely as I could. Then, since
the optimizer takes as input a function to compute our objective and gradient
values, I modified our compiler backend to produce C instead of JavaScript, so
that I could compile and link that together with the Rust code for the optimizer
itself.

Turns out, native is way faster. For example, consider this Penrose diagram (in
the style of a couple papers from [SIGGRAPH 2020][] and [SIGGRAPH 2022][]):

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

## Implementation

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

## Takeaways

This took a lot of trial and error! I also leaned heavily on experts like Ben
and Alex to get me through some of the trickier parts. I'm super excited about
all the work going on in the [Rust/Wasm][rustwasm] space; there's a lot of great
tools and resources there already, and I'm hoping that going forward we can
continue making all of this more accessible to newcomers.

[alex crichton]: https://github.com/alexcrichton
[ben titzer]: https://s3d.cmu.edu/people/core-faculty/titzer-ben.html
[experiment]: https://github.com/penrose/experiments/tree/main/2022-optimizer-performance
[function constructor]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/Function
[l-bfgs]: https://en.wikipedia.org/wiki/Limited-memory_BFGS
[rustwasm]: https://rustwasm.github.io/
[siggraph 2020]: http://www.cs.cmu.edu/~kmcrane/Projects/MonteCarloGeometryProcessing/index.html
[siggraph 2022]: https://cs.dartmouth.edu/wjarosz/publications/sawhneyseyb22gridfree.html
[siggraph teaser]: https://raw.githubusercontent.com/penrose/penrose/3f0947795e975c33f7d8cfad0be467746221f005/diagrams/siggraph-teaser-euclidean-teaser.svg
[siggraph teaser perf]: https://raw.githubusercontent.com/penrose/experiments/bc833544044f82b381c643b8a4062146181b8ba0/2022-optimizer-performance/mac-arm/siggraph-teaser-euclidean-teaser.svg
[walk on spheres]: https://raw.githubusercontent.com/penrose/penrose/3f0947795e975c33f7d8cfad0be467746221f005/diagrams/wos-nested-estimator-walk-on-spheres.svg
[walk on spheres perf]: https://raw.githubusercontent.com/penrose/experiments/bc833544044f82b381c643b8a4062146181b8ba0/2022-optimizer-performance/mac-arm/wos-nested-estimator-walk-on-spheres.svg
[wasm pr]: https://github.com/penrose/penrose/pull/1092
[wasm-bindgen]: https://github.com/rustwasm/wasm-bindgen
[will crichton]: https://willcrichton.net/

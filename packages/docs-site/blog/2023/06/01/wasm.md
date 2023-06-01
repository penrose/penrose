<script setup>
import BlogMeta from "../../../../src/components/BlogMeta.vue";
</script>

# Rust Wasm 10x faster than JS

<BlogMeta github="samestep" date="2023-06-01" />

In January we merged pull request [#1092][wasm pr], which took the performance
bottleneck component of our system (the optimizer) and rewrote it from
TypeScript to Rust that we compile to WebAssembly. Now it's an order of
magnitude faster.

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

![Euclidean geometry diagram][siggraph teaser]

![Euclidean geometry performance][siggraph teaser perf]

## Planning

Luckily for me, [Ben Titzer][], one of the creators of WebAssembly, works in our
department at CMU!

[ben titzer]: https://s3d.cmu.edu/people/core-faculty/titzer-ben.html
[experiment]: https://github.com/penrose/experiments/tree/main/2022-optimizer-performance
[siggraph 2020]: http://www.cs.cmu.edu/~kmcrane/Projects/MonteCarloGeometryProcessing/index.html
[siggraph 2022]: https://cs.dartmouth.edu/wjarosz/publications/sawhneyseyb22gridfree.html
[siggraph teaser]: https://raw.githubusercontent.com/penrose/penrose/3f0947795e975c33f7d8cfad0be467746221f005/diagrams/siggraph-teaser-euclidean-teaser.svg
[siggraph teaser perf]: https://raw.githubusercontent.com/penrose/experiments/bc833544044f82b381c643b8a4062146181b8ba0/2022-optimizer-performance/mac-arm/siggraph-teaser-euclidean-teaser.svg
[walk on spheres]: https://raw.githubusercontent.com/penrose/penrose/3f0947795e975c33f7d8cfad0be467746221f005/diagrams/wos-nested-estimator-walk-on-spheres.svg
[walk on spheres perf]: https://raw.githubusercontent.com/penrose/experiments/bc833544044f82b381c643b8a4062146181b8ba0/2022-optimizer-performance/mac-arm/wos-nested-estimator-walk-on-spheres.svg
[wasm pr]: https://github.com/penrose/penrose/pull/1092

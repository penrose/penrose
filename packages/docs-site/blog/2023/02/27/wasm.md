<script setup>
import BlogMeta from "../../../../src/components/BlogMeta.vue";
</script>

# Rust Wasm 10x faster than JS

<BlogMeta github="samestep" date="2023-02-27" />

In January we merged pull request [#1092][wasm pr], which took the performance
bottleneck component of our system (the optimizer) and rewrote it from
TypeScript to Rust that we compile to WebAssembly. Now it's an order of
magnitude faster.

[wasm pr]: https://github.com/penrose/penrose/pull/1092

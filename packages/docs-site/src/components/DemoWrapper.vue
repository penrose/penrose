<script setup lang="ts">
import { defineAsyncComponent } from "vue";

import siggraphTeaser from "@penrose/examples/src/geometry-domain/trio-siggraph-teaser-euclidean-teaser.json";
import vennTree from "@penrose/examples/src/set-theory-domain/trio-tree-venn.json"

const demo = [
  siggraphTeaser,
  vennTree
];

const Demo = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  window.global = window; // shim global for svg-flatten
  const { Demo } = await import("@penrose/components");
  return applyPureReactInVue(Demo);
});
</script>

<template>
  <div
    style="
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      overflow: hidden;
    "
  >
    <!-- TODO: bad hardcoded width -->
    <Demo :examples="demo" width="280px" />
  </div>
</template>

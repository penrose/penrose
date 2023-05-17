<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import siggraphTeaser from "@penrose/examples/dist/geometry-domain/siggraph-teaser-euclidean-teaser";
import treeVenn from "@penrose/examples/dist/set-theory-domain/tree-venn";

const demo = [
  {
    sub: siggraphTeaser.substance,
    sty: siggraphTeaser.style,
    dsl: siggraphTeaser.domain,
    variation: siggraphTeaser.variation,
  },
  {
    sub: treeVenn.substance,
    sty: treeVenn.style,
    dsl: treeVenn.domain,
    variation: "PlumvilleCapybara104",
  },
];

const Demo = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
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

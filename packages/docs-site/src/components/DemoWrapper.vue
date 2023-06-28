<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import siggraphTeaser from "@penrose/examples/dist/geometry-domain/siggraph-teaser.trio.js";
import treeVenn from "@penrose/examples/dist/set-theory-domain/tree-venn-3d.trio.js";
import hexagonal from "@penrose/examples/dist/spectral-graphs/examples/hexagonal-lattice.trio.js";
import caffieine from "@penrose/examples/dist/structural-formula/molecules/caffeine.trio.js";

import { useMediaQuery } from "@vueuse/core";

const isVertical = useMediaQuery("(min-width: 960px)");
const isMobile = useMediaQuery("(min-width: 640px)");

const demo = [
  {
    sub: siggraphTeaser.substance,
    sty: siggraphTeaser.style.map(({ contents }) => contents).join("\n"),
    dsl: siggraphTeaser.domain,
    variation: siggraphTeaser.variation,
  },
  {
    sub: hexagonal.substance,
    sty: hexagonal.style.map(({ contents }) => contents).join("\n"),
    dsl: hexagonal.domain,
    variation: hexagonal.variation,
    stepSize: 10,
  },
  {
    sub: treeVenn.substance,
    sty: treeVenn.style.map(({ contents }) => contents).join("\n"),
    dsl: treeVenn.domain,
    variation: "PlumvilleCapybara104",
    imageResolver: treeVenn.style[0].resolver,
  },
  {
    sub: caffieine.substance,
    sty: caffieine.style.map(({ contents }) => contents).join("\n"),
    dsl: caffieine.domain,
    variation: caffieine.variation,
    stepSize: 1,
    imageResolver: caffieine.style[0].resolver,
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
      display: flex;
      align-items: center;
      justify-content: center;
      overflow: hidden;
      width: 100%;
      height: 100%;
    "
  >
    <!-- TODO: bad hardcoded width -->
    <div
      :style="`width: ${
        isVertical.valueOf() ? 400 : isMobile.valueOf() ? 250 : 200
      }px; height: ${
        isVertical.valueOf() ? 400 : isMobile.valueOf() ? 250 : 200
      }px;`"
    >
      <Demo :examples="demo" />
    </div>
  </div>
</template>

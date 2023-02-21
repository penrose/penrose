<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import geometryDomain from "@penrose/examples/dist/geometry-domain/index";
import setTheoryDomain from "@penrose/examples/dist/set-theory-domain/index";

const demo = [
  {
    sub: geometryDomain["teaser.substance"],
    sty: geometryDomain["euclidean-teaser.style"],
    dsl: geometryDomain["geometry.domain"],
    variation: "StromboliCat61816",
  },
  {
    sub: setTheoryDomain["tree.substance"],
    sty: setTheoryDomain["venn.style"],
    dsl: setTheoryDomain["setTheory.domain"],
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

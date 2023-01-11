<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import geometryDomain from "@penrose/examples/dist/geometry-domain/index"
import setTheoryDomain from "@penrose/examples/dist/set-theory-domain/index"

const demo = [
  {sub: geometryDomain["teaser.sub"], sty: geometryDomain["euclidean-teaser.sty"], dsl: geometryDomain["geometry.dsl"], variation: "StromboliCat61816"},
  {sub: setTheoryDomain["tree.sub"], sty: setTheoryDomain["venn.sty"], dsl: setTheoryDomain["setTheory.dsl"], variation: "PlumvilleCapybara104"}
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

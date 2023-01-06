<script setup lang="ts">
import { examples, registry } from "@penrose/examples";
import { defineAsyncComponent } from "vue";

const exampleFromURI = (uri) => {
  let x = examples;
  for (const part of uri.split("/")) {
    x = x[part];
  }
  return x;
};

const findTrio = (sub, sty) => {
  const matching = registry.trios.filter(
    ({ substance, style }) => substance === sub && style === sty
  );
  if (matching.length !== 1) {
    throw Error(`expected exactly one matching trio, got ${matching.length}`);
  }
  const [{ substance, style, domain, variation }] = matching;
  return {
    dsl: exampleFromURI(registry.domains[domain].URI),
    sub: exampleFromURI(registry.substances[substance].URI),
    sty: exampleFromURI(registry.styles[style].URI),
    variation,
  };
};

const demo = [
  findTrio("siggraph-teaser", "euclidean-teaser"),
  findTrio("tree", "venn"),
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

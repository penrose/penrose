<script setup lang="ts">
import registry from "@penrose/examples/dist/registry.js";
import { withBase, useData } from "vitepress";
import { defineAsyncComponent } from "vue";

const Gallery = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  const { Gallery } = await import("@penrose/components");
  return applyPureReactInVue(Gallery);
});
const link = withBase(`/try/index.html`);
</script>

<template>
  <div style="width: 100%">
    <Gallery
      :registry="registry"
      :ideLink="link"
      :dark="useData().isDark.value"
    />
  </div>
</template>

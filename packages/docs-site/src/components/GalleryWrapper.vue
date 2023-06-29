<script setup lang="ts">
import { withBase } from "vitepress";
import { defineAsyncComponent } from "vue";

const Gallery = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  const { Gallery } = await import("@penrose/components");
  return applyPureReactInVue(Gallery);
});
const link = withBase(`/try/index.html`);
</script>

<template>
  <div style="width: 100%"><Gallery :ideLink="link" /></div>
</template>

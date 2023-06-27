<script setup lang="ts">
import * as veaury from "veaury";
import { withBase } from "vitepress";
import { defineAsyncComponent } from "vue";

const Gallery = defineAsyncComponent(async () => {
  const Gallery = (await import("@penrose/components/dist/Gallery.js")).default;
  return veaury.applyPureReactInVue(Gallery);
});
const link = withBase(`/try/index.html`);
</script>

<template>
  <div style="width: 100%"><Gallery :ideLink="link" /></div>
</template>

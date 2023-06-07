<script setup lang="ts">
import { defineAsyncComponent } from "vue";

const Gallery = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  const { Gallery } = await import("@penrose/components");
  return applyPureReactInVue(Gallery);
});
</script>

<template>
  <div style="width: 100%">
    <Gallery />
  </div>
</template>

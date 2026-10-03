<script setup lang="ts">
import { defineAsyncComponent } from "vue";

const StagedDiagram = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  const { StagedDiagram } = await import("@penrose/components");
  return applyPureReactInVue(StagedDiagram);
});
const props = defineProps(["trio"]);
</script>

<template>
  <ClientOnly>
    <StagedDiagram :trio="props.trio" />
  </ClientOnly>
</template>

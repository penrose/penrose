<script setup lang="ts">
import { defineAsyncComponent } from "vue";

const StagedDiagram = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  const StagedDiagram = (
    await import("@penrose/components/dist/StagedDiagram.js")
  ).default;
  return applyPureReactInVue(StagedDiagram);
});
const props = defineProps(["trio"]);
</script>

<template>
  <StagedDiagram :trio="props.trio" />
</template>

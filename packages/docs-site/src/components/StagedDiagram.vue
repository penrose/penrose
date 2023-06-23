<script setup lang="ts">
import { defineAsyncComponent } from "vue";

const StagedDiagram = defineAsyncComponent(async () => {
  const { applyReactInVue } = await import("veaury");
  const { StagedDiagram } = await import("@penrose/components");
  return applyReactInVue(StagedDiagram);
});

const props = defineProps(["trio"]);
</script>

<template>
  <StagedDiagram :trio="props.trio" />
</template>

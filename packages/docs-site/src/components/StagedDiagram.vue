<script setup lang="ts">
import * as veaury from "veaury";
import { defineAsyncComponent } from "vue";

const StagedDiagram = defineAsyncComponent(async () => {
  const StagedDiagram = (
    await import("@penrose/components/dist/StagedDiagram.js")
  ).default;
  return veaury.applyReactInVue(StagedDiagram);
});

const props = defineProps(["trio"]);
</script>

<template>
  <StagedDiagram :trio="props.trio" />
</template>

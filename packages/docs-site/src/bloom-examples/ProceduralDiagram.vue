<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import ProceduralDiagramReact from "./procedural_diagram";

const ProceduralDiagram = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  return applyPureReactInVue(ProceduralDiagramReact);
});
</script>

<template>
  <ProceduralDiagram />
</template>

<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import HelloDiagramReact from "./hello_diagram";

const HelloDiagram = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  return applyPureReactInVue(HelloDiagramReact);
});
</script>

<template>
  <HelloDiagram />
</template>

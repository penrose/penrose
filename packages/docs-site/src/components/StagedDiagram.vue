<script setup lang="ts">
import { defineComponent } from "vue";
import { applyReactInVue } from "veaury";
import { StagedDiagram as StagedDiagramReact } from "@penrose/components";

const StagedDiagram = applyReactInVue(StagedDiagramReact);

const props = defineProps(["trio"]);
</script>

<template>
  <StagedDiagram :trio="props.trio" />
</template>

<script setup lang="ts">
import * as veaury from "veaury";
import { StagedDiagram as StagedDiagramReact } from "@penrose/components";

const StagedDiagram = veaury.applyReactInVue(StagedDiagramReact);

const props = defineProps(["trio"]);
</script>

<template>
  <StagedDiagram :trio="props.trio" />
</template>

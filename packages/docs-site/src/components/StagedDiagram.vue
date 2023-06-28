<script setup lang="ts">
import { defineAsyncComponent, defineComponent, h, onMounted } from "vue";

let StagedDiagram = defineComponent((props) => {
  return () => {
    return h("div");
  };
});

onMounted(() => {
  StagedDiagram = defineAsyncComponent(async () => {
    const { applyPureReactInVue } = await import("veaury");
    const StagedDiagram = (
      await import("@penrose/components/dist/StagedDiagram.js")
    ).default;
    return applyPureReactInVue(StagedDiagram);
  });
});
const props = defineProps(["trio"]);
</script>

<template>
  <ClientOnly>
    <StagedDiagram :trio="props.trio" />
  </ClientOnly>
</template>

<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import PlanetsReact from "./planets";

const PlanetsArrows = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  return applyPureReactInVue(() => {
    return PlanetsReact({ arrows: true });
  });
});
</script>

<template>
  <PlanetsArrows />
</template>

<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import PlanetsReact from "./planets";

const Planets = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  return applyPureReactInVue(() => {
    return PlanetsReact({ arrows: false });
  });
});
</script>

<template>
  <Planets />
</template>

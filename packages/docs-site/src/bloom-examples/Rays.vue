<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import RaysReact from "./rays";

const Rays = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  return applyPureReactInVue(RaysReact);
});
</script>

<template>
  <Rays />
</template>

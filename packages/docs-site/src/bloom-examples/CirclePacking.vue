<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import CirclePackingReact from "./circle_packing";

const CirclePacking = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  return applyPureReactInVue(CirclePackingReact);
});
</script>

<template>
  <CirclePacking />
</template>

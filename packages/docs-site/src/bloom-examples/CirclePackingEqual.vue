<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import CirclePackingReact from "./circle_packing";

const CirclePackingEqual = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  return applyPureReactInVue(() =>
    CirclePackingReact({ constraintType: "equally-spaced" }),
  );
});
</script>

<template>
  <CirclePackingEqual />
</template>

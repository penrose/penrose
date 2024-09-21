<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import CirclePackingReact from "./circle_packing";

const CirclePackingPadded = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  return applyPureReactInVue(() =>
    CirclePackingReact({ constraintType: "disjoint-padded" }),
  );
});
</script>

<template>
  <CirclePackingPadded />
</template>

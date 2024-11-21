<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import CirclePackingReact from "./circle_packing";

const CirclePackingDisjoint = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  return applyPureReactInVue(() =>
    CirclePackingReact({ constraintType: "disjoint" }),
  );
});
</script>

<template>
  <CirclePackingDisjoint />
</template>

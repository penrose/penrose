<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import EigenReact from "./eigen";

const Eigen = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  return applyPureReactInVue(EigenReact);
});
</script>

<template>
  <Eigen />
</template>

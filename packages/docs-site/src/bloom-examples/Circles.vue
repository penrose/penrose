<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import CirclesReact from "./circles";

const Circles = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  return applyPureReactInVue(CirclesReact);
});
</script>

<template>
  <Circles />
</template>

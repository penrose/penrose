<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import ExamplesReact from "./examples_page";

const Examples = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  return applyPureReactInVue(ExamplesReact);
});
</script>

<template>
  <Examples />
</template>

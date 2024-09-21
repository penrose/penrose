<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import ReflectionReact from "./reflection";

const Reflection = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  return applyPureReactInVue(ReflectionReact);
});
</script>

<template>
  <Reflection />
</template>

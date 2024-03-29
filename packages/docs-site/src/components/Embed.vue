<script setup lang="ts">
import { defineAsyncComponent } from "vue";

const Embed = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  const { Embed } = await import("@penrose/components");
  return applyPureReactInVue(Embed);
});
const props = defineProps([
  "domain",
  "substance",
  "style",
  "variation",
  "interactive",
]);
</script>

<template>
  <Embed
    :domain="props.domain"
    :style="props.style"
    :substance="props.substance"
    :variation="props.variation"
    :interactive="props.interactive"
  />
</template>

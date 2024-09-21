<script setup lang="ts">
import { defineAsyncComponent } from "vue";
import PoolReact from "./pool";

const Pool = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  return applyPureReactInVue(PoolReact);
});
</script>

<template>
  <Pool />
</template>

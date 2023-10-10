<script ts>
import { defineComponent } from "vue";
export default defineComponent({
  props: ["pr"],
});
</script>

<template>
  <span class="pr">
    <a
      :key="pr"
      :href="`https://github.com/penrose/penrose/pull/${pr}`"
      class="m-0"
      rel="noopener noreferrer"
      :aria-label="`PR ${pr} on GitHub`"
    >
      <img :src="`https://img.shields.io/badge/PR-${pr}-blue`" class="avatar" />
    </a>
  </span>
</template>

<style>
.pr {
  display: inline-block;
  height: 1em;
}
</style>

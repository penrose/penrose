<script ts>
import { defineComponent } from "vue";
export default defineComponent({
  props: ["issue"],
});
</script>

<template>
  <span class="issue">
    <a
      :key="pr"
      :href="`https://github.com/penrose/penrose/issue/${pr}`"
      class="m-0"
      rel="noopener noreferrer"
      :aria-label="`Issue ${pr} on GitHub`"
    >
      <img
        :src="`https://img.shields.io/badge/Issue-${issue}-green`"
        width="50"
        class="avatar"
      />
    </a>
  </span>
</template>

<style>
.issue {
  display: inline-block;
  width: 50px;
}
</style>

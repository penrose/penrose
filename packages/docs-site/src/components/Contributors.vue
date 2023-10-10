<script setup lang="ts">
import { data as contributors } from "../contributors.data.js";
</script>

<template>
  <div class="contributor-container">
    <a
      v-for="id in contributors"
      :key="id"
      :href="`https://github.com/${id}`"
      class="m-0"
      rel="noopener noreferrer"
      :aria-label="`${id} on GitHub`"
    >
      <img
        loading="lazy"
        :src="`https://github.com/${id}.png`"
        width="50"
        height="50"
        class="avatar"
        :alt="`${id}'s avatar`"
      />
    </a>
  </div>
</template>

<style>
.contributor-container {
  display: flex;
  flex-wrap: wrap;
  gap: 2px;
  justify-content: center;
}
.avatar {
  width: 50px;
  height: 50px;
  border-radius: 100%;
}
</style>

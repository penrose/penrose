<!-- https://github.com/vuejs/blog/blob/main/.vitepress/theme/Date.vue -->

<script setup lang="ts">
import type { Post } from "./posts.data.js";

const props = defineProps<{ date: Post["date"] }>();

function getDateTime() {
  return new Date(props.date.time).toISOString();
}
</script>

<template>
  <dl>
    <dt class="sr-only">Published on</dt>
    <dd class="text-base leading-6 font-medium dark:text-gray-500">
      <time :datetime="getDateTime()">{{ date.string }}</time>
    </dd>
  </dl>
</template>

<script setup lang="ts">
defineProps<{
  name: "flip" | "shuffle" | "reset" | "copy" | "download" | "back";
}>();
const paths = {
  flip: [
    "M7 4H5a2 2 0 0 0-2 2v12a2 2 0 0 0 2 2h2",
    "M17 4h2a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2h-2",
    "M10 3h4v18h-4z",
    "m17 8 3 3-3 3",
    "M16 11h4",
  ],
  shuffle: [
    "M3 7h3c4 0 8 10 12 10h3",
    "m18 14 3 3-3 3",
    "M3 17h3c1.3 0 2.5-1.1 3.7-2.7",
    "M14.3 9.7C15.5 8.1 16.7 7 18 7h3",
    "m18 4 3 3-3 3",
  ],
  reset: ["M4 10a8 8 0 1 1 1 7", "M4 4v6h6"],
  copy: ["M9 8h10a2 2 0 0 1 2 2v10H9z", "M15 4V2H3v12h2"],
  download: ["M12 3v12", "m7 10 5 5 5-5", "M4 16v5h16v-5"],
  back: ["m10 5-7 7 7 7", "M3 12h18"],
};
</script>
<template>
  <svg
    viewBox="0 0 24 24"
    fill="none"
    stroke="currentColor"
    stroke-width="1.6"
    stroke-linecap="round"
    stroke-linejoin="round"
    aria-hidden="true"
    focusable="false"
  >
    <path v-for="(d, index) in paths[name]" :key="index" :d="d" />
  </svg>
</template>

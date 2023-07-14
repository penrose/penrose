<script setup lang="ts">
defineProps<{ github: string }>();
</script>

<template>
  <div style="display: flex; gap: 5px; align-items: center">
    <img
      :src="`https://www.github.com/${github}.png`"
      width="30"
      style="border-radius: 50%"
    />
    <a :href="`https://github.com/${github}`">@{{ github }}</a>
  </div>
</template>

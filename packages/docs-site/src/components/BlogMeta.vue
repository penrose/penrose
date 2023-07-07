<script setup lang="ts">
defineProps<{ github: string; date: string }>();
</script>

<template>
  <div style="margin: 10px; display: flex; align-items: center; gap: 5px">
    <img
      :src="`https://www.github.com/${github}.png`"
      width="30"
      style="border-radius: 50%"
    />
    <a :href="`https://github.com/${github}`">@{{ github }}</a>
    {{ date }}
  </div>
</template>

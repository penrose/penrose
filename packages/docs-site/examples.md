---
layout: "page"
---

<script setup>
import registry from "@penrose/examples/dist/registry.js";
import Gallery from './src/components/Gallery.vue'
const trios = [...registry.entries()]
  .filter(([, meta]) => meta.trio && meta.gallery)
  .map(([id]) => id);
</script>

<Gallery :trios="trios" />

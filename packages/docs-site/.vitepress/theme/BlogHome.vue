<template>
  <div class="blog-layout">
    <div v-for="post in posts" :key="post.slug" class="post-preview">
      <h2>{{ post.title }}</h2>
      <p>Author: {{ frontmatter.author }}</p>
      <div v-html="post.excerpt" />
      <router-link :to="post.slug">Read more</router-link>
    </div>
  </div>
</template>

<script setup lang="ts">
import { data as posts } from "./posts.data.js";
import { useData } from "vitepress";

const { frontmatter } = useData();
</script>

<style scoped>
.blog-layout {
  display: flex;
  flex-direction: column;
}

.post-preview {
  margin-bottom: 20px;
}
</style>

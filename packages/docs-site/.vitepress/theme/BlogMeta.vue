<!-- adapted from https://github.com/vuejs/blog/blob/main/.vitepress/theme/Home.vue -->

<template>
  <h1>
    {{ frontmatter.title }}
  </h1>

  <div
    style="
      margin: 10px;
      margin-left: 0;
      display: flex;
      align-items: center;
      gap: 15px;
    "
  >
    <Author
      v-for="(author, index) in frontmatter.authors"
      :key="index"
      :github="author.github"
      :name="author.name"
    ></Author>
  </div>
</template>

<script setup lang="ts">
import { useData } from "vitepress";
import Author from "./Author.vue";
const { frontmatter } = useData();
</script>

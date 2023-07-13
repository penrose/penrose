<!-- adapted from https://github.com/vuejs/blog/blob/main/.vitepress/theme/Home.vue -->

<template>
  <h1>
    {{ frontmatter.title }}
  </h1>
  <Author :github="frontmatter.github" :name="frontmatter.name"></Author>
</template>

<script setup lang="ts">
import { useData } from "vitepress";
import Author from "./Author.vue";
const { frontmatter } = useData();
</script>

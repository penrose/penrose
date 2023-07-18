<script setup>
import DefaultTheme from "vitepress/theme";
import { useData } from "vitepress";
import DemoWrapper from "./DemoWrapper.vue";

const { page, frontmatter } = useData();
const { Layout } = DefaultTheme;
</script>

<template>
  <Layout>
    <template #home-hero-image><DemoWrapper /></template>
  </Layout>
</template>

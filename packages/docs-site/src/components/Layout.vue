<script setup>
import DefaultTheme from "vitepress/theme";
import DemoWrapper from "./DemoWrapper.vue";

const { Layout } = DefaultTheme;
</script>

<template>
  <Layout>
    <template #home-hero-image><DemoWrapper /></template>
  </Layout>
</template>

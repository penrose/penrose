<!-- adapted from https://github.com/vuejs/blog/blob/main/.vitepress/theme/Home.vue -->

<template>
  <main class="max-w-3xl mx-auto px-4 sm:px-6 xl:max-w-5xl xl:px-0">
    <div class="divide-y divide-gray-200 dark:divide-slate-200/5">
      <div class="pt-6 pb-8 space-y-2 md:space-y-5">
        <h1
          class="text-3xl leading-9 font-extrabold tracking-tight sm:text-4xl sm:leading-10 md:text-6xl md:leading-14"
        >
          {{ frontmatter.title }}
        </h1>
        <p class="text-lg leading-7">
          {{ frontmatter.subtext }}
        </p>
      </div>
      <ul class="black dark:divide-slate-200/5">
        <li class="py-6" v-for="{ title, url, date, excerpt } of posts">
          <article
            class="space-y-0 xl:grid xl:grid-cols-4 xl:space-y-0 xl:items-baseline"
          >
            <Date :date="date" />
            <div class="xl:col-span-3">
              <div class="space-y-3">
                <h2 class="text-xl leading-8 font-bold tracking-tight">
                  <a :href="url">{{ title }}</a>
                </h2>
                <div
                  v-if="excerpt"
                  class="prose dark:prose-invert max-w-none text-slate-900 dark:text-gray-400"
                  v-html="excerpt"
                ></div>
              </div>
              <div class="text-base leading-6 font-medium">
                <a
                  class="link text-[#40b4f7]"
                  aria-label="read more"
                  :href="url"
                  >Read more →</a
                >
              </div>
            </div>
          </article>
        </li>
      </ul>
    </div>
  </main>
</template>

<script setup lang="ts">
import { data as posts } from "./posts.data.js";
import { useData } from "vitepress";
import Date from "./Date.vue";
const { frontmatter } = useData();
</script>

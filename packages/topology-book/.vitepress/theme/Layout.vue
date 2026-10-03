<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref, watch } from "vue";
import { useData, useRoute, withBase } from "vitepress";
import BookReader from "../../../docs-site/src/elementary-topology/BookReader.vue";
import { chapters } from "./chapters";

const { page } = useData();
const route = useRoute();
const contentsOpen = ref(false);
const isHome = computed(() => page.value.relativePath === "index.md");
const isReader = computed(() => page.value.relativePath === "reader.md");
const activeChapter = computed(() =>
  chapters.find((chapter) =>
    page.value.relativePath.startsWith(`chapter-${chapter.directory}/`),
  ),
);
const isLibrary = computed(() => page.value.relativePath === "api.md");
const isIllustrations = computed(() =>
  ["further-illustrations.md", "loop-retracing.md"].includes(
    page.value.relativePath,
  ),
);
watch(
  () => route.path,
  () => {
    contentsOpen.value = false;
  },
);
const closeOnEscape = (event: KeyboardEvent) => {
  if (event.key === "Escape") contentsOpen.value = false;
};
onMounted(() => window.addEventListener("keydown", closeOnEscape));
onUnmounted(() => window.removeEventListener("keydown", closeOnEscape));
</script>

<template>
  <div class="topology-book" :class="{ 'reading-view': isReader }">
    <a class="skip-link" href="#book-main">Skip to the book</a>
    <header class="book-masthead">
      <a class="book-wordmark" :href="withBase('/')"
        >Elementary <em>Topology</em></a
      >
      <nav class="book-navigation" aria-label="Book navigation">
        <a
          :href="withBase('/reader')"
          :aria-current="isReader ? 'page' : undefined"
          >Read</a
        >
        <button
          :aria-expanded="contentsOpen"
          aria-controls="book-contents-menu"
          @click="contentsOpen = !contentsOpen"
        >
          Contents
          <span aria-hidden="true">{{ contentsOpen ? "−" : "+" }}</span>
        </button>
        <a
          :href="withBase('/further-illustrations')"
          :aria-current="isIllustrations ? 'page' : undefined"
          >Illustrations</a
        >
        <a
          :href="withBase('/api')"
          :aria-current="isLibrary ? 'page' : undefined"
          >Library</a
        >
      </nav>
    </header>

    <section
      v-if="contentsOpen"
      id="book-contents-menu"
      class="contents-menu"
      aria-label="Contents"
    >
      <div class="contents-menu-heading">
        <p class="eyebrow">The book</p>
        <a :href="withBase('/front-matter/contents')"
          >All sections <span aria-hidden="true">↗</span></a
        >
      </div>
      <ol class="chapter-grid">
        <li v-for="chapter in chapters" :key="chapter.directory">
          <a
            :href="withBase(`/chapter-${chapter.directory}/`)"
            :aria-current="activeChapter === chapter ? 'page' : undefined"
          >
            <span class="chapter-numeral">{{ chapter.number }}</span>
            <span>{{ chapter.title }}</span>
          </a>
        </li>
      </ol>
      <div class="contents-colophon">
        <a :href="withBase('/front-matter/')">Preface &amp; front matter</a>
        <a :href="withBase('/back-matter/appendix-on-infinite-products')"
          >Appendix</a
        >
        <a :href="withBase('/back-matter/index-of-symbols')">Symbols</a>
        <a :href="withBase('/back-matter/subject-index')">Index</a>
      </div>
    </section>

    <main id="book-main">
      <template v-if="isHome">
        <section class="book-title-page" aria-labelledby="book-title">
          <div class="title-page-copy">
            <p class="eyebrow">An illustrated edition</p>
            <h1 id="book-title">Elementary<br /><em>Topology</em></h1>
            <p class="book-author">
              Michael C. Gemignani <span>Second edition</span>
            </p>
            <p class="title-page-description">
              The original pages, with mathematical diagrams you can move,
              explore, and turn over to read their programs.
            </p>
            <a class="begin-reading" :href="withBase('/reader?scan=1')"
              >Open the book <span aria-hidden="true">→</span></a
            >
          </div>
          <a
            class="title-page-illustration"
            :href="withBase('/reader?scan=1')"
            aria-label="Explore the cover's interactive curve family"
          >
            <img
              :src="
                withBase(
                  '/elementary-topology/figures/figure-cover-rosette.svg',
                )
              "
              alt="A family of purple loops sharing a point inside a disk, recreated from the book's cover."
            />
          </a>
        </section>
        <section class="home-reader" aria-label="Explore the original pages">
          <div class="section-heading">
            <p class="eyebrow">Inside the book</p>
            <a :href="withBase('/reader')"
              >Focused reading <span aria-hidden="true">↗</span></a
            >
          </div>
          <BookReader />
        </section>
        <section class="motion-preview" aria-labelledby="motion-title">
          <div class="section-heading">
            <p id="motion-title" class="eyebrow">Ideas in motion</p>
            <a :href="withBase('/further-illustrations')"
              >More illustrations <span aria-hidden="true">↗</span></a
            >
          </div>
          <div class="motion-links">
            <a :href="withBase('/reader?page=222')"
              ><span class="motion-chapter">Chapter X</span
              ><strong>Finding a fixed point</strong
              ><span>Advance a contraction, one iterate at a time.</span
              ><span class="motion-arrow" aria-hidden="true">→</span></a
            >
            <a :href="withBase('/reader?page=246')"
              ><span class="motion-chapter">Chapter XI</span
              ><strong>A loop and its inverse</strong
              ><span>Watch the two journeys shorten to their basepoint.</span
              ><span class="motion-arrow" aria-hidden="true">→</span></a
            >
          </div>
        </section>
        <section
          id="contents"
          class="home-contents"
          aria-labelledby="contents-title"
        >
          <div class="section-heading">
            <h2 id="contents-title">Contents</h2>
            <a :href="withBase('/front-matter/contents')"
              >Browse every section <span aria-hidden="true">↗</span></a
            >
          </div>
          <ol class="chapter-grid">
            <li v-for="chapter in chapters" :key="chapter.directory">
              <a :href="withBase(`/chapter-${chapter.directory}/`)"
                ><span class="chapter-numeral">{{ chapter.number }}</span
                ><span>{{ chapter.title }}</span></a
              >
            </li>
          </ol>
          <div class="contents-colophon">
            <a :href="withBase('/front-matter/')">Front matter</a
            ><a :href="withBase('/back-matter/appendix-on-infinite-products')"
              >Appendix</a
            ><a :href="withBase('/back-matter/index-of-symbols')">Symbols</a
            ><a :href="withBase('/back-matter/subject-index')">Index</a>
          </div>
        </section>
        <section class="edition-note">
          <p class="eyebrow">About this edition</p>
          <p>
            All available source pages are preserved. Thirty-one printed page
            positions are missing from the supplied scan; the gaps remain
            explicit. The new illustrations are identified separately from the
            source figures.
          </p>
          <a :href="withBase('/api')"
            >Explore the reusable mathematical library
            <span aria-hidden="true">→</span></a
          >
        </section>
      </template>
      <section
        v-else-if="isReader"
        class="focused-reader"
        aria-label="Source book reader"
      >
        <BookReader />
      </section>
      <article v-else class="book-prose">
        <div v-if="activeChapter" class="chapter-context">
          <a :href="withBase(`/chapter-${activeChapter.directory}/`)"
            >Chapter {{ activeChapter.number }}
            <span aria-hidden="true">·</span> {{ activeChapter.title }}</a
          ><a :href="withBase(`/reader?page=${activeChapter.page}`)"
            >Original pages <span aria-hidden="true">↗</span></a
          >
        </div>
        <Content />
      </article>
    </main>
    <footer class="book-colophon">
      <a :href="withBase('/')">Elementary Topology</a
      ><span>Mathematical illustrations made with Penrose</span
      ><a :href="withBase('/front-matter/publication-and-dedication')"
        >Source &amp; publication</a
      >
    </footer>
  </div>
</template>

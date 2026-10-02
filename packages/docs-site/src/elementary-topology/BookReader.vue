<script setup lang="ts">
import { computed, onMounted, ref, watch } from "vue";
import { withBase } from "vitepress";
import figureManifest from "../../../../docs/elementary-topology/figures.json";
import InteractiveFigure from "./InteractiveFigure.vue";
import type { BookFigure } from "./figure-programs";

type SourcePage = {
  pdfPage: number;
  printedPage: string | null;
  width: number;
  height: number;
  image: string;
};
type Book = { pages: SourcePage[]; missingPrintedPages: number[] };
const figures = figureManifest.figures as BookFigure[];
const figureTarget = (id: string) =>
  `book-figure-${id.replace(/[^\w-]/g, "-")}`;
const book = ref<Book | null>(null);
const sourceUnavailable = ref(false);
const pdfPage = ref(27);
const selectedChapter = ref("2");
const chapters = [
  { id: "front", title: "Front matter", print: null },
  { id: "1", title: "1 · Preliminaries", print: 1 },
  { id: "2", title: "2 · Metric Spaces", print: 16 },
  { id: "3", title: "3 · Topologies", print: 40 },
  { id: "4", title: "4 · Derived Topological Spaces. Continuity", print: 64 },
  { id: "5", title: "5 · The Separation Axioms", print: 91 },
  { id: "6", title: "6 · Convergence", print: 113 },
  { id: "7", title: "7 · Covering Properties", print: 142 },
  { id: "8", title: "8 · More About Compactness", print: 163 },
  { id: "9", title: "9 · Connectedness", print: 183 },
  { id: "10", title: "10 · Metrizability. Complete Metric Spaces", print: 208 },
  { id: "11", title: "11 · Introduction to Homotopy Theory", print: 233 },
  { id: "appendix", title: "Appendix on Infinite Products", print: 261 },
  { id: "symbols", title: "Index of Symbols", print: 265 },
  { id: "index", title: "Index", print: 267 },
];
const current = computed(() => book.value?.pages[pdfPage.value - 1]);
watch(current, (page) => {
  if (!page) return;
  const printed = Number(page?.printedPage);
  selectedChapter.value = /^\d+$/.test(page?.printedPage ?? "")
    ? [...chapters]
        .reverse()
        .find((chapter) => chapter.print !== null && chapter.print <= printed)!
        .id
    : "front";
  const url = new URL(window.location.href);
  if (page.printedPage) {
    url.searchParams.set("page", page.printedPage);
    url.searchParams.delete("scan");
  } else {
    url.searchParams.set("scan", String(page.pdfPage));
    url.searchParams.delete("page");
  }
  window.history.replaceState(window.history.state, "", url);
});
const replacements = computed(() =>
  figures.filter(
    (figure) =>
      figure.pdfPage === pdfPage.value &&
      figure.status === "reviewed" &&
      figure.sourceBox &&
      figure.svg,
  ),
);
const pending = computed(() =>
  figures.filter(
    (figure) => figure.pdfPage === pdfPage.value && figure.status === "pending",
  ),
);
const missingBefore = computed(() => {
  if (!book.value || !current.value) return [];
  const previous = book.value.pages[pdfPage.value - 2];
  const from = Number(previous?.printedPage);
  const to = Number(current.value.printedPage);
  return Number.isFinite(from) && Number.isFinite(to)
    ? book.value.missingPrintedPages.filter((page) => page > from && page < to)
    : [];
});
const pageLabel = (page: SourcePage) =>
  page.printedPage
    ? `Printed page ${page.printedPage}`
    : `Front matter · scan ${page.pdfPage}`;
const selectChapter = () => {
  const chapter = chapters.find((item) => item.id === selectedChapter.value);
  if (!chapter || !book.value) return;
  if (chapter.print === null) pdfPage.value = 1;
  else {
    // The source may lack the chapter opening. Select its first supplied page.
    const first = book.value.pages.find(
      (page) =>
        /^\d+$/.test(page.printedPage ?? "") &&
        Number(page.printedPage) >= chapter.print!,
    );
    if (first) pdfPage.value = first.pdfPage;
  }
};
const move = (step: number) => {
  if (book.value)
    pdfPage.value = Math.min(
      book.value.pages.length,
      Math.max(1, pdfPage.value + step),
    );
};
const turnWithKeyboard = (event: KeyboardEvent) => {
  if (
    event.target instanceof HTMLElement &&
    ["SELECT", "INPUT", "TEXTAREA"].includes(event.target.tagName)
  )
    return;
  if (event.key === "ArrowLeft" || event.key === "ArrowRight") {
    event.preventDefault();
    move(event.key === "ArrowLeft" ? -1 : 1);
  }
};
const placement = (figure: BookFigure) => {
  const box = figure.sourceBox!;
  return {
    left: `${box[0] * 100}%`,
    top: `${box[1] * 100}%`,
    width: `${box[2] * 100}%`,
    height: `${box[3] * 100}%`,
    clipPath: figure.sourceClip
      ? `polygon(${figure.sourceClip
          .map(([x, y]) => `${x * 100}% ${y * 100}%`)
          .join(",")})`
      : undefined,
  };
};

onMounted(async () => {
  try {
    const response = await fetch(
      withBase("/elementary-topology/source/book.json"),
    );
    if (!response.ok) throw new Error("Source unavailable");
    book.value = await response.json();
    const params = new URLSearchParams(window.location.search);
    const query = params.get("page");
    const scan = Number(params.get("scan"));
    const target =
      (query
        ? book.value?.pages.find((page) => page.printedPage === query)
        : undefined) ??
      (query && /^\d+$/.test(query)
        ? book.value?.pages.find(
            (page) =>
              /^\d+$/.test(page.printedPage ?? "") &&
              Number(page.printedPage) >= Number(query),
          )
        : undefined) ??
      (!query && Number.isSafeInteger(scan) && scan > 0
        ? book.value?.pages.find((page) => page.pdfPage === scan)
        : undefined);
    if (target) pdfPage.value = target.pdfPage;
  } catch {
    sourceUnavailable.value = true;
  }
});
</script>

<template>
  <div class="book-reader" tabindex="0" @keydown="turnWithKeyboard">
    <p v-if="sourceUnavailable" role="status">
      Book pages are unavailable in this preview. The reviewed figures and
      transcribed sections are available from the book index.
    </p>
    <p v-else-if="!book" role="status">Loading the book…</p>
    <template v-else>
      <div class="reader-controls">
        <label
          >Chapter
          <select v-model="selectedChapter" @change="selectChapter">
            <option
              v-for="chapter in chapters"
              :key="chapter.id"
              :value="chapter.id"
            >
              {{ chapter.title }}
            </option>
          </select></label
        >
        <label
          >Page
          <select v-model.number="pdfPage">
            <option
              v-for="page in book.pages"
              :key="page.pdfPage"
              :value="page.pdfPage"
            >
              {{ pageLabel(page) }}
            </option>
          </select></label
        >
        <div class="page-buttons">
          <button
            :disabled="pdfPage === 1"
            aria-label="Previous supplied page"
            @click="move(-1)"
          >
            ←
          </button>
          <button
            :disabled="pdfPage === book.pages.length"
            aria-label="Next supplied page"
            @click="move(1)"
          >
            →
          </button>
        </div>
      </div>
      <p v-if="missingBefore.length" class="source-gap" role="status">
        Printed {{ missingBefore.length === 1 ? "page" : "pages" }}
        {{ missingBefore.join(", ") }}
        {{ missingBefore.length === 1 ? "is" : "are" }} absent from the supplied
        scan.
      </p>
      <div
        v-if="current"
        :key="current.pdfPage"
        class="source-page"
        :style="{ aspectRatio: `${current.width} / ${current.height}` }"
      >
        <img
          class="page-image"
          :src="withBase(`/elementary-topology/source/${current.image}`)"
          :alt="`${pageLabel(current)} of Elementary Topology, second edition`"
        />
        <div
          v-for="figure in replacements"
          :key="figure.id"
          class="replacement"
          :style="placement(figure)"
          :id="figureTarget(figure.id)"
        >
          <img
            :src="withBase(`/elementary-topology/figures/${figure.svg}`)"
            alt=""
            aria-hidden="true"
          />
        </div>
      </div>
      <p class="page-status">
        {{ replacements.length }}
        {{ replacements.length === 1 ? "figure" : "figures" }} replaced by
        Penrose on this page.<span v-if="pending.length">
          {{ pending.length }}
          {{
            pending.length === 1 ? "illustration awaits" : "illustrations await"
          }}
          reproduction.</span
        >
      </p>
      <InteractiveFigure
        v-for="figure in replacements"
        :key="`${pdfPage}-${figure.id}`"
        :figure="figure"
        :canvas-target="`#${figureTarget(figure.id)}`"
      />
    </template>
  </div>
</template>

<style scoped>
.book-reader {
  margin: 1.5rem 0;
}
.reader-controls {
  display: flex;
  flex-wrap: wrap;
  align-items: end;
  gap: 0.8rem;
  margin-bottom: 1rem;
}
.reader-controls label {
  display: flex;
  flex: 1 1 11rem;
  flex-direction: column;
  font-size: 0.8rem;
  gap: 0.25rem;
}
.reader-controls select {
  max-width: 100%;
  border: 1px solid var(--vp-c-divider);
  border-radius: 0.3rem;
  padding: 0.4rem;
  background: var(--vp-c-bg);
}
.page-buttons {
  display: flex;
  gap: 0.4rem;
}
.page-buttons button {
  border: 1px solid var(--vp-c-divider);
  border-radius: 0.3rem;
  padding: 0.35rem 0.7rem;
}
.page-buttons button:disabled {
  opacity: 0.35;
}
.source-page {
  position: relative;
  width: 100%;
  background: white;
}
.page-image {
  display: block;
  width: 100%;
  height: 100%;
}
.replacement {
  display: flex;
  position: absolute;
  align-items: center;
  justify-content: center;
  background: white;
}
.replacement img {
  width: 100%;
  height: 100%;
  object-fit: contain;
}
.replacement > img {
  position: absolute;
  inset: 0;
}
.page-status,
.source-gap {
  font-size: 0.85rem;
  color: var(--vp-c-text-2);
}
.source-gap {
  padding: 0.5rem 0.75rem;
  border-left: 2px solid #d87937;
}
</style>

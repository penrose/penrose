<script setup lang="ts">
import { computed, onMounted, ref, watch } from "vue";
import { useData, withBase } from "vitepress";
import { transcriptionLinksForSourcePage } from "./transcription-links.generated";
import figureManifest from "../../../../docs/elementary-topology/figures.json";
import InteractiveFigure from "./InteractiveFigure.vue";
import ReadingAdditions from "./ReadingAdditions.vue";
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
const { page: routePage } = useData();
const figureTarget = (id: string) =>
  `book-figure-${id.replace(/[^\w-]/g, "-")}`;
const book = ref<Book | null>(null);
const sourceUnavailable = ref(false);
const pdfPage = ref(27);
const selectedChapter = ref("2");
const activeFigureId = ref<string | null>(null);
const savedPlaceKey = "elementary-topology-reading-place";
const rememberedPage = () => {
  try {
    return Number(localStorage.getItem(savedPlaceKey));
  } catch {
    return 0;
  }
};
const chapters = [
  { id: "front", title: "Front matter", print: null },
  { id: "1", title: "1 · Preliminaries", print: 1 },
  { id: "2", title: "2 · Metric Spaces", print: 16 },
  { id: "3", title: "3 · Topologies", print: 40 },
  { id: "4", title: "4 · Derived Spaces. Continuity", print: 64 },
  { id: "5", title: "5 · The Separation Axioms", print: 91 },
  { id: "6", title: "6 · Convergence", print: 113 },
  { id: "7", title: "7 · Covering Properties", print: 142 },
  { id: "8", title: "8 · More About Compactness", print: 163 },
  { id: "9", title: "9 · Connectedness", print: 183 },
  { id: "10", title: "10 · Metrizability. Complete Spaces", print: 208 },
  { id: "11", title: "11 · Homotopy Theory", print: 233 },
  { id: "appendix", title: "Appendix on Infinite Products", print: 261 },
  { id: "symbols", title: "Index of Symbols", print: 265 },
  { id: "index", title: "Index", print: 267 },
];
const current = computed(() => book.value?.pages[pdfPage.value - 1]);
const textLinks = computed(() =>
  current.value
    ? transcriptionLinksForSourcePage(
        current.value.printedPage,
        pdfPage.value,
        routePage.value.relativePath,
      )
    : [],
);
const replacements = computed(() =>
  figures.filter(
    (figure) =>
      figure.pdfPage === pdfPage.value &&
      figure.status === "reviewed" &&
      figure.sourceBox &&
      figure.svg,
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
    ? `Page ${page.printedPage}`
    : `Front matter · ${page.pdfPage}`;
watch(current, (page) => {
  activeFigureId.value = null;
  if (!page) return;
  const printed = Number(page.printedPage);
  selectedChapter.value = /^\d+$/.test(page.printedPage ?? "")
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
  try {
    localStorage.setItem(savedPlaceKey, String(page.pdfPage));
  } catch {
    /* Reading still works when browser storage is unavailable. */
  }
});
const selectChapter = () => {
  const chapter = chapters.find((item) => item.id === selectedChapter.value);
  if (!chapter || !book.value) return;
  if (chapter.print === null) pdfPage.value = 1;
  else {
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
  if (event.ctrlKey || event.metaKey || event.altKey || event.defaultPrevented)
    return;
  if (
    event.target instanceof Element &&
    event.target.closest(
      "button, select, input, textarea, pre, [contenteditable], [role=slider], .figure-card",
    )
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
  };
};
const sourceClip = (figure: BookFigure) =>
  figure.sourceClip
    ? `polygon(${figure.sourceClip
        .map(([x, y]) => `${x * 100}% ${y * 100}%`)
        .join(",")})`
    : undefined;
const handleFlip = (id: string, flipped: boolean) => {
  if (flipped) activeFigureId.value = id;
  else if (activeFigureId.value === id) activeFigureId.value = null;
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
    const scan = Number(params.get("scan")) || (!query ? rememberedPage() : 0);
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
  <section
    class="book-reader"
    aria-label="Interactive book reader"
    tabindex="0"
    @keydown="turnWithKeyboard"
  >
    <div v-if="sourceUnavailable" class="reader-empty" role="status">
      <span class="reader-kicker">The reading edition</span>
      <p>
        Book pages are unavailable in this preview. The illustrations and
        transcribed chapters remain available in the contents.
      </p>
    </div>
    <div v-else-if="!book" class="reader-empty" role="status">
      Opening the book…
    </div>
    <template v-else>
      <div class="reader-controls">
        <label class="chapter-selector"
          ><span class="reader-kicker">Chapter</span>
          <select
            v-model="selectedChapter"
            aria-label="Chapter"
            @change="selectChapter"
          >
            <option
              v-for="chapter in chapters"
              :key="chapter.id"
              :value="chapter.id"
            >
              {{ chapter.title }}
            </option>
          </select>
        </label>
        <div class="page-turner">
          <button
            :disabled="pdfPage === 1"
            aria-label="Previous supplied page"
            @click="move(-1)"
          >
            <svg viewBox="0 0 24 24" aria-hidden="true">
              <path d="m14 6-6 6 6 6" />
            </svg>
          </button>
          <label class="page-selector"
            ><span class="sr-only">Page</span
            ><select v-model.number="pdfPage" aria-label="Page">
              <option
                v-for="page in book.pages"
                :key="page.pdfPage"
                :value="page.pdfPage"
              >
                {{ pageLabel(page) }}
              </option>
            </select></label
          >
          <button
            :disabled="pdfPage === book.pages.length"
            aria-label="Next supplied page"
            @click="move(1)"
          >
            <svg viewBox="0 0 24 24" aria-hidden="true">
              <path d="m10 6 6 6-6 6" />
            </svg>
          </button>
        </div>
      </div>
      <p v-if="missingBefore.length" class="source-gap" role="status">
        The supplied scan skips
        {{ missingBefore.length === 1 ? "page" : "pages" }}
        {{ missingBefore.join(", ") }}.
      </p>
      <div class="reading-desk">
        <div
          v-if="current"
          :key="current.pdfPage"
          class="source-page"
          :style="{ aspectRatio: `${current.width} / ${current.height}` }"
        >
          <img
            class="page-image"
            :src="withBase(`/elementary-topology/source/${current.image}`)"
            :alt="`${pageLabel(
              current,
            )} of Elementary Topology, second edition`"
          />
          <div
            v-for="figure in replacements"
            :key="figure.id"
            :id="figureTarget(figure.id)"
            class="replacement"
            :class="{ 'is-flipped': activeFigureId === figure.id }"
            :style="placement(figure)"
          >
            <div
              v-show="activeFigureId !== figure.id"
              class="source-mask"
              :style="{ clipPath: sourceClip(figure) }"
            >
              <img
                :src="withBase(`/elementary-topology/figures/${figure.svg}`)"
                alt=""
                aria-hidden="true"
              />
            </div>
            <InteractiveFigure
              :figure="figure"
              :embedded="true"
              :overlay-clip="sourceClip(figure)"
              :active="activeFigureId === figure.id"
              @flip="handleFlip(figure.id, $event)"
            />
          </div>
        </div>
      </div>
      <div class="reader-footnote">
        <span v-if="replacements.length"
          ><span class="interactive-dot" />Drag a figure. Flip it to read its
          program.</span
        ><span v-else>Elementary Topology · Second edition</span
        ><a
          v-if="textLinks.length === 1"
          class="text-link"
          :href="withBase(textLinks[0].href)"
          :aria-label="`Read ${textLinks[0].title} as text`"
          >Read text <span aria-hidden="true">↗</span></a
        >
        <details v-else-if="textLinks.length > 1" class="text-sections">
          <summary>Read text <span aria-hidden="true">⌄</span></summary>
          <ul>
            <li v-for="link in textLinks" :key="link.href">
              <a :href="withBase(link.href)">{{ link.title }}</a>
            </li>
          </ul>
        </details>
        <span class="reader-progress"
          >{{ pdfPage }} / {{ book.pages.length }}</span
        >
      </div>
      <ReadingAdditions :key="pdfPage" :pdf-page="pdfPage" />
      <nav class="reader-bottom" aria-label="Continue reading">
        <button :disabled="pdfPage === 1" @click="move(-1)">
          ← Previous page</button
        ><span>{{ current ? pageLabel(current) : "" }}</span
        ><button :disabled="pdfPage === book.pages.length" @click="move(1)">
          Next page →
        </button>
      </nav>
    </template>
  </section>
</template>

<style scoped>
.book-reader {
  --book-ink: #302f29;
  --book-muted: #77766c;
  --book-rule: #dedcd2;
  --book-accent: #ad592d;
  margin: 0 auto;
  max-width: 960px;
  color: var(--book-ink);
}
.reader-controls {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 1.5rem;
  border-top: 1px solid var(--book-rule);
  border-bottom: 1px solid var(--book-rule);
  padding: 0.9rem 0.3rem;
  margin-bottom: 1.8rem;
}
.chapter-selector {
  display: grid;
  min-width: 0;
  gap: 0.2rem;
}
.reader-kicker {
  font: 600 0.64rem/1.5 sans-serif;
  letter-spacing: 0.18em;
  text-transform: uppercase;
  color: var(--book-muted);
}
.reader-controls select {
  border: 0;
  background: transparent;
  color: var(--book-ink);
  font: inherit;
  font-family: Georgia, "Times New Roman", serif;
  cursor: pointer;
  max-width: 100%;
  padding: 0.15rem 1.3rem 0.15rem 0;
}
.chapter-selector select {
  font-size: 1.15rem;
}
.page-turner {
  display: flex;
  align-items: center;
  gap: 0.55rem;
  flex: 0 0 auto;
}
.page-selector select {
  font-size: 0.95rem;
  width: 7.2rem;
  text-align: center;
}
.page-turner button {
  width: 2.3rem;
  height: 2.3rem;
  border: 1px solid var(--book-rule);
  border-radius: 50%;
  background: transparent;
  display: grid;
  place-items: center;
  color: var(--book-ink);
  cursor: pointer;
}
.page-turner svg {
  width: 17px;
  height: 17px;
  fill: none;
  stroke: currentColor;
  stroke-width: 1.4;
}
button:hover:not(:disabled) {
  color: var(--book-accent);
  border-color: var(--book-accent);
}
button:disabled {
  opacity: 0.3;
  cursor: default;
}
button:focus-visible,
select:focus-visible {
  outline: 2px solid var(--book-accent);
  outline-offset: 4px;
}
.reading-desk {
  padding: 0 1rem;
}
.source-page {
  position: relative;
  width: 100%;
  background: white;
  box-shadow:
    0 1px 2px #38352a10,
    0 12px 36px #38352a0c;
  isolation: isolate;
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
  overflow: visible;
}
.replacement.is-flipped {
  z-index: 40;
}
.source-mask {
  position: absolute;
  inset: 0;
  background: white;
}
.source-mask img {
  display: block;
  width: 100%;
  height: 100%;
  object-fit: contain;
}
.reader-footnote {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 1rem;
  padding: 0.8rem 1rem 0;
  font: 0.72rem/1.6 sans-serif;
  color: var(--book-muted);
}
.text-link,
.text-sections summary {
  font:
    0.8rem/1.6 Georgia,
    serif;
  color: var(--book-accent);
  cursor: pointer;
}
.text-link {
  margin-left: auto;
}
.text-link:hover {
  text-decoration: underline;
}
.text-sections {
  position: relative;
  margin-left: auto;
}
.text-sections ul {
  position: absolute;
  z-index: 50;
  bottom: 1.8rem;
  right: 0;
  width: 17rem;
  max-width: 70vw;
  padding: 0.6rem;
  margin: 0;
  list-style: none;
  background: #fffdf8;
  border: 1px solid var(--book-rule);
  box-shadow: 0 6px 24px #302f2910;
}
.text-sections a {
  display: block;
  padding: 0.35rem;
  color: var(--book-ink);
  font:
    0.85rem/1.5 Georgia,
    serif;
}
.reader-progress {
  white-space: nowrap;
}
.interactive-dot {
  display: inline-block;
  width: 5px;
  height: 5px;
  border-radius: 50%;
  background: var(--book-accent);
  margin: 0 0.5rem 0.12rem 0;
}
.reader-bottom {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 1rem;
  padding: 1rem 0.3rem;
  border-top: 1px solid var(--book-rule);
  margin-top: 2rem;
  font:
    0.86rem/1.5 Georgia,
    serif;
}
.reader-bottom button {
  cursor: pointer;
  background: none;
  border: 0;
  padding: 0.4rem 0;
}
.reader-bottom span {
  color: var(--book-muted);
  font-size: 0.75rem;
}
.source-gap {
  margin: -1rem 0 1.2rem;
  font: 0.75rem/1.6 sans-serif;
  color: var(--book-muted);
}
.reader-empty {
  min-height: 20rem;
  display: grid;
  align-content: center;
  text-align: center;
  max-width: 38rem;
  margin: auto;
}
.sr-only {
  position: absolute;
  width: 1px;
  height: 1px;
  padding: 0;
  margin: -1px;
  overflow: hidden;
  clip: rect(0, 0, 0, 0);
  white-space: nowrap;
  border: 0;
}
@media (max-width: 600px) {
  .reader-controls {
    gap: 0.5rem;
    padding: 0.8rem 0;
    margin-bottom: 1rem;
  }
  .chapter-selector {
    flex: 1 1 0;
  }
  .chapter-selector select {
    font-size: 0.87rem;
    width: 100%;
  }
  .page-turner {
    gap: 0.2rem;
  }
  .page-turner button {
    width: 1.9rem;
    height: 1.9rem;
  }
  .page-selector select {
    width: 5.2rem;
    padding-right: 0.3rem;
    font-size: 0.8rem;
  }
  .reading-desk {
    padding: 0;
  }
  .reader-footnote {
    flex-wrap: wrap;
    gap: 0.2rem 0.8rem;
    padding: 0.65rem 0 0;
    font-size: 0.65rem;
  }
}
@media print {
  .reader-controls,
  .reader-bottom,
  .reader-footnote {
    display: none;
  }
  .source-page {
    box-shadow: none;
  }
}
</style>

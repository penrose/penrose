<script setup lang="ts">
import {
  computed,
  defineAsyncComponent,
  nextTick,
  onBeforeUnmount,
  onMounted,
  ref,
  watch,
} from "vue";
import { withBase } from "vitepress";
import {
  readFigureProgram,
  type BookFigure,
  type ProgramKind,
} from "./figure-programs";

const props = defineProps<{ figure: BookFigure; canvasTarget?: string }>();
const LiveFigure = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  const { default: component } = await import("./LiveFigure");
  return applyPureReactInVue(component);
});
const mounted = ref(false);
const seed = ref(`gemignani-${props.figure.id}`);
const draftSeed = ref(seed.value);
const generation = ref(0);
const sampleLayout = ref(false);
const ready = ref(false);
const failed = ref(false);
const host = ref<HTMLElement | null>(null);
const kind = ref<ProgramKind>("substance");
const fullModule = ref(false);
const code = ref("");
const sourcePath = ref("");
const sourceLoading = ref(false);
const sourceError = ref(false);
const copyStatus = ref("");
let observer: MutationObserver | undefined;
let sourceRevision = 0;

const label = computed(() =>
  /^\d/.test(props.figure.id)
    ? `Figure ${props.figure.id}`
    : props.figure.title ?? props.figure.description,
);
const dragTarget = computed(() =>
  props.figure.interaction === "construction"
    ? "the region"
    : props.figure.interaction === "mixed"
    ? "labels or the marked region"
    : "labels",
);
const displayedCode = computed(() => {
  if (
    kind.value !== "substance" ||
    fullModule.value ||
    !props.figure.substanceLines
  )
    return code.value;
  const [start, end] = props.figure.substanceLines;
  return code.value
    .split("\n")
    .slice(start - 1, end)
    .join("\n");
});
const sourceTitle = computed(() =>
  kind.value === "substance"
    ? props.figure.substanceFactory ?? "Substance module"
    : kind.value === "style"
    ? "Reusable Style module"
    : "Domain module",
);
const invocation = computed(
  () =>
    `${props.figure.buildFactory ?? "buildFigure"}(${(
      props.figure.buildArguments ?? []
    )
      .map((argument) => JSON.stringify(argument))
      .join(", ")})`,
);
const substanceInvocation = computed(
  () =>
    `${props.figure.substanceFactory ?? "substance"}(${(
      props.figure.substanceArguments ??
      props.figure.buildArguments ??
      []
    )
      .map((argument) => JSON.stringify(argument))
      .join(", ")})`,
);
const reseed = (sample = true) => {
  seed.value = draftSeed.value || `gemignani-${props.figure.id}`;
  generation.value++;
  sampleLayout.value = sample;
  ready.value = false;
  failed.value = false;
};
const resample = () => {
  draftSeed.value = `figure-${props.figure.id}-${crypto.randomUUID()}`;
  reseed(true);
};
const reset = () => {
  draftSeed.value = `gemignani-${props.figure.id}`;
  reseed(false);
};
const markFailed = () => {
  failed.value = true;
};
const observe = async () => {
  await nextTick();
  observer?.disconnect();
  if (!host.value) return;
  observer = new MutationObserver(() => {
    if (host.value?.querySelector("svg")) {
      ready.value = true;
      observer?.disconnect();
    }
  });
  observer.observe(host.value, { childList: true, subtree: true });
  if (host.value.querySelector("svg")) ready.value = true;
};
const loadSource = async () => {
  const revision = ++sourceRevision;
  sourceLoading.value = true;
  sourceError.value = false;
  copyStatus.value = "";
  try {
    const program = await readFigureProgram(props.figure, kind.value);
    if (revision !== sourceRevision) return;
    code.value = program.source;
    sourcePath.value = program.path;
  } catch {
    if (revision === sourceRevision) sourceError.value = true;
  } finally {
    if (revision === sourceRevision) sourceLoading.value = false;
  }
};
const copy = async () => {
  try {
    await navigator.clipboard.writeText(displayedCode.value);
    copyStatus.value = "Copied.";
  } catch {
    copyStatus.value = "Select the source text to copy it.";
  }
};
const download = () => {
  const url = URL.createObjectURL(
    new Blob([code.value], { type: "text/plain;charset=utf-8" }),
  );
  const link = document.createElement("a");
  link.href = url;
  link.download = sourcePath.value.split("/").at(-1)!;
  link.click();
  URL.revokeObjectURL(url);
};
watch(kind, loadSource);
watch(generation, observe);
onMounted(() => {
  mounted.value = true;
  loadSource();
  observe();
});
onBeforeUnmount(() => observer?.disconnect());
</script>

<template>
  <section
    class="interactive-figure"
    :aria-label="`${label} controls and source`"
  >
    <Teleport
      :to="canvasTarget || 'body'"
      :disabled="!mounted || !canvasTarget"
    >
      <div
        class="figure-canvas"
        :class="{ embedded: canvasTarget }"
        :aria-busy="mounted && !ready && !failed"
      >
        <img
          v-if="figure.svg"
          v-show="!ready"
          class="static-figure"
          :src="withBase(`/elementary-topology/figures/${figure.svg}`)"
          :alt="figure.description"
        />
        <div
          v-if="mounted && !failed"
          ref="host"
          class="live-figure"
          :class="{ visible: ready }"
          :aria-label="`${label}: drag ${dragTarget} to adjust the layout`"
          role="group"
        >
          <LiveFigure
            :key="generation"
            v-bind="{
              figure,
              seed,
              sampleLayout,
              readyCallback: observe,
              failureCallback: markFailed,
            }"
          />
        </div>
      </div>
    </Teleport>
    <details class="figure-inspector">
      <summary>{{ label }} · program and layout</summary>
      <p>
        Drag {{ dragTarget }} to adjust the layout. You can also focus a drag
        handle and use the arrow keys. The mathematical relationships stay
        fixed.
      </p>
      <form class="layout-controls" @submit.prevent="reseed()">
        <label
          >Seed
          <input
            v-model="draftSeed"
            :aria-label="`${label} seed`"
            spellcheck="false"
        /></label>
        <button type="submit">Apply seed</button>
        <button type="button" @click="resample">Re-sample</button>
        <button type="button" @click="reset">Reset layout</button>
      </form>
      <p v-if="failed" class="figure-message" role="status">
        Interactive layout unavailable. Showing the reviewed figure.
      </p>
      <p v-else-if="mounted && !ready" class="figure-message" role="status">
        Preparing interactive layout…
      </p>
      <p v-else class="figure-message" role="status">Layout seed: {{ seed }}</p>
      <div class="program-controls">
        <label
          >Program
          <select v-model="kind" :aria-label="`${label} source program`">
            <option value="substance">Substance</option>
            <option value="style">Style</option>
            <option value="domain">Domain</option>
          </select></label
        >
        <label v-if="kind === 'substance' && figure.substanceLines"
          ><input v-model="fullModule" type="checkbox" /> Full module</label
        >
        <button
          type="button"
          :disabled="sourceLoading || sourceError"
          @click="copy"
        >
          Copy source
        </button>
        <button
          type="button"
          :disabled="sourceLoading || sourceError"
          @click="download"
        >
          Download module
        </button>
      </div>
      <p v-if="sourceLoading" role="status">Loading source…</p>
      <p v-else-if="sourceError" role="status">
        This source module is unavailable.
      </p>
      <template v-else
        ><p class="source-name">
          <strong>{{ sourceTitle }}</strong
          ><br /><code>{{ sourcePath }}</code>
        </p>
        <p v-if="kind === 'substance'" class="source-name">
          Substance construction: <code>{{ substanceInvocation }}</code>
          <br />Figure construction: <code>{{ invocation }}</code>
        </p>
        <pre
          tabindex="0"
          :aria-label="`${label} ${kind} source`"
        ><code>{{ displayedCode }}</code></pre>
      </template>
      <p v-if="copyStatus" role="status">{{ copyStatus }}</p>
    </details>
  </section>
</template>

<style scoped>
.figure-canvas {
  position: relative;
  width: 100%;
  height: 20rem;
  background: white;
}
.figure-canvas.embedded {
  height: 100%;
}
.static-figure {
  display: block;
  width: 100%;
  height: 100%;
  object-fit: contain;
}
.live-figure {
  position: absolute;
  inset: 0;
  visibility: hidden;
  background: white;
}
.live-figure.visible {
  visibility: visible;
}
.live-figure :deep([data-bloom-drag]:focus-visible) {
  outline: 2px solid #d87937;
  outline-offset: 3px;
}
.figure-inspector {
  border-top: 1px solid var(--vp-c-divider);
  padding: 0.7rem 0;
}
.figure-inspector summary {
  cursor: pointer;
  font-weight: 600;
}
.figure-inspector p {
  font-size: 0.85rem;
}
.layout-controls,
.program-controls {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 0.6rem;
  margin: 0.75rem 0;
}
.layout-controls label,
.program-controls label {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  font-size: 0.85rem;
}
.layout-controls input {
  min-width: 10rem;
  max-width: 20rem;
}
input,
select,
button {
  border: 1px solid var(--vp-c-divider);
  border-radius: 0.3rem;
  padding: 0.3rem 0.55rem;
  background: var(--vp-c-bg);
  font-size: 0.85rem;
}
button {
  cursor: pointer;
}
button:disabled {
  opacity: 0.4;
  cursor: default;
}
input[type="checkbox"] {
  min-width: 0;
  accent-color: #d87937;
}
.figure-message {
  color: var(--vp-c-text-2);
}
.source-name code {
  overflow-wrap: anywhere;
  font-size: 0.75rem;
}
pre {
  max-height: 32rem;
  overflow: auto;
  padding: 1rem;
  background: var(--vp-code-block-bg);
  font-size: 0.8rem;
  line-height: 1.5;
}
pre code {
  white-space: pre;
}
</style>

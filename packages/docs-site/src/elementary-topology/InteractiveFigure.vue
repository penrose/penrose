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
import FigureIcon from "./FigureIcon.vue";
import { sourceTokens } from "./source-tokens";
import {
  readFigureProgram,
  type BookFigure,
  type ProgramKind,
} from "./figure-programs";

const props = defineProps<{
  figure: BookFigure;
  canvasTarget?: string;
  /** Render directly in a book-page figure anchor without cross-parent Teleport. */
  embedded?: boolean;
  overlayClip?: string;
  pageTarget?: string;
  /** The book reader can close the previous source back without rebuilding its diagram. */
  active?: boolean;
}>();
const emit = defineEmits<{
  (event: "flip", flipped: boolean): void;
  (event: "ready", ready: boolean): void;
}>();
const LiveFigure = defineAsyncComponent(async () => {
  const { applyPureReactInVue } = await import("veaury");
  const { default: component } = await import("./LiveFigure");
  return applyPureReactInVue(component);
});
const mounted = ref(false),
  flipped = ref(false);
const card = ref<HTMLElement | null>(null);
const flipControl = ref<HTMLButtonElement | null>(null);
const returnControl = ref<HTMLButtonElement | null>(null);
const backStyle = ref<Record<string, string>>({});
const sideTools = ref(false);
const seed = ref(`gemignani-${props.figure.id}`),
  draftSeed = ref(seed.value);
const generation = ref(0),
  sampleLayout = ref(false),
  ready = ref(false),
  failed = ref(false);
const host = ref<HTMLElement | null>(null);
const kind = ref<ProgramKind>("substance"),
  fullModule = ref(false);
const code = ref(""),
  sourcePath = ref(""),
  sourceLoading = ref(false),
  sourceError = ref(false),
  copyStatus = ref("");
let observer: MutationObserver | undefined;
let sizeObserver: ResizeObserver | undefined;
let sourceRevision = 0;
let gesturePointer: number | undefined;
const kinds: ProgramKind[] = ["substance", "style", "domain"];
const identifier = computed(
  () => `figure-${props.figure.id.replace(/[^\w-]/g, "-")}`,
);
const label = computed(() =>
  /^\d/.test(props.figure.id)
    ? `Figure ${props.figure.id}`
    : props.figure.title ?? props.figure.description,
);
const isEmbedded = computed(() =>
  Boolean(props.embedded || props.canvasTarget),
);
const frontClip = computed(
  () =>
    props.overlayClip ??
    (props.figure.sourceClip
      ? `polygon(${props.figure.sourceClip
          .map(([x, y]) => `${x * 100}% ${y * 100}%`)
          .join(",")})`
      : undefined),
);
const dragTarget = computed(() =>
  props.figure.interaction === "table"
    ? "the table"
    : props.figure.interaction === "objects"
    ? "the marked objects"
    : props.figure.interaction === "construction"
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
const highlightedCode = computed(() => sourceTokens(displayedCode.value));
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

/** A back face can be larger than its original figure while staying on the source page. */
const measureBack = () => {
  const target = props.canvasTarget
    ? document.querySelector<HTMLElement>(props.canvasTarget)
    : card.value;
  if (!target) return;
  const anchor = target.getBoundingClientRect();
  const page = props.pageTarget
    ? document.querySelector<HTMLElement>(props.pageTarget)
    : target.closest<HTMLElement>(".source-page");
  const bounds = (page ?? target).getBoundingClientRect();
  const margin = isEmbedded.value ? Math.min(16, bounds.width / 12) : 12;
  const viewport = window.visualViewport;
  const viewportTop = viewport?.offsetTop ?? 0;
  const viewportHeight = viewport?.height ?? window.innerHeight;
  const width = Math.max(1, Math.min(520, bounds.width - 2 * margin));
  const height = Math.max(
    1,
    Math.min(480, bounds.height - 2 * margin, viewportHeight - 2 * margin),
  );
  const clamp = (value: number, lo: number, hi: number) =>
    Math.min(hi, Math.max(lo, value));
  const x = clamp(
    anchor.left + (anchor.width - width) / 2,
    bounds.left + margin,
    bounds.right - width - margin,
  );
  let y = clamp(
    anchor.top + (anchor.height - height) / 2,
    bounds.top + margin,
    bounds.bottom - height - margin,
  );
  const visibleLo = Math.max(bounds.top + margin, viewportTop + margin);
  const visibleHi = Math.min(
    bounds.bottom - height - margin,
    viewportTop + viewportHeight - height - margin,
  );
  if (visibleLo <= visibleHi) y = clamp(y, visibleLo, visibleHi);
  backStyle.value = {
    "--figure-back-max-width": `${bounds.width - 2 * margin}px`,
    "--figure-back-width": `${width}px`,
    "--figure-back-height": `${height}px`,
    "--figure-back-offset-x": `${x - anchor.left}px`,
    "--figure-back-offset-y": `${y - anchor.top}px`,
  };
};
const measureTools = () => {
  const target = card.value;
  const page = target?.closest<HTMLElement>(".source-page");
  if (!target || !page || !isEmbedded.value || window.innerWidth > 520) {
    sideTools.value = false;
    return;
  }
  const anchor = target.getBoundingClientRect();
  const bounds = page.getBoundingClientRect();
  const x = anchor.right + 8,
    y = anchor.top;
  const obstructed = Array.from(
    page.querySelectorAll<HTMLElement>(".replacement"),
  )
    .filter((other) => !other.contains(target))
    .some((other) => {
      const rect = other.getBoundingClientRect();
      return (
        rect.left < x + 56 &&
        rect.right > x &&
        rect.top < y + 48 &&
        rect.bottom > y
      );
    });
  sideTools.value =
    bounds.right - anchor.right >= 70 && anchor.height >= 52 && !obstructed;
};
const updateBack = () => {
  measureTools();
  if (flipped.value) measureBack();
};
const trackGesture = (event: PointerEvent) => {
  if (
    event.target instanceof Element &&
    event.target.closest(".live-figure svg")
  )
    gesturePointer = event.pointerId;
};
const finishGesture = (event: PointerEvent) => {
  if (event.pointerId === gesturePointer) gesturePointer = undefined;
};
const cancelGesture = () => {
  if (gesturePointer === undefined) return;
  const pointerId = gesturePointer;
  gesturePointer = undefined;
  window.dispatchEvent(new PointerEvent("pointercancel", { pointerId }));
};
const reseed = (sample = true) => {
  cancelGesture();
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
// Stable callback identities prevent React's useDiagram from rebuilding during a flip.
const markFailed = () => {
  failed.value = true;
  ready.value = false;
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
const setFlipped = async (value: boolean, restoreFocus = true) => {
  if (flipped.value === value) return;
  // Move focus outside the face about to become inert, then to the visible control.
  if (value) cancelGesture();
  if (restoreFocus) card.value?.focus({ preventScroll: true });
  if (value) measureBack();
  flipped.value = value;
  emit("flip", value);
  if (value && !code.value && !sourceLoading.value) loadSource();
  await nextTick();
  if (restoreFocus)
    (value ? returnControl.value : flipControl.value)?.focus({
      preventScroll: true,
    });
};
const codeKey = (event: KeyboardEvent) => {
  event.stopPropagation();
  if (event.key === "Escape") {
    event.preventDefault();
    setFlipped(false);
  }
};
const programKey = (event: KeyboardEvent, index: number) => {
  const next =
    event.key === "ArrowRight"
      ? (index + 1) % kinds.length
      : event.key === "ArrowLeft"
      ? (index + kinds.length - 1) % kinds.length
      : event.key === "Home"
      ? 0
      : event.key === "End"
      ? kinds.length - 1
      : undefined;
  if (next === undefined) return;
  event.preventDefault();
  event.stopPropagation();
  kind.value = kinds[next];
  card.value
    ?.querySelector<HTMLButtonElement>(`[data-program="${kind.value}"]`)
    ?.focus();
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
watch(ready, (value) => emit("ready", value));
watch(
  () => props.active,
  (value) => {
    if (value === false && flipped.value) setFlipped(false, false);
  },
);
onMounted(async () => {
  mounted.value = true;
  emit("ready", false);
  await nextTick();
  observe();
  measureTools();
  const target = props.canvasTarget
    ? document.querySelector<HTMLElement>(props.canvasTarget)
    : card.value;
  const page = target?.closest<HTMLElement>(".source-page");
  sizeObserver = new ResizeObserver(updateBack);
  if (target) sizeObserver.observe(target);
  if (page) sizeObserver.observe(page);
  window.addEventListener("resize", updateBack);
  window.addEventListener("scroll", updateBack, { passive: true });
  window.visualViewport?.addEventListener("resize", updateBack);
  window.visualViewport?.addEventListener("scroll", updateBack);
  window.addEventListener("pointerup", finishGesture);
  window.addEventListener("pointercancel", finishGesture);
});
onBeforeUnmount(() => {
  cancelGesture();
  sourceRevision++;
  observer?.disconnect();
  sizeObserver?.disconnect();
  window.removeEventListener("resize", updateBack);
  window.removeEventListener("scroll", updateBack);
  window.visualViewport?.removeEventListener("resize", updateBack);
  window.visualViewport?.removeEventListener("scroll", updateBack);
  window.removeEventListener("pointerup", finishGesture);
  window.removeEventListener("pointercancel", finishGesture);
});
</script>

<template>
  <Teleport
    :to="mounted ? canvasTarget || 'body' : 'body'"
    :disabled="!mounted || !canvasTarget"
  >
    <section
      v-if="!canvasTarget || mounted"
      ref="card"
      class="figure-card"
      :class="{ embedded: isEmbedded, 'is-flipped': flipped }"
      :style="backStyle"
      tabindex="-1"
      :aria-label="`${label}, interactive diagram and source`"
      @keydown.esc.stop.prevent="setFlipped(false)"
      @pointerdown.capture="trackGesture"
    >
      <div
        class="figure-toolbar"
        :class="{ 'side-tools': sideTools }"
        :inert="flipped"
        :aria-hidden="flipped"
      >
        <button
          ref="flipControl"
          type="button"
          class="flip-control"
          :aria-label="`Flip ${label} to see its program`"
          :aria-expanded="flipped"
          :aria-controls="`${identifier}-source`"
          :title="
            failed
              ? 'Reviewed static figure; flip to read its program'
              : 'Flip to source program'
          "
          :aria-busy="mounted && !ready && !failed"
          @click="setFlipped(true)"
        >
          <span
            v-if="mounted && !ready && !failed"
            class="control-spinner"
            aria-hidden="true"
          /><FigureIcon v-else name="flip" /><span>Flip</span>
        </button>
        <div class="secondary-tools">
          <button
            type="button"
            :aria-label="`Re-sample ${label}`"
            title="Re-sample layout"
            @click="resample"
          >
            <FigureIcon name="shuffle" /></button
          ><button
            type="button"
            :aria-label="`Reset ${label} layout`"
            title="Reset source layout"
            @click="reset"
          >
            <FigureIcon name="reset" />
          </button>
        </div>
      </div>
      <div class="figure-turntable">
        <div
          class="figure-face figure-front"
          :inert="flipped"
          :aria-hidden="flipped"
        >
          <div
            class="figure-canvas"
            :style="{ clipPath: frontClip }"
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
        </div>
        <div
          class="figure-face figure-back"
          :inert="!flipped"
          :aria-hidden="!flipped"
        >
          <div :id="`${identifier}-source`" class="source-panel">
            <header class="source-header">
              <div>
                <span class="source-eyebrow">Behind the figure</span>
                <h3>{{ label }}</h3>
              </div>
              <button
                ref="returnControl"
                type="button"
                class="return-control"
                :aria-label="`Return to ${label}`"
                title="Return to diagram (Escape)"
                @click="setFlipped(false)"
              >
                <FigureIcon name="back" /><span>Figure</span>
              </button>
            </header>
            <p v-if="failed" class="source-message" role="status">
              The interactive layout is unavailable. The reviewed static figure
              remains visible on the front; its source is available here.
            </p>
            <div class="program-bar">
              <div
                class="program-tabs"
                role="tablist"
                :aria-label="`${label} source program`"
              >
                <button
                  v-for="(program, index) in kinds"
                  :id="`${identifier}-tab-${program}`"
                  :key="program"
                  type="button"
                  role="tab"
                  :data-program="program"
                  :aria-selected="kind === program"
                  :aria-controls="`${identifier}-program`"
                  :tabindex="kind === program ? 0 : -1"
                  :class="{ selected: kind === program }"
                  @click="kind = program"
                  @keydown="programKey($event, index)"
                >
                  {{ program }}
                </button>
              </div>
              <div class="source-actions">
                <button
                  type="button"
                  :disabled="sourceLoading || sourceError || !code"
                  :aria-label="`Copy ${kind} source`"
                  title="Copy shown source"
                  @click="copy"
                >
                  <FigureIcon name="copy" /></button
                ><button
                  type="button"
                  :disabled="sourceLoading || sourceError || !code"
                  aria-label="Download full source module"
                  title="Download module"
                  @click="download"
                >
                  <FigureIcon name="download" />
                </button>
              </div>
            </div>
            <div class="source-caption">
              <code>{{
                kind === "substance" ? substanceInvocation : sourceTitle
              }}</code
              ><label v-if="kind === 'substance' && figure.substanceLines"
                ><input v-model="fullModule" type="checkbox" />Full
                module</label
              >
            </div>
            <div
              :id="`${identifier}-program`"
              class="program-body"
              role="tabpanel"
              :aria-labelledby="`${identifier}-tab-${kind}`"
            >
              <p v-if="sourceLoading" class="source-message" role="status">
                Loading source…
              </p>
              <p v-else-if="sourceError" class="source-message" role="status">
                This source module is unavailable.
              </p>
              <pre
                v-else
                class="source-code"
                tabindex="0"
                :aria-label="`${label} ${kind} source`"
                @keydown="codeKey"
              ><code><span v-for="(token, index) in highlightedCode" :key="index" :class="token.kind ? `token-${token.kind}` : undefined">{{ token.text }}</span></code></pre>
            </div>
            <footer class="source-footer">
              <details class="source-details">
                <summary>Module &amp; construction</summary>
                <code>{{ sourcePath }}</code
                ><span>Figure construction</span><code>{{ invocation }}</code>
              </details>
              <details
                v-if="figure.sourceCorrection"
                class="source-notation-note"
              >
                <summary>Source notation note</summary>
                <p>{{ figure.sourceCorrection.source }}</p>
                <p>{{ figure.sourceCorrection.mathematicalProgram }}</p>
                <p>{{ figure.sourceCorrection.visibleFigure }}</p>
              </details>
              <details class="layout-details">
                <summary>Layout seed</summary>
                <p>
                  Drag {{ dragTarget }} or use a focused handle's arrow keys.
                  Mathematical facts stay fixed.
                </p>
                <form class="layout-controls" @submit.prevent="reseed()">
                  <input
                    v-model="draftSeed"
                    :aria-label="`${label} seed`"
                    spellcheck="false"
                  /><button type="submit">Apply</button
                  ><button
                    type="button"
                    aria-label="Re-sample layout"
                    title="Re-sample layout"
                    @click="resample"
                  >
                    <FigureIcon name="shuffle" /></button
                  ><button
                    type="button"
                    aria-label="Reset source layout"
                    title="Reset source layout"
                    @click="reset"
                  >
                    <FigureIcon name="reset" />
                  </button>
                </form>
                <p class="seed-value">Current: {{ seed }}</p>
              </details>
              <span v-if="copyStatus" class="copy-status" role="status">{{
                copyStatus
              }}</span>
            </footer>
          </div>
        </div>
      </div>
      <span class="figure-status" role="status" aria-live="polite">{{
        failed
          ? "Interactive layout unavailable; reviewed static figure shown."
          : mounted && !ready
          ? "Preparing interactive layout…"
          : ""
      }}</span>
    </section>
  </Teleport>
</template>

<style scoped>
.figure-card {
  position: relative;
  width: 100%;
  height: 22rem;
  margin: 2.8rem 0 1.5rem;
  perspective: 1400px;
  isolation: isolate;
  border: 1px solid #e8e1d9;
  border-radius: 12px;
  background: white;
  box-shadow: 0 2px 12px #36241408;
}
.figure-card.embedded {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  margin: 0;
  border: 0;
  border-radius: 0;
  background: transparent;
  box-shadow: none;
}
.figure-card.is-flipped {
  z-index: 30;
}
.figure-turntable {
  position: relative;
  width: 100%;
  height: 100%;
  transform-style: preserve-3d;
  transition: transform 420ms cubic-bezier(0.2, 0.7, 0.2, 1);
}
.is-flipped .figure-turntable {
  transform: rotateY(180deg);
}
.figure-face {
  position: absolute;
  inset: 0;
  backface-visibility: hidden;
  -webkit-backface-visibility: hidden;
}
.figure-front {
  transform: rotateY(0deg);
}
.figure-back {
  transform: rotateY(180deg);
  pointer-events: none;
}
.is-flipped .figure-front {
  pointer-events: none;
}
.is-flipped .figure-back {
  pointer-events: auto;
}
.figure-canvas {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  background: white;
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
.figure-toolbar {
  position: absolute;
  right: 0;
  top: -32px;
  z-index: 2;
  display: flex;
  align-items: center;
  gap: 3px;
  height: 28px;
  transition: opacity 160ms;
}
.is-flipped .figure-toolbar {
  visibility: hidden;
  opacity: 0;
  pointer-events: none;
}
button {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 5px;
  height: 28px;
  padding: 4px 6px;
  border: 1px solid transparent;
  border-radius: 6px;
  background: #fff;
  color: #665649;
  cursor: pointer;
  font: inherit;
  font-size: 12px;
  line-height: 1;
  transition:
    background 140ms,
    color 140ms,
    border-color 140ms;
}
button svg {
  width: 16px;
  height: 16px;
  flex: 0 0 auto;
}
button:hover {
  background: #f8f1e9;
  color: #a25024;
  border-color: #ead8c6;
}
button:focus-visible,
input:focus-visible,
summary:focus-visible,
.source-code:focus-visible {
  outline: 2px solid #bc7040;
  outline-offset: 2px;
}
button:disabled {
  opacity: 0.4;
  cursor: default;
}
.flip-control {
  order: 2;
  font-size: 11px;
  font-weight: 550;
  background: #fffefce8;
  color: #8b7867;
  border-color: #ede6dd;
}
.secondary-tools {
  order: 1;
  display: flex;
  gap: 2px;
  visibility: hidden;
  opacity: 0;
  transition: opacity 140ms;
}
.figure-toolbar.side-tools {
  top: 0;
  right: auto;
  left: calc(100% + 8px);
  width: 56px;
  height: auto;
  flex-direction: column;
}
.side-tools .flip-control {
  order: 0;
}
.side-tools .secondary-tools {
  order: 1;
}
.control-spinner {
  width: 12px;
  height: 12px;
  box-sizing: border-box;
  border: 1.5px solid #ead8c6;
  border-top-color: #a25024;
  border-radius: 50%;
  animation: figure-loading 900ms linear infinite;
}
@keyframes figure-loading {
  to {
    transform: rotate(360deg);
  }
}
.figure-card:hover .secondary-tools,
.figure-card:focus-within .secondary-tools {
  visibility: visible;
  opacity: 1;
}
.figure-status {
  position: absolute;
  width: 1px;
  height: 1px;
  padding: 0;
  margin: -1px;
  overflow: hidden;
  clip-path: inset(50%);
  white-space: nowrap;
}
.source-panel {
  position: absolute;
  left: var(--figure-back-offset-x, 12px);
  top: var(--figure-back-offset-y, 12px);
  width: var(--figure-back-width, calc(100% - 24px));
  max-width: var(--figure-back-max-width, calc(100vw - 32px));
  height: var(--figure-back-height, calc(100% - 24px));
  display: flex;
  flex-direction: column;
  overflow: auto;
  box-sizing: border-box;
  border: 1px solid #ddc7b2;
  border-radius: 12px;
  background: #fffdf9;
  color: #352a22;
  box-shadow:
    0 14px 42px #382a2526,
    0 2px 6px #382a2514;
  font-size: 12px;
  line-height: 1.45;
}
.source-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  padding: 12px 14px 10px;
  border-bottom: 1px solid #e9dfd3;
  flex: 0 0 auto;
}
.source-eyebrow {
  font-size: 9px;
  letter-spacing: 0.09em;
  text-transform: uppercase;
  color: #a06a44;
}
.source-header h3 {
  margin: 2px 0 0;
  font-size: 14px;
  font-weight: 600;
  line-height: 1.25;
  letter-spacing: -0.01em;
}
.return-control {
  border-color: #e6d8c9;
  flex: 0 0 auto;
}
.program-bar {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  justify-content: space-between;
  gap: 5px;
  padding: 7px 11px;
  flex: 0 0 auto;
}
.program-tabs {
  display: flex;
  padding: 2px;
  border-radius: 7px;
  background: #f0e9df;
}
.program-tabs button {
  text-transform: capitalize;
  height: 26px;
  padding: 4px 9px;
  background: transparent;
  color: #7a6a5a;
}
.program-tabs button.selected {
  background: white;
  color: #7c431e;
  border-color: #e1d4c5;
  box-shadow: 0 1px 2px #382a2512;
}
.source-actions {
  display: flex;
  gap: 2px;
}
.source-caption {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  justify-content: space-between;
  gap: 5px;
  padding: 0 14px 8px;
  color: #7a6553;
  font-size: 10px;
  flex: 0 0 auto;
}
.source-caption code {
  max-width: 100%;
  overflow-wrap: anywhere;
  background: transparent;
  padding: 0;
  color: inherit;
  font-size: 10px;
}
.source-caption label {
  display: inline-flex;
  align-items: center;
  gap: 4px;
  white-space: nowrap;
}
input[type="checkbox"] {
  accent-color: #b66d36;
}
.program-body {
  display: flex;
  flex: 1 1 auto;
  min-height: 100px;
  margin: 0 10px;
  border: 1px solid #e8ddd0;
  border-radius: 6px;
  background: #faf7f1;
  overflow: hidden;
}
.source-code {
  flex: 1 1 auto;
  width: 100%;
  height: 100%;
  margin: 0;
  padding: 12px;
  box-sizing: border-box;
  overflow: auto;
  font: 12px/1.65 var(--vp-font-family-mono, monospace);
  background: transparent;
}
.source-code code {
  white-space: pre;
  padding: 0;
  font: inherit;
  color: #45352a;
  background: none;
}
.token-comment {
  color: #767266;
  font-style: italic;
}
.token-string {
  color: #9c4926;
}
.token-keyword {
  color: #2e6382;
}
.token-number {
  color: #75548b;
}
.source-message {
  padding: 10px;
  font-size: 12px;
}
.source-footer {
  display: flex;
  flex-direction: column;
  gap: 5px;
  padding: 9px 14px 11px;
  flex: 0 0 auto;
  font-size: 10px;
  color: #806f5e;
}
summary {
  cursor: pointer;
}
.source-details code {
  display: block;
  overflow-wrap: anywhere;
  background: transparent;
  padding: 2px 0;
  font-size: 10px;
  color: #73533b;
}
.source-details span {
  display: block;
  margin-top: 4px;
}
.source-notation-note p,
.layout-details p {
  margin: 5px 0;
  font-size: 10px;
  line-height: 1.5;
}
.layout-controls {
  display: flex;
  align-items: center;
  flex-wrap: wrap;
  gap: 4px;
  margin-top: 6px;
}
.layout-controls input {
  flex: 1 1 12rem;
  width: 0;
  min-width: 7rem;
  padding: 4px 6px;
  border: 1px solid #ddcebd;
  border-radius: 5px;
  background: white;
  color: #604b3b;
  font: 11px var(--vp-font-family-mono, monospace);
}
.layout-controls button {
  height: 26px;
  border-color: #e6d8c9;
}
.seed-value {
  overflow-wrap: anywhere;
}
.copy-status {
  color: #7c431e;
}
@media (prefers-reduced-motion: reduce) {
  .control-spinner {
    animation: none;
  }
  .figure-turntable,
  button,
  .figure-toolbar,
  .secondary-tools {
    transition: none;
  }
}
@media (max-width: 520px) {
  .figure-toolbar {
    top: -26px;
    height: 22px;
  }
  .figure-toolbar button {
    height: 22px;
    font-size: 10px;
    padding: 3px 5px;
  }
  .figure-toolbar button svg {
    width: 13px;
    height: 13px;
  }
  .figure-card:not(.embedded) {
    height: 23rem;
  }
  .source-header {
    padding: 10px 11px 8px;
  }
  .program-tabs button {
    padding-inline: 6px;
    font-size: 11px;
  }
  .source-caption {
    padding-inline: 11px;
  }
  .source-code {
    padding: 9px;
  }
}
</style>

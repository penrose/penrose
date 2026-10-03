<script setup lang="ts">
import { computed, onBeforeUnmount, ref, watch } from "vue";
import InteractiveFigure from "./InteractiveFigure.vue";
import type { BookFigure } from "./figure-programs";
const props = defineProps<{ figure: BookFigure }>();
const loop = computed(() => props.figure.id.startsWith("original-retracing-"));
const radial = computed(
  () => props.figure.id === "exploration-radial-contraction",
);
const count = computed(() =>
  loop.value || radial.value
    ? 12
    : Number(props.figure.buildArguments?.[3] ?? 8),
);
const step = ref(0);
const ready = ref(false);
const playing = ref(false);
let timer: ReturnType<typeof setTimeout> | undefined;
const stop = () => {
  playing.value = false;
  clearTimeout(timer);
};
const frame = computed<BookFigure>(() => ({
  ...props.figure,
  svg: undefined,
  buildFactory: radial.value
    ? "buildRadialContractionFrameFigure"
    : loop.value
    ? "buildInverseCancellationFrameFigure"
    : "buildContractionIterationFrameFigure",
  buildArguments: radial.value
    ? [step.value / count.value, [0.53, 0.53]]
    : loop.value
    ? [props.figure.buildArguments?.[0] ?? "circle", step.value / count.value]
    : [...(props.figure.buildArguments ?? []), step.value],
  substanceArguments: radial.value
    ? [1 - step.value / count.value, [0.53, 0.53]]
    : loop.value
    ? [props.figure.buildArguments?.[0] ?? "circle", step.value / count.value]
    : props.figure.substanceArguments,
}));
const progress = computed(() =>
  radial.value
    ? `r = ${(1 - step.value / count.value).toFixed(2)}`
    : loop.value
    ? `s = ${(step.value / count.value).toFixed(2)}`
    : `n = ${step.value}`,
);
const caption = computed(() => {
  if (radial.value)
    return step.value === 0
      ? "The identity leaves the disk and its marked point in place."
      : step.value === count.value
      ? "The entire disk has become one point: the origin."
      : "Every point moves toward the origin by the same scale factor.";
  if (loop.value)
    return step.value === 0
      ? "Follow the loop, then retrace it back to the basepoint."
      : step.value === count.value
      ? "Only the basepoint remains. The loop and its inverse cancel."
      : "The outgoing and return paths shrink together. The basepoint stays fixed.";
  return step.value === 0
    ? "Start at the chosen point. Each step applies the same contraction."
    : step.value === count.value
    ? "The iterates approach the fixed point; their distance shrinks geometrically."
    : "Move up to the graph of f, then across to the diagonal. Repeat from the new value.";
});
const schedule = () => {
  clearTimeout(timer);
  if (!playing.value || !ready.value) return;
  if (step.value >= count.value) {
    stop();
    return;
  }
  timer = setTimeout(() => {
    ready.value = false;
    step.value++;
  }, 850);
};
const seek = (value: number) => {
  stop();
  const next = Math.max(0, Math.min(count.value, value));
  if (next === step.value) return;
  ready.value = false;
  step.value = next;
};
const togglePlay = () => {
  if (playing.value) {
    stop();
    return;
  }
  if (step.value === count.value) {
    ready.value = false;
    step.value = 0;
  }
  playing.value = true;
  schedule();
};
const markReady = (value: boolean) => {
  ready.value = value;
  schedule();
};
watch(step, () => {
  clearTimeout(timer);
  ready.value = false;
});
watch(
  () => props.figure.id,
  () => {
    stop();
    step.value = 0;
    ready.value = false;
  },
);
onBeforeUnmount(stop);
</script>

<template>
  <div
    class="steppable-figure"
    :class="{
      'loop-animation': loop,
      'compact-animation': loop || radial,
    }"
    @pointerdown.capture="
      ($event.target as Element).closest('.live-figure, .figure-toolbar') &&
        stop()
    "
  >
    <InteractiveFigure
      :key="`${figure.id}-${step}`"
      :figure="frame"
      @ready="markReady"
      @flip="stop"
    />
    <div class="step-controls" aria-label="Animation controls">
      <button
        class="play-control"
        :aria-label="playing ? 'Pause animation' : 'Play animation'"
        :disabled="!ready && !playing"
        @click="togglePlay"
      >
        <svg v-if="playing" viewBox="0 0 20 20" aria-hidden="true">
          <path d="M7 4v12M13 4v12" />
        </svg>
        <svg v-else viewBox="0 0 20 20" aria-hidden="true">
          <path d="m7 4 9 6-9 6Z" />
        </svg>
      </button>
      <button
        aria-label="Previous animation step"
        :disabled="step === 0"
        @click="seek(step - 1)"
      >
        <svg viewBox="0 0 20 20" aria-hidden="true">
          <path d="m12 5-5 5 5 5" />
        </svg>
      </button>
      <label class="step-track"
        ><span class="sr-only">{{
          loop || radial ? "Homotopy time" : "Iteration"
        }}</span
        ><input
          type="range"
          :value="step"
          min="0"
          :max="count"
          step="1"
          :aria-valuetext="progress"
          @input="seek(Number(($event.target as HTMLInputElement).value))"
      /></label>
      <button
        aria-label="Next animation step"
        :disabled="step === count"
        @click="seek(step + 1)"
      >
        <svg viewBox="0 0 20 20" aria-hidden="true">
          <path d="m8 5 5 5-5 5" />
        </svg>
      </button>
      <output class="step-value">{{ progress }}</output>
    </div>
    <p class="step-caption" aria-live="polite">{{ caption }}</p>
  </div>
</template>

<style scoped>
.steppable-figure {
  margin: 1.6rem 0;
}
.compact-animation :deep(.figure-card) {
  max-width: 540px;
  margin-left: auto;
  margin-right: auto;
}
.step-controls {
  display: flex;
  align-items: center;
  gap: 0.45rem;
  max-width: 530px;
  margin: 0.6rem auto 0;
  padding: 0.5rem 0.2rem;
}
.step-controls button {
  display: grid;
  place-items: center;
  width: 32px;
  height: 32px;
  flex: 0 0 auto;
  border: 1px solid #dedcd2;
  border-radius: 50%;
  background: transparent;
  color: #504c43;
  cursor: pointer;
}
.step-controls button:disabled {
  opacity: 0.3;
  cursor: default;
}
.step-controls button:hover:not(:disabled) {
  border-color: #ad592d;
  color: #ad592d;
}
.step-controls button:focus-visible,
.step-track input:focus-visible {
  outline: 2px solid #ad592d;
  outline-offset: 3px;
}
.step-controls svg {
  width: 16px;
  height: 16px;
  fill: none;
  stroke: currentColor;
  stroke-width: 1.5;
}
.play-control svg {
  fill: currentColor;
}
.step-track {
  flex: 1 1 auto;
  min-width: 0;
  display: flex;
  padding: 0 0.25rem;
}
.step-track input {
  width: 100%;
  cursor: pointer;
  accent-color: #ad592d;
}
.step-value {
  flex: 0 0 4.5rem;
  font:
    0.77rem/1.4 ui-monospace,
    monospace;
  text-align: right;
  color: #504c43;
}
.step-caption {
  text-align: center;
  color: #77766c;
  font:
    0.9rem/1.6 Georgia,
    serif;
  margin: 0.4rem auto 0;
  max-width: 38rem;
  min-height: 2.9rem;
}
.sr-only {
  position: absolute;
  width: 1px;
  height: 1px;
  overflow: hidden;
  clip: rect(0, 0, 0, 0);
  white-space: nowrap;
}
@media (prefers-reduced-motion: reduce) {
  .step-controls {
    scroll-behavior: auto;
  }
}
</style>

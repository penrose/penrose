<script setup lang="ts">
import { computed, ref } from "vue";
import additions from "../../../../docs/elementary-topology/original-illustrations.json";
import InteractiveFigure from "./InteractiveFigure.vue";
import SteppableFigure from "./SteppableFigure.vue";
import type { BookFigure } from "./figure-programs";
const props = defineProps<{ pdfPage: number }>();
const selected = ref(0);
const illustrations = computed(() => [
  ...additions.illustrations.filter((item) =>
    item.sourcePassage.pdfPages.includes(props.pdfPage),
  ),
  ...(props.pdfPage === 217
    ? [
        {
          id: "exploration-radial-contraction",
          title: "Contracting the unit disk",
          pdfPage: 217,
          status: "exploration",
          description:
            "A continuous radial homotopy contracts the closed unit disk and its marked point to the origin.",
          implementation: "packages/bloom/src/examples/homotopies.ts",
          buildFactory: "buildRadialContractionFrameFigure",
          buildArguments: [0, [0.53, 0.53]],
          substanceFactory: "radialDiskContractionSliceSubstance",
          substanceArguments: [1, [0.53, 0.53]],
          style: "packages/bloom/src/styles/homotopies.tsx",
          domainModule: "packages/bloom/src/domains/point-set-topology.ts",
          interaction: "labels",
          sourcePassage: { section: "11.1 Homotopy", pdfPages: [217] },
        },
      ]
    : []),
]);
const figure = computed(
  () => illustrations.value[selected.value] as BookFigure | undefined,
);
const animated = computed(
  () =>
    figure.value &&
    /original-(retracing|.*contraction)|exploration-radial-contraction/.test(
      figure.value.id,
    ),
);
const explanations: Record<
  string,
  { title: string; text: string; choices?: string[] }
> = {
  "1.4 Groups": {
    title: "Seeing the kernel",
    text: "A homomorphism sends every orange element to the identity. These elements form the kernel: a subgroup whose geometry becomes visible as a single fiber.",
    choices: ["Z₆ → Z₃", "Z₈ → Z₄"],
  },
  "4.2 Derived Sets in Subspaces": {
    title: "The same set, a different surrounding space",
    text: "The x-axis has no interior in the plane. Viewed as a space in its own right, every point is interior. Compare the two neighborhoods to see why.",
  },
  "4.6 Product Spaces": {
    title: "A line inside a product space",
    text: "Fix one coordinate and let the other vary. The resulting slice is a copy of the real line; projection provides its inverse.",
  },
  "8.1 Compactness in Euclidean Space": {
    title: "One radius for the whole cover",
    text: "Compactness lets us choose finitely many smaller balls. Their smallest radius gives a positive Lebesgue number that works everywhere in the interval.",
    choices: ["The unit interval", "A larger interval"],
  },
  "10.3 Complete Metric Spaces": {
    title: "Watch a contraction find its fixed point",
    text: "Apply the same function again and again. The iterates draw a cobweb, while their distance from the fixed point shrinks by the same factor at every step.",
    choices: ["Monotone approach", "Alternating approach"],
  },
  "11.1 Homotopy": {
    title: "Contract a disk to a single point",
    text: "The map jᵣ sends each point p to r·p. Step from the identity at r = 1 to the constant map at r = 0; the orange image and its marked point shrink together.",
  },
  "11.3 The Fundamental Group": {
    title: "A loop and its inverse cancel",
    text: "Follow a loop and return along the same path. Advance the homotopy to shorten both journeys continuously, without moving the basepoint.",
    choices: ["Circle loop", "Figure-eight loop"],
  },
};
const passage = computed(() => illustrations.value[0]?.sourcePassage);
const explanation = computed(() =>
  passage.value ? explanations[passage.value.section] : undefined,
);
</script>

<template>
  <aside
    v-if="figure && explanation"
    class="reading-addition"
    aria-label="Added illustration for this passage"
  >
    <div class="addition-heading">
      <span class="addition-kicker">An added illustration</span
      ><span>{{ passage?.section }}</span>
    </div>
    <h2>{{ explanation.title }}</h2>
    <p class="addition-introduction">{{ explanation.text }}</p>
    <div
      v-if="illustrations.length > 1"
      class="example-choices"
      role="group"
      aria-label="Compare examples"
    >
      <button
        v-for="(item, index) in illustrations"
        :key="item.id"
        :aria-pressed="selected === index"
        @click="selected = index"
      >
        {{ explanation.choices?.[index] ?? item.title }}
      </button>
    </div>
    <SteppableFigure v-if="animated" :key="figure.id" :figure="figure" />
    <InteractiveFigure v-else :key="figure.id" :figure="figure" />
  </aside>
</template>

<style scoped>
.reading-addition {
  border-top: 1px solid #d7cebb;
  margin: 3rem 0 0;
  padding: 2rem 2rem 1rem;
  background: #f7f2e8;
}
.addition-heading {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 1rem;
  color: #888173;
  font: 0.66rem/1.6 sans-serif;
}
.addition-kicker {
  color: #ad592d;
  letter-spacing: 0.17em;
  text-transform: uppercase;
  font-weight: 600;
}
.reading-addition h2 {
  border: 0;
  margin: 1.1rem 0 0.65rem;
  font:
    400 clamp(1.45rem, 3vw, 1.9rem) / 1.3 Georgia,
    serif;
  color: #302f29;
}
.addition-introduction {
  font:
    1rem/1.7 Georgia,
    serif;
  max-width: 42rem;
  color: #645e52;
  margin: 0 0 1.4rem;
}
.example-choices {
  display: flex;
  flex-wrap: wrap;
  gap: 0.3rem;
  margin: 0.8rem 0 1.2rem;
}
.example-choices button {
  cursor: pointer;
  border: 1px solid transparent;
  border-radius: 3px;
  padding: 0.45rem 0.8rem;
  color: #70695c;
  background: transparent;
  font:
    0.83rem/1.5 Georgia,
    serif;
}
.example-choices button[aria-pressed="true"] {
  border-color: #d5c8b2;
  color: #8c4827;
  background: #fffdf7;
}
.example-choices button:focus-visible {
  outline: 2px solid #ad592d;
  outline-offset: 3px;
}
@media (max-width: 600px) {
  .reading-addition {
    padding: 1.4rem 0.9rem 0.6rem;
    margin-top: 2rem;
  }
  .addition-heading {
    align-items: flex-start;
    flex-direction: column;
    gap: 0.2rem;
  }
}
</style>

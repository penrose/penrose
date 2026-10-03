<script setup lang="ts">
import manifest from "../../../../docs/elementary-topology/figures.json";

const figures = manifest.figures.filter((figure) =>
  ["numbered-figure", "unnumbered-mathematical-diagram"].includes(figure.kind),
);
const reviewed = figures.filter(
  (figure) => figure.status === "reviewed",
).length;
const numbered = figures.filter((figure) => figure.kind === "numbered-figure");
const panelCount = numbered.reduce(
  (total, figure) => total + figure.panels.length,
  0,
);
</script>

<template>
  <p>
    The supplied scan contains 250 pages. Its visual inventory records
    {{ numbered.length }} numbered figures with {{ panelCount }} panels, one
    unnumbered mathematical diagram, three tables, and the cover illustration.
    {{ reviewed }} of the {{ figures.length }} available mathematical figures
    have been reproduced and reviewed. Both mathematical tables and the cover
    artwork also have native interactive reproductions; the symbol index remains
    typeset text. These counts cover the supplied pages. The scan omits 31
    printed pagination positions, including the locations of nine unlocated
    figure labels.
  </p>
</template>

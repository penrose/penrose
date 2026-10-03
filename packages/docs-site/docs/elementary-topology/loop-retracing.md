---
title: Retracing a loop — Elementary Topology
description: Two new Penrose illustrations of the inverse law for based loops.
---

<script setup>
import SteppableFigure from '../../src/elementary-topology/SteppableFigure.vue';
import additions from '../../../../docs/elementary-topology/original-illustrations.json';
</script>

# Retracing a loop

These are new illustrations of
[Section 11.3, Proposition 4, printed page 246](./chapter-11/fundamental-group#printed-page-246).
They are separate from the book's numbered figures.

Following a loop $a$ and returning along its inverse $a^{-1}(r)=a(1-r)$
produces a loop homotopic to the constant loop $k$, while keeping the
basepoint $y_0$ fixed. The book gives the homotopy

$$H(r,s)=a\bigl(2\min(r,1-r)(1-s)\bigr).$$

At $s=0$, this traverses $a$ and then its inverse. As $s$ increases, it
traverses a shorter initial part of $a$ and returns along that same part.
At $s=1$, every point maps to $y_0$. Both $H(0,s)$ and $H(1,s)$ equal $y_0$
for every $s$, so this is a homotopy relative to the interval endpoints.

The gray curve is the original loop as a reference. Orange shows the image
of the current loop, and the two arrows show outgoing and return traversal.
Use the step controls to advance from $s=0$ to $s=1$, or play the construction.

<SteppableFigure :figure="additions.illustrations.find(f => f.id === 'original-retracing-circle')" />

The same style and mathematical rescaling work for a self-intersecting loop:

<SteppableFigure :figure="additions.illustrations.find(f => f.id === 'original-retracing-figure-eight')" />

Both Substances use the reusable loop, inverse, concatenation, relative
homotopy and fundamental-group vocabulary. Their finite Fourier coefficients
define mathematical plane loops; the native Path samples evaluate those
functions directly. Flip a diagram to inspect the actual
Substance factory, Style and Domain, or drag and re-sample the labels.

[Return to further illustrations](./further-illustrations)

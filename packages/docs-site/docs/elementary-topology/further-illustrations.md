---
title: Further illustrations — Elementary Topology
description: New Penrose illustrations for unillustrated passages, with reusable mathematical programs.
---

<script setup>
import InteractiveFigure from '../../src/elementary-topology/InteractiveFigure.vue';
import additions from '../../../../docs/elementary-topology/original-illustrations.json';
</script>

# Further illustrations

These are new illustrations of passages that have no corresponding figure in
the supplied book. They are separate from the numbered figure reproductions.
Each uses the same typed topology vocabulary as the book figures. Open the
program controls to inspect the Substance, Style and Domain modules, drag labels
to adjust their positions, or re-sample the layout.

## Ambient and relative derived sets

[Section 4.2, Example 6, printed page 68](./chapter-04/derived-sets-in-subspaces#printed-page-68)
compares the $x$-axis $A=Y$ in the Euclidean plane $X=R^2$ with the same set
considered in its own subspace topology.

In the plane, every disk $N$ around $p\in A$ contains an off-axis point such as
$q$. Thus $A$ has empty interior and is its own frontier. In $Y$, the
neighborhood is $N\cap Y$, and $A=Y$ is both open and closed: its interior is
all of $A$, and its frontier is empty. Its closure is $A$ in both spaces.

<InteractiveFigure :figure="additions.illustrations[0]" />

The Substance program asserts both topologies and their interior, frontier and
closure relationships. Its reusable style accepts other horizontal affine
lines and neighborhood radii as well.

## A coordinate-slice embedding

[Section 4.6, Proposition 20, printed pages 87–88](./chapter-04/product-spaces#printed-page-87)
embeds each factor of a product by fixing the other coordinates.
This example chooses the vertical slice
$Y=\{0.75\}\times R\subset R^2$ and the inclusion
$q_2(x)=(0.75,x)$. Its corestriction $R\to Y$ is a homeomorphism, with inverse
given by projection onto the second coordinate.

<InteractiveFigure :figure="additions.illustrations[1]" />

The orange line is the image $Y$; the dashed coordinate axes make its nonzero
offset visible. This uses the same Substance factory and coordinate-embedding
style as Figure 4.10, with the other coordinate allowed to vary and the
ambient axes exposed. Changing the fixed coordinate or the varying coordinate
reuses both modules.

[Return to the book index](./index)

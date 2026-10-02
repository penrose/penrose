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
Each uses the same typed mathematical vocabulary as the book figures. Open the
program controls to inspect the Substance, Style and Domain modules, drag the
marked objects or labels, or re-sample the layout.

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

## A positive Lebesgue number

[Section 8.1, Proposition 3, printed page 165](./chapter-08/compactness-in-euclidean-space#printed-page-165)
finds a uniform positive radius for an open cover of a compact metric space.
Here $X=[0,1]$. The smaller balls $N(x_j,\rho_{x_j}/2)$ form a finite subcover,
and $\rho=\min_j\rho_{x_j}/2>0$. For any $x\in X$, choose a smaller ball
containing $x$. The triangle inequality puts every $z\in N(x,\rho)$ inside
the corresponding original cover member $N(x_j,\rho_{x_j})$.

<InteractiveFigure :figure="additions.illustrations.find(f => f.id === 'original-lebesgue-number')" />

The same style handles a different interval, cover and witness points:

<InteractiveFigure :figure="additions.illustrations.find(f => f.id === 'original-symmetric-lebesgue-number')" />

The balls are relative to $X$, so their strips stop at its endpoints. The
library verifies the cover over the whole interval by an interval sweep.
The displayed dots illustrate one choice of $x$ and $z$.

## Iterating a contraction

[Section 10.3, Exercise 5, printed pages 222–223](./chapter-10/complete-metric-spaces#printed-page-222)
considers $s_n=f^n(y)$ for a map that contracts distances by $0\le k<1$.
For an affine map $f(x)=ax+b$ on the complete real line, $k=|a|$ and the
unique fixed point is $z=b/(1-a)$. The exact identity
$|s_n-z|=k^n|y-z|$ shows convergence and explains the shrinking error curve.

<InteractiveFigure :figure="additions.illustrations.find(f => f.id === 'original-contraction-iterates')" />

A negative slope produces alternating iterates. The Substance parameters
change; the same style draws both the iteration and its error curve.

<InteractiveFigure :figure="additions.illustrations.find(f => f.id === 'original-alternating-contraction')" />

The diagrams show eight steps. The algebraic identity applies to every $n$;
the finite drawing represents it without inferring convergence from samples.

## The kernel of a group homomorphism

[Section 1.4, Exercise 2, printed page 15](./chapter-01/groups#printed-page-15)
asks why the elements sent to the identity form a subgroup. Reduction modulo
$3$ gives a homomorphism $f:Z_6\to Z_3$ with
$\ker f=\{\overline{0},\overline{3}\}$. The orange fiber is exactly the
preimage of the target identity.

<InteractiveFigure :figure="additions.illustrations.find(f => f.id === 'original-group-kernel-6-3')" />

The same Substance factory and Style illustrate $Z_8\to Z_4$:

<InteractiveFigure :figure="additions.illustrations.find(f => f.id === 'original-group-kernel-8-4')" />

The library checks the full operation tables, every map value and the subgroup
laws. Dragging a source element moves its incident arrow and adjusts the
kernel enclosure while its mathematical value stays fixed.

## Retracing a loop

[Section 11.3, Proposition 4, printed page 246](./chapter-11/fundamental-group#printed-page-246)
contracts $a\mathbin{\#}a^{-1}$ to the constant loop while keeping its
basepoint fixed. Orange shows the current image; gray retains the original
curve as a reference. The two arrows distinguish outgoing and return traversal.

<InteractiveFigure :figure="additions.illustrations.find(f => f.id === 'original-retracing-circle')" />

The same Style and rescaling work for a self-intersecting loop:

<InteractiveFigure :figure="additions.illustrations.find(f => f.id === 'original-retracing-figure-eight')" />

[Read the homotopy formula and its endpoint behavior.](./loop-retracing)

[Return to the book index](./index)

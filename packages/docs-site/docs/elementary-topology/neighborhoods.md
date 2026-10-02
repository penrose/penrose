---
title: Neighborhoods — Elementary Topology
description: Section 2.2 of Michael C. Gemignani's Elementary Topology, with figures rendered by Penrose.
---

# 2.2 Neighborhoods

Let $X,D$ be a metric space. If $x$ is any point of $X$, then we may want
to consider all the points of $X$ within a certain distance of $x$, that is, the
set of points of $X$ which are within some degree of nearness to $x$.

**Definition 2.** If $X,D$ is a metric space, $x\in X$, and $\rho$ is any positive
real number, then the $D$-$\rho$-neighborhood of $x$ is defined to be the set
of all points $y$ of $X$ such that $D(x,y)<\rho$; that is, the $D$-$\rho$-neighborhood
of $x$ is defined to be

$$
\{y\in X\mid D(x,y)<\rho\}.
$$

Where there is no danger of ambiguity, the $D$-$\rho$-neighborhood of $x$ will
be called the $\rho$-neighborhood of $x$ and will be denoted by $N(x,\rho)$.

**Example 6.** Let $R^2$ be the coordinate plane. Figures 2.1 through 2.4
illustrate the $1$-neighborhoods of $(0,0)$ with respect to the metrics $D$,
$D_1$, $D_2$, and $D_3$ of Examples 2 and 3. The reader should be sure to verify
these figures.

<div class="topology-figure-grid">
  <figure><img src="/elementary-topology/figures/figure-2.1.svg" alt="The unit disk about the origin in the Pythagorean metric." /><figcaption>Figure 2.1</figcaption></figure>
  <figure><img src="/elementary-topology/figures/figure-2.2.svg" alt="The diamond shaped unit neighborhood about the origin in metric D₁." /><figcaption>Figure 2.2</figcaption></figure>
  <figure><img src="/elementary-topology/figures/figure-2.3.svg" alt="The singleton containing the origin in discrete metric D₂." /><figcaption>Figure 2.3</figcaption></figure>
  <figure><img src="/elementary-topology/figures/figure-2.4.svg" alt="The square shaped unit neighborhood about the origin in metric D₃." /><figcaption>Figure 2.4</figcaption></figure>
</div>

Note that the $D_1$-$1$-neighborhood of $(0,0)$ is a subset of the $D$-$1$-neighborhood
of $(0,0)$. Since

$$
D((x_1,y_1),(x_2,y_2))\leq D_1((x_1,y_1),(x_2,y_2))
$$

for any two points $(x_1,y_1)$ and $(x_2,y_2)$ of $R^2$ (Exercise 2), the
$D_1$-$\rho$-neighborhood of any point $(x',y')$ of $R^2$ is a subset of the
$D$-$\rho$-neighborhood for any positive real number $\rho$. However, a simple
calculation shows that the $D$-$\rho/\sqrt{2}$-neighborhood of $(x',y')$ is a
subset of the $D_1$-$\rho$-neighborhood of $(x',y')$ (Fig. 2.5). Note, however,
that if $\rho\leq1$, there is no positive number $q$ such that either the
$D$-$q$-neighborhood of $(x',y')$ or the $D_1$-$q$-neighborhood of $(x',y')$
is a subset of the $D_2$-$\rho$-neighborhood of $(x',y')$ which consists of
$(x',y')$ alone.

<figure class="topology-single-figure"><img src="/elementary-topology/figures/figure-2.5.svg" alt="A taxicab neighborhood between concentric Euclidean neighborhoods of radii ρ and ρ divided by the square root of two." /><figcaption>Figure 2.5</figcaption></figure>

**Example 7.** Let $X,D$ be the metric space described in Example 5. Suppose
$f\in X$ and $\rho>0$. We may draw a $\rho$-collar about the graph of $f$ as
shown in Fig. 2.6. Then the $D$-$\rho$-neighborhood of $f$ will consist of all
functions from $[0,1]$ into $[0,1]$ whose graphs lie within the $\rho$-collar of $f$.

<figure class="topology-single-figure"><img src="/elementary-topology/figures/figure-2.6.svg" alt="The graph of f and its vertical ρ collar, with values f(x), f(x)+ρ, and f(x)−ρ." /><figcaption>Figure 2.6</figcaption></figure>

## Exercises

1. Confirm Figs. 2.1 through 2.4.
2. Prove that $D((x_1,y_1),(x_2,y_2))\leq D_1((x_1,y_1),(x_2,y_2))$ for
   any two points $(x_1,y_1)$ and $(x_2,y_2)$ in $R^2$ as claimed in Example 6.
   Also in Example 6, carry out the computation which shows that the
   $D$-$\rho/\sqrt{2}$-neighborhood of $(x,y)$ is a subset of the
   $D_1$-$\rho$-neighborhood of $(x,y)$ for any $(x,y)\in R^2$.

<style>
.topology-figure-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 1.5rem; margin: 1.75rem 0; }
.topology-figure-grid figure, .topology-single-figure { margin: 0; text-align: center; }
.topology-figure-grid img { width: 100%; background: white; }
.topology-figure-grid figcaption, .topology-single-figure figcaption { font-family: Georgia, serif; font-size: 0.875rem; margin-top: 0.4rem; }
.topology-single-figure { max-width: 28rem; margin: 1.75rem auto; }
.topology-single-figure img { width: 100%; background: white; }
@media (max-width: 480px) { .topology-figure-grid { grid-template-columns: 1fr; } }
</style>

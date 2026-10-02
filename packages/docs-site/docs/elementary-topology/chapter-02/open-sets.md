---
title: Open Sets — Elementary Topology
description: The available text of section 2.3, printed pages 22–23, of Elementary Topology, second edition.
---

# 2.3 Open Sets

::: warning Missing source page 21
Printed page 21 is absent from the supplied scan. The section opening, Definition 3, Proposition 1's statement, and the beginning of the following proof are unavailable. This page begins with the exact available continuation on printed page 22; no missing text has been reconstructed. The section title is supplied by the running header on printed page 23. Figure 2.7 is unlocated in the supplied source.
:::

<span id="printed-page-22"></span>

<!-- source: PDF 29, printed 22 -->

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-2.8.svg" alt="Figure 2.8: a neighborhood of w contained in a neighborhood of x." /><figcaption>Figure 2.8</figcaption></figure>

$\rho-D(x,w)$. Then if $z\in N(w,q)$, we have $D(w,z)<\rho-D(x,w)$. Therefore

$$
D(x,z)\leq D(x,w)+D(w,z)<D(x,w)+\rho-D(x,w)=\rho.
$$

We thus have that $z\in N(x,\rho)$; hence $N(w,q)\subset N(x,\rho)$. Therefore $N(x,\rho)$ is an open set.

**Proposition 2.** Let $X,D$ be a metric space. Then

a) $X$ and $\phi$ are both ($D$-) open sets,

b) the intersection of any two open sets is again an open set, and

c) the union of any family of open sets is again an open set.

_Proof_

a) If $x\in X$ and $\rho>0$, then $N(x,\rho)\subset X$. Therefore $X$ is an open set. Since $\phi$ contains no points whatsoever, it is true that for each $x\in\phi$ (there is no such $x$) and any $\rho>0$, $N(x,\rho)\subset\phi$; hence $\phi$ is also an open set.

b) Suppose $U$ and $V$ are open subsets of $X$ and $x\in U\cap V$. Since $U$ is open, there is a positive number $\rho_1$ such that $N(x,\rho_1)\subset U$. Since $V$ is open, there is a positive number $\rho_2$ such that $N(x,\rho_2)\subset V$. Set $\rho=\min(\rho_1,\rho_2)$. Then $N(x,\rho)\subset U\cap V$; therefore $U\cap V$ is open.

c) Let $\{U_i\}$, $i\in I$, be any family of open subsets of $X$ and $x\in\bigcup_I U_i$. Then $x\in U_i$ for some $i$. Since $U_i$ is open, there is a positive number $\rho$ such that $N(x,\rho)\subset U_i$. But then $N(x,\rho)\subset\bigcup_I U_i$; hence $\bigcup_I U_i$ is open.

**Proposition 3.** Let $X$ be a set with metric $D$. A subset $U$ of $X$ is open if and only if $U$ is the union of a family of $\rho$-neighborhoods.

<span id="printed-page-23"></span>

<!-- source: PDF 30, printed 23 -->

_Proof._ Assume $U$ to be the union of a family of $\rho$-neighborhoods. Since each $\rho$-neighborhood is open by Proposition 1, $U$ is the union of a family of open sets. Therefore $U$ is open by Proposition 2(c).

Suppose that $U$ is an open subset of $X$. Then for each $x\in U$, we can find at least one $N(x,\rho)$ such that $N(x,\rho)\subset U$. Since $N(x,\rho)\subset U$ for each $x\in U$, $\bigcup_U N(x,\rho)\subset U$. On the other hand, each $x\in U$ is an element of at least $N(x,\rho)$; hence $U\subset\bigcup_U N(x,\rho)$. Therefore $U=\bigcup_U N(x,\rho)$.

If $X$ is any set, then $X$ and $\phi$ have been shown to be $D$-open sets for any metric $D$ which can be defined on $X$. If $D_1$ and $D_2$ are any two metrics for $X$, it is not necessarily true that each $D_1$-open set is $D_2$-open, or that each $D_2$-open set is $D_1$-open.

**Example 9.** Let $D$ and $D_2$ be the metrics defined on $R^2$ as in Examples 2 and 3. For any $(x,y)\in R^2$, the $D_2$-1-neighborhood of $(x,y)$ is precisely $\{(x,y)\}$, since $(x,y)$ is the only point which is less than $D_2$-distance 1 from itself. Therefore $\{(x,y)\}$ is $D_2$-open, since it is a $D_2$-neighborhood. But $\{(x,y)\}$ is not $D$-open, since for any $\rho>0$, the $D$-$\rho$-neighborhood of $(x,y)$ contains infinitely many points besides $(x,y)$. (See Exercise 4 also.)

## Exercises

1. Let $X,D$ be a metric space. Suppose that $x$ and $y$ are two distinct points of $X$. Prove that there are open sets $U$ and $V$ in $X$ such that $x\in U$, $y\in V$ and $U\cap V=\phi$. [_Hint:_ Let $U=N(x,\frac12D(x,y))$.]
2. Determine which of the following subsets of the plane $R^2$ with the Pythagorean metric are open.

   a) $\{(x,y)\mid x<0\}$

   b) $\{(x,y)\mid x+y>5\}$

   c) $\{(x,y)\mid x^2+y^2<1,\text{ or }(x,y)=(1,0)\}$

   d) $\{(x,y)\mid x>2\text{ and }y\leq3\}$

3. Let $\rho$ be any positive number. Prove that any $D$-$\rho$-neighborhood of $R^2$ is $D_1$-, $D_2$-, and $D_3$-open.
4. Prove that any subset of $R^2$ which is $D$-open is $D_1$-open and, conversely, that any subset of the plane which is $D_1$-open is $D$-open.
5. Make appropriate sketches for the proofs of (b) and (c) in Proposition 2.
6. Let $D_1$ and $D_2$ be possible metrics for a set $X$. $D_1$ and $D_2$ are said to be _equivalent_ if every $D_1$-open set is $D_2$-open and every $D_2$-open set is $D_1$-open. Prove that $D_1$ and $D_2$ are equivalent if and only if, given any $x\in X$ and any $\rho>0$, there are positive numbers $\rho_1$ and $\rho_2$ such that the $D_1$-$\rho_1$-neighborhood of $x$ is a subset of the $D_2$-$\rho$-neighborhood of $x$, and the $D_2$-$\rho_2$-neighborhood of $x$ is a subset of the $D_1$-$\rho$-neighborhood of $x$.
7. Let $L$ be any straight line in $R^2$. Prove that $R^2-L$ is open with respect to all metrics introduced on $R^2$ thus far in the text. Try to find a metric on $R^2$ for which $R^2-L$ is not necessarily open.

[Continue to 2.4 Closed Sets](./closed-sets)

<style>
.topology-chapter-figure { max-width: 28rem; margin: 1.75rem auto; text-align: center; }
.topology-chapter-figure img { width: 100%; background: white; }
.topology-chapter-figure figcaption { font-family: Georgia, serif; font-size: 0.875rem; margin-top: 0.4rem; }
.topology-chapter-figure-pair { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 1.5rem; margin: 1.75rem 0; }
.topology-chapter-figure-pair .topology-chapter-figure { margin: 0; }
@media (max-width: 480px) { .topology-chapter-figure-pair { grid-template-columns: 1fr; } }
</style>

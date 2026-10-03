---
title: Continuity — Elementary Topology
description: The available text of section 2.6, printed pages 30–34, of Elementary Topology, second edition.
---

# 2.6 Continuity

::: warning Missing source page 29
Printed page 29 is absent from the supplied scan. The section opening and Definition 6, referenced below, are unavailable and have not been reconstructed. The title is retained from the running headers on the available pages. This transcription begins at printed page 30 and includes the §2.6 exercises at the top of printed page 34.
:::

<span id="printed-page-30"></span>

<!-- source: PDF 36, printed 30 -->

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-2.15.svg" alt="A neighborhood of a in X mapped inside a neighborhood of f(a) in Y." /><figcaption>Figure 2.15</figcaption></figure>

**Example 14.** Let $R$ be the space of real numbers with the absolute value metric. Then the function defined by $f(x)=2x+3$ from $R$ to $R$ is continuous. A simple calculation shows that $f(N(a,\rho/2))\subset N(f(a),\rho)$, for any $a\in R$.

It is usually quite awkward to prove the continuity of a function directly from Definition 6. We therefore need propositions which will help us determine whether or not a function is continuous, but which are, in general, easier to apply than Definition 6.

**Proposition 8.** Let $f$ be a function from the metric space $X,D$ into the metric space $Y,D'$. Then $f$ is continuous if and only if, given any open set $U$ of $Y$,

$$
f^{-1}(U)=\{x\in X\mid f(x)\in U\}
$$

is an open subset of $X$.

_Proof._ First suppose that $f$ is continuous and that $U$ is an open subset of $Y$. Let $x\in f^{-1}(U)$; then $f(x)\in U$. Since $U$ is open, there is a positive number $\rho$ such that $N(f(x),\rho)\subset U$. Since $f$ is continuous, there is a positive number $q$ such that

$$
f(N(x,q))\subset N(f(x),\rho)\subset U.
$$

Therefore $N(x,q)\subset f^{-1}(U)$. Since, for $x\in f^{-1}(U)$, we have found $q>0$ such that $N(x,q)\subset f^{-1}(U)$, then $f^{-1}(U)$ is open.

Suppose, on the other hand, that $f^{-1}(U)$ is an open subset of $X$ whenever $U$ is an open subset of $Y$. We have previously shown that if $f(a)\in Y$ and if $\rho$ is any positive number, then $N(f(a),\rho)$ is an open subset of $Y$; therefore $f^{-1}(N(f(a),\rho))$ is an open subset of $X$. There is therefore a positive number $q$ such that

$$
N(a,q)\subset f^{-1}(N(f(a),\rho)).
$$

We have then that for this $q$, $f(N(a,q))\subset N(f(a),\rho)$; hence $f$ is continuous.

<span id="printed-page-31"></span>

<!-- source: PDF 37, printed 31 -->

The following proposition is quite similar to Proposition 8, but is often easier to apply.

**Proposition 9.** Let $f$ be a function from the metric space $X,D$ into the metric space $Y,D'$. Then $f$ is continuous if and only if given any $D'$-$\rho$-neighborhood $U$ in $Y$, $f^{-1}(U)$ is an open subset of $X$.

_Proof._ Suppose $f$ is continuous. If $U$ is any $D'$-$\rho$-neighborhood in $Y$, then $U$ is an open subset of $Y$. Therefore $f^{-1}(U)$ is an open subset of $X$ by Proposition 8.

Suppose that $f^{-1}(U)$ is an open subset of $X$ whenever $U$ is a $D'$-$\rho$-neighborhood in $Y$. Let $V$ be any open subset of $Y$. By Proposition 8 we will have shown that $f$ is continuous if we show that $f^{-1}(V)$ is an open subset of $X$. Now $V$ is the union of $D'$-$\rho$-neighborhoods (Proposition 3), say $V=\bigcup_I U_i$, where each $U_i$ is a $D'$-$\rho$-neighborhood. Then

$$
f^{-1}(V)=f^{-1}\left(\bigcup_I U_i\right)=\bigcup_I f^{-1}(U_i).
$$

But each $f^{-1}(U_i)$ is by hypothesis an open subset of $X$; hence $f^{-1}(V)$ is the union of a family of open subsets of $X$ and consequently is open. Therefore $f$ is continuous.

**Example 15.** Using Proposition 9, we will show that the function $f$ from $R^2$ with the Pythagorean metric to $R$, the set of real numbers with the absolute value metric, defined by $f(x,y)=x$, is continuous. If $a\in R$ and $\rho$ is any positive real number, then $N(a,\rho)$ is the open interval $(a-\rho,a+\rho)$. Then $f^{-1}(N(a,\rho))$ is easily seen to be

$$
\{(x,y)\mid a-\rho<x<a+\rho\}
$$

(Fig. 2.16), an open subset of $R^2$. Therefore, by Proposition 9, $f$ is continuous.

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-2.16.svg" alt="The inverse image of a real interval under projection is an open strip in the plane." /><figcaption>Figure 2.16</figcaption></figure>

**Example 16.** The identity function $i$ [defined by $i(x,y)=(x,y)$] from $R^2$ with metric $D$ onto $R^2$ with metric $D_1$ is continuous, since any $D$-open subset of $R^2$ is $D_1$-open, and conversely (Section 2.3, Exercise 4). That

<span id="printed-page-32"></span>

<!-- source: PDF 38, printed 32 -->

is, if $U$ is any open subset of $R^2,D_1$, then $i^{-1}(U)=i(U)=U$ is also an open subset of $R^2,D$; hence $i$ is continuous. Since $i^{-1}=i$, and since each $D$-open set is also $D_1$-open, we see that $i^{-1}$ is continuous as a function from $R^2,D_1$ onto $R^2,D$. It is quite possible, however, for a one-one function from a metric space, $X,D$ onto a metric space $Y,D'$ to be continuous without $f^{-1}$ being continuous, as will be demonstrated in Example 17.

The following proposition relates continuity with the convergence of sequences.

**Proposition 10.** Let $f$ be a function from the metric space $X,D$ into the metric space $Y,D'$. Then $f$ is continuous if and only if given any sequence $S=\{s_n\}$, $n\in N$, in $X$ such that $s_n\to y$,

$$
f(S)=\{f(s_n)\},\quad n\in N,\quad\text{converges to }f(y)\text{ in }Y.
$$

_Proof._ Suppose that $f$ is continuous, but that there is a sequence $S=\{s_n\}$, $n\in N$, in $X$ such that $s_n\to y$, but $f(S)$ does not converge to $f(y)$. Since $f(S)$ does not converge to $f(y)$, there must be a positive number $\rho$ such that $N(f(y),\rho)$ excludes infinitely many of the $f(s_n)$. But since $f$ is continuous, there is a positive number $q$ such that $f(N(y,q))\subset N(f(y),\rho)$. By assumption, $s_n\to y$; hence $N(y,q)$ contains all but finitely many of the $s_n$. This implies that $N(f(y),\rho)$ contains all but finitely many of the $f(s_n)$, a contradiction.

Conversely, suppose that given any sequence $S=\{s_n\}$, $n\in N$, in $X$ such that $s_n\to y$, then $f(S)$ converges to $f(y)$; assume that $f$ is not continuous. Then, since $f$ is not continuous, there is a point $f(a)$ in $Y$ and a positive number $\rho$ for which there is no positive number $q$ such that

$$
f(N(a,q))\subset N(f(a),\rho).
$$

Consider the family $\{U_n\}$, $n\in N$, of neighborhoods of $a$, where $U_n=N(a,1/n)$. For each $n$ we can select $s_n\in U_n$ such that $f(s_n)\notin N(f(a),\rho)$; this selection is possible because $f$ is not continuous. Then $s_n\to a$ (Exercise 1); but $\{f(s_n)\}$, $n\in N$, is a sequence in $Y$ which does not converge to $f(a)$, since $N(f(a),\rho)$ by construction of $\{s_n\}$, $n\in N$, contains no $f(s_n)$ whatsoever. This contradicts our initial hypothesis that $f$ preserves the limits of sequences; hence $f$ could not be discontinuous. Therefore $f$ is continuous.

A continuous function is thus seen to be one which in some sense preserves the convergence of sequences. (See Exercise 7 also.)

**Example 17.** The identity function $i$ from $R^2$ with metric $D$ onto $R^2$ with metric $D_2$ (Example 3) is not continuous. For the sequence defined by $s_n=(1,1/n)$ converges to $(1,0)$ with respect to metric $D$ (Example 12),

<span id="printed-page-33"></span>

<!-- source: PDF 39, printed 33 -->

but the sequence $\{i(s_n)\}=\{s_n\}$, $n\in N$ does not converge to $i(1,0)=(1,0)$ with respect to $D_2$. Note, however, that

$$
i^{-1}:R^2,D_2\to R^2,D
$$

is continuous. This follows from the fact that any sequence in $R^2$ which converges with respect to $D_2$ is essentially a constant sequence (Section 2.5, Exercise 4), and any constant sequence converges with respect to any metric whatsoever on $R^2$. We thus see that even the identity function from a set $X$ with one metric onto the same set with a different metric may fail to be continuous.

## Exercises

1. In the converse part of Proposition 10, prove $s_n\to a$.
2. Discuss the continuity of the function in each of the following:

   a) the function defined by $f(x)=5x+7$ from the space $R,D$ onto itself, where $R$ is the set of real numbers and $D$ is the absolute value metric;

   b) the function defined by $f(x,y)=x+y$ from $R^2,D_1$ onto $R,D$ [with $R,D$ as in (a)];

   c) the function defined by $f(g)=g(0)$ from the space $X,D$ of Example 5 onto the closed interval $[0,1]$ considered as a subspace of the space of real numbers with the absolute value metric;

   d) the identity function $i$ from $R^2,D_1$ onto $R^2,D_3$.

3. Suppose $f$ to be a function from a metric space $X,D$ into the metric space $Y,D'$ such that $D(x,x')\geq kD'(f(x),f(x'))$, where $k$ is a constant positive real number. Prove that $f$ is continuous.
4. Suppose that $f$ is a continuous function from $X,D$ into $Y,D'$, and $g$ a continuous function from $Y,D'$ into $Z,D''$. Prove that $g\circ f$ is a continuous function from $X,D$ into $Z,D''$.
5. Assume $f$ to be a function from $X,D$ onto a subspace $W$ of $Y,D'$. Prove that $f$ is continuous as a function from $X,D$ into $Y,D'$ if and only if $f$ is continuous as a function from $X,D$ onto $W,D'\mid W$.
6. Suppose $W$ is a subset of $Y,D$. Prove that the function $i:W\to Y$ defined by $i(w)=w$ for each $w\in W$ is continuous as a function from $W,D\mid W$ into $Y,D$.
7. Prove that a function $f$ from a space $X,D$ into a space $Y,D'$ is continuous if and only if given any convergent sequence $S$ in $X$, $f(S)$ is a convergent sequence in $Y$. [_Hint:_ It must be shown that if $S$ converges to $x$ in $X$, then $f(S)$ converges to $f(x)$.]
8. The concept of equivalent metrics was introduced in Section 2.3, Exercise 6. Prove that metrics $D$ and $D'$ on a set $X$ are equivalent if and only if the identity map from both $X,D$ onto $X,D'$ and from $X,D'$ onto $X,D$ is continuous.

<span id="printed-page-34"></span>

<!-- source: PDF 40, printed 34; section 2.6 fragment -->

9. a) Suppose $f:R^2\to R^2$ takes any circle in $R^2$ onto a circle. Need $f$ be continuous if $R^2$ has the usual Pythagorean metric?

   b) Suppose $f:R^2\to R^2$ takes collinear points into collinear points. Need $f$ be continuous?

10. Prove that the set $f^{-1}(N(a,\rho))\subset R^2$ of Example 15 is open with respect to the usual Pythagorean metric.

[Continue to 2.7 “Distance” Between Two Sets](./distance-between-sets)

<style>
.topology-chapter-figure { max-width: 28rem; margin: 1.75rem auto; text-align: center; }
.topology-chapter-figure img { width: 100%; background: white; }
.topology-chapter-figure figcaption { font-family: Georgia, serif; font-size: 0.875rem; margin-top: 0.4rem; }
.topology-chapter-figure-pair { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 1.5rem; margin: 1.75rem 0; }
.topology-chapter-figure-pair .topology-chapter-figure { margin: 0; }
@media (max-width: 480px) { .topology-chapter-figure-pair { grid-template-columns: 1fr; } }
</style>

---
title: Finer and Coarser Topologies — Elementary Topology
description: The available text of section 3.4, printed pages 53–55, of Elementary Topology, second edition.
---

# 3.4 Finer and Coarser Topologies

::: warning Missing source page 52
Printed page 52 is absent from the supplied scan. This page begins with Definition 6 on printed page 53; the section title is retained from that page's running header. No unavailable section opening or preceding text is supplied. Printed page 55 continues with §3.5 after the exercises.
:::

<span id="printed-page-53"></span>

<!-- source: PDF 56, printed 53 -->

**Definition 6.** Let $X$ be any set, and suppose that $\tau$ and $\tau'$ are two topologies on $X$. Then $\tau$ is said to be _finer_ than $\tau'$ if $\tau'\subset\tau$, that is, any $\tau'$-open set is also a $\tau$-open set. $\tau$ is said to be _strictly finer_ than $\tau'$ if $\tau$ is finer than $\tau'$, but $\tau\neq\tau'$. If $\tau$ is finer than $\tau'$, then we may say that $\tau'$ is _coarser_ than $\tau$.

**Proposition 8.** Let $X$ be any set. Then two topologies $\tau$ and $\tau'$ on $X$ are equal if and only if $\tau$ is finer than $\tau'$ and $\tau'$ is finer than $\tau$.

_Proof._ $\tau$ finer than $\tau'$ means $\tau'\subset\tau$. $\tau'$ finer than $\tau$ means $\tau\subset\tau'$. Therefore $\tau=\tau'$.

**Example 14.** Let $X$ be any set. Then the discrete topology on $X$ is finer than any topology on $X$ and is strictly finer than any other topology on $X$. The trivial topology on $X$ is coarser than any topology on $X$.

**Example 15.** Let $\tau$ and $\tau'$ be any two topologies on some set $X$. Let

$$
\mathfrak{S}=\tau\cap\tau'\quad\text{and}\quad\mathfrak{S}'=\tau\cup\tau'.
$$

Then $\mathfrak{S}$ and $\mathfrak{S}'$ are subbases for unique topologies $\tau_1$ and $\tau_2$, respectively, on $X$. (By Proposition 5, any collection of subsets of $X$ is a subbasis for a unique topology on $X$. The reader should be certain that he understands that $\tau\cap\tau'$ is the family of all subsets of $X$ which are both $\tau$-open and $\tau'$-open, e.g. $X$ and $\phi$, and _not_ intersections of $\tau$-open and $\tau'$-open sets.) $\tau_1$ is coarser than both $\tau$ and $\tau'$, since any $\tau_1$-open set is both $\tau$-open and $\tau'$-open. $\tau_2$, on the other hand, is finer than both $\tau$ and $\tau'$. The reader should also see Exercise 5.

**Proposition 9.** Let $\tau$ and $\tau'$ be two topologies on some set $X$. Suppose that $\mathfrak{N}$ and $\mathfrak{N}'$ are open neighborhood systems for $\tau$ and $\tau'$, respectively. Then $\tau\subset\tau'$ if and only if for each $x\in X$ and $N\in\mathfrak{N}_x$, there is $N'\in\mathfrak{N}'_x$ such that $N'\subset N$.

_Proof._ Assume $U$ to be any $\tau$-open set and $x\in U$. Then there is $N\in\mathfrak{N}_x$ such that $x\in N\subset U$. Now if there is $N'\in\mathfrak{N}'_x$ such that $x\in N'\subset N$, then $x\in N'\subset U$; hence $U$ is also $\tau'$-open. Therefore $\tau\subset\tau'$. On the other hand, if $\tau\subset\tau'$, then since $N\in\tau$, $N\in\tau'$; hence there is $N'\in\mathfrak{N}'_x$ such that $N'\subset N$. (Note that the reason for virtually every step in this proof is Definition 5v.)

**Corollary 1.** Let $\tau$ and $\tau'$ be two topologies on the set $X$. Suppose $\mathfrak{N}$ and $\mathfrak{N}'$ are open neighborhood systems for $\tau$ and $\tau'$, respectively. Then $\tau=\tau'$ if and only if for each $x\in X$ and $N\in\mathfrak{N}_x$, there is $N'\in\mathfrak{N}'_x$ such that $N'\subset N$, and for each $N'\in\mathfrak{N}'_x$, there is $N\in\mathfrak{N}_x$ such that $N\subset N'$.

<span id="printed-page-54"></span>

<!-- source: PDF 57, printed 54 -->

_Proof._ Applying Proposition 9, we see that this corollary merely states that $\tau=\tau'$ if and only if $\tau\subset\tau'$ and $\tau'\subset\tau$.

**Corollary 2.** Suppose $X$ to be a set and $D$ and $D'$ possible metrics on $X$. Then the topology induced by $D$ is the same as the topology induced by $D'$ if and only if for each $x\in X$ and positive number $\rho$, there are positive numbers $\rho_1$ and $\rho_2$ such that the $D$-$\rho_1$-neighborhood of $x$ is a subset of the $D'$-$\rho$-neighborhood of $x$, and the $D'$-$\rho_2$-neighborhood of $x$ is a subset of the $D$-$\rho$-neighborhood of $x$.

_Proof._ The $\rho$-neighborhoods of points in a metric space form an open neighborhood system for the metric topology (see Example 10). Corollary 2 then is merely a restatement of Corollary 1 applied to metric spaces.

**Example 16.** Suppose that $R^2$, the coordinate plane, is given either the Pythagorean metric $D$, or the metric $D_1$ of Chapter 2, Example 3. It was shown in Chapter 2, Example 6 that the hypotheses of Corollary 2 apply; hence the topologies induced by $D$ and $D_1$ on $R^2$ are the same.

**Example 17.** Let $\tau$ be the topology on $R^2$ which is defined by the open neighborhood system described in Example 12. It is easily verified (Fig. 3.3) that $\tau$ is the same topology as that induced on $R^2$ by the Pythagorean metric (Exercise 7).

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-3.3.svg" alt="Figure 3.3: Open disks refining one another at a common point." /><figcaption>Figure 3.3</figcaption></figure>

The reader might be tempted to conjecture that two topologies $\tau$ and $\tau'$ on some set $X$ are equal if and only if given bases $\mathfrak{B}$ and $\mathfrak{B}'$ for $\tau$ and $\tau'$, respectively, for each $B\in\mathfrak{B}$ there is $B'\in\mathfrak{B}'$ with $B'\subset B$, and for each $B'\in\mathfrak{B}'$, there is $B\in\mathfrak{B}$ such that $B\subset B'$. This conjecture is, however, false, as is shown by Example 11 and Section 3.3, Exercise 3. Each interval of the form $[x,a)$ contains some interval of the form $(p,q)$ [though of course $(p,q)$ could not contain $x$], and each interval of the form $(p,q)$ contains some interval of the form $[x,a)$. The set of all half-open intervals of the form $[x,a)$ forms a basis for a topology $\tau'$, and the set of open intervals also forms a basis for a topology $\tau$. If the conjecture were correct, these two topologies should be equal, which they are not.

<span id="printed-page-55"></span>

<!-- source: PDF 58, printed 55; section 3.4 fragment -->

## Exercises

1. In Example 15, prove as asserted that $\tau_1$ is coarser than both $\tau$ and $\tau'$, and that $\tau_2$ is finer than both $\tau$ and $\tau'$.
2. Prove that the topology induced on $R$, the set of real numbers, by the absolute value metric is the same as the topology for which the set of all open intervals is a basis.
3. Prove that the topologies induced on $R^2$ by the metrics $D_1$ and $D_3$, Chapter 2, Example 3, are equal.
4. Define a metric $D'$ on the set $R$ of real numbers by $D'(x,y)=3|x-y|$. How does the topology induced on $R$ by $D'$ compare with the topology induced on $R$ by the absolute value metric? Answer this same question with $D'$ replaced by $D''$, where $D''$ is defined by $D''(x,y)=|x-y|^2$.
5. In Example 15, prove that $\tau_1$ is the finest topology which is coarser than both $\tau$ and $\tau'$, and that $\tau_2$ is the coarsest topology which is finer than $\tau$ and $\tau'$.
6. Find all possible topologies on $\{x,y,z\}$. Order these topologies as to fineness and coarseness. Construct a diagram which illustrates the relationships between the topologies.
7. In Example 17, prove that $\tau$ is the same as the topology induced on $R^2$ by the Pythagorean metric.
8. Suppose $\mathfrak{B}$ and $\mathfrak{B}'$ are bases for topologies $\tau$ and $\tau'$, respectively, on a set $X$. Suppose that each member of $\mathfrak{B}'$ contains a member of $\mathfrak{B}$. Are $\tau$ and $\tau'$ necessarily comparable? If so, in what way?

[Continue to 3.5 Derived Sets](./derived-sets)

<style>
.topology-chapter-figure { max-width: 28rem; margin: 1.75rem auto; text-align: center; }
.topology-chapter-figure img { width: 100%; background: white; }
.topology-chapter-figure figcaption { font-family: Georgia, serif; font-size: 0.875rem; margin-top: 0.4rem; }
</style>

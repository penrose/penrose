---
title: T₀- and T₁-Spaces — Elementary Topology
description: The available text of section 5.1 of the supplied second-edition scan.
---

# 5.1 $T_0$- and $T_1$-Spaces

::: info Transcription note
Source: printed pages 91 and 93–95 (PDF pages 91–94). Printed page 92 is missing. Printed page 95 continues with §5.2 after Exercise 10. Handwritten annotations overwrite two point labels in the proof of Proposition 2; the visible annotated reading, $z$ and $y$, is retained here and the obscured underlying glyphs are not reconstructed.
:::

<span id="printed-page-91"></span>

<!-- Source: PDF page 91, printed page 91. -->

Propositions 12 and 13 of Chapter 2, and Section 2.3, Exercise 1 furnish us with examples of “separation” properties for metric spaces. A “separation” property really does imply separation in the following sense: Given any two nonintersecting subsets $A$ and $B$ of a topological space $X$, where $A$ and $B$ are subsets of a certain type, there are other nonintersecting subsets $U$ and $V$ of $X$, generally open sets, such that $A\subset U$ and $B\subset V$ (Fig. 5.1). In other words, being separated in a topological space is a bit stronger than merely being disjoint. There are various degrees of separation. As we saw in Chapter 2, it is possible to separate disjoint closed subsets of a metric space in a rather strong way. But not all topological spaces are metric spaces; hence not all topological spaces have strong separation properties. Although most important topological spaces are at least $T_2$ (see below), many are not.

<figure class="topology-chapter-figure">
  <img src="/elementary-topology/figures/figure-5.1.svg" alt="Disjoint subsets A and B contained in disjoint open sets U and V." />
  <figcaption>Figure 5.1</figcaption>
</figure>

Many topologists will not even consider a topological space which is not $T_2$, and some won't touch anything which is not at least _normal_. But since it is hoped that the reader has not yet developed personal prejudices, at least in the area of topology, and since, too, the reader has not yet begun to specialize to the point where he feels justified in throwing out whatever does not fall in his sphere of interest, we shall even study some of the lesser separation axioms. The first separation axiom follows.

**Definition 1.** A topological space $X,\tau$ is said to be $T_0$ if given any two distinct points $x$ and $y$ of $X$, there is a neighborhood of at least one which does not contain the other.

::: warning Missing source page 92
Printed page 92 is absent. The next available page begins within a paragraph. Proposition 1, its corollary, and any unavailable examples or discussion are not reconstructed. References to that material in the supplied exercises are retained.
:::

<span id="printed-page-93"></span>

<!-- Source: PDF page 92, printed page 93. The opening is a source fragment. -->

space each one-point subset is closed, whereas this is not necessarily true in a pseudometric space. In fact, it is definitely false with a pseudometric space unless the pseudometric is a metric. For if $x$ and $y$ are distinct points such that $D(x,y)=0$, then every open set which contains $x$ contains $y$, and every open set which contains $y$ contains $x$; hence $x$ and $y$ cannot be separated. Therefore $\{x,y\}$ is a subset of $\operatorname{Cl}\{x\}$ and $\operatorname{Cl}\{y\}$, and it follows that $\operatorname{Cl}\{x\}\ne\{x\}$ and $\operatorname{Cl}\{y\}\ne\{y\}$. A pseudometric space, then, is generally not even $T_0$.

A slightly stronger separation property than $T_0$ is given by the following.

**Definition 2.** A space $X,\tau$ is said to be $T_1$ if for any two distinct points $x$ and $y$ of $X$, there is a neighborhood of $x$ which does not contain $y$ and a neighborhood of $y$ which does not contain $x$.

**Proposition 2.** A space $X,\tau$ is $T_1$ if and only if for each $x\in X$,

$$
\operatorname{Cl}\{x\}=\{x\}.
$$

_Proof._ Suppose $X$ is $T_1$, and suppose there is $x\in X$ such that $z\in\operatorname{Cl}\{x\}$, $z\ne x$. Then every neighborhood of $z$ must contain $x$. Hence there is no neighborhood of $z$ which excludes $x$, a contradiction since $X$ is assumed to be $T_1$. Therefore $\operatorname{Cl}\{x\}=\{x\}$ for all $x\in X$.

Now assume that $\operatorname{Cl}\{x\}=\{x\}$ for each $x\in X$ and that $y$ and $z$ are distinct points of $X$. If every neighborhood of $z$ contains $y$, then

$$
z\in\operatorname{Cl}\{y\}=\{y\};
$$

hence $y=z$, a contradiction. Therefore there is a neighborhood of $y$ which excludes $z$. Similarly, there is a neighborhood of $z$ which does not contain $y$; hence $X$ is $T_1$.

**Corollary.** A space $X,\tau$ is $T_1$ if and only if every one-point subset of $X$ is closed.

The proof is left as an exercise.

**Example 3.** Every metric space is $T_1$, since every one-point subset of a metric space is closed (Chapter 2, Proposition 5).

**Example 4.** Let $N$ be the set of positive integers with the topology defined by making every finite subset of $N$ closed (Section 3.1, Exercise 6). Then every one-element subset of $N$ is closed; hence $N$ is a $T_1$-space. Suppose $x$ and $y$ are two distinct positive integers. Then any neighborhood of either $x$ or $y$ contains all but finitely many positive integers. Therefore, while it is possible to find a neighborhood of $x$ which excludes $y$ and a <span id="printed-page-94"></span><!-- Source: PDF page 93, printed page 94. --> neighborhood of $y$ which excludes $x$, it is not possible to find a neighborhood $U$ of $x$ and a neighborhood $V$ of $y$ such that $U\cap V=\phi$.

It should be clear from the definitions that any $T_1$-space is also a $T_0$-space. In Example 1 the reader can find an example of a space which is $T_0$ but is not $T_1$.

## Exercises

1. Prove the corollaries to Propositions 1 and 2.

2. Suppose $X$ is any finite set. Prove that the only topology on $X$ which makes $X$ into a $T_1$-space is the discrete topology. If $X$ is a set of $n$ elements, what is the fewest number of members a topology can have which makes $X$ into a $T_0$-space?

3. Let $X$ be any set and $D$ be a pseudometric on $X$. Define a relation $R$ on $X$ by $x\mathrel{R}y$ if $D(x,y)=0$, for any $x,y\in X$.

   a) Prove that $R$ is an equivalence relation on $X$.

   b) Let $X$ have the topology induced by $D$ (defining open sets as if $D$ were a metric). For each $x\in X$, let $\bar{x}$ denote the $R$-equivalence class of $x$. Define $\bar{D}(\bar{x},\bar{y})=D(x,y)$, for any $x,y\in X$.

   i) Prove that $\bar{D}$ is a metric for $X/R$.

   ii) Prove that the topology induced on $X/R$, considered merely as the set of equivalence classes, is the same as the identification topology on $X/R$.

   iii) Find a natural one-one correspondence between the open sets of $X,D$ and the open sets of $X/R,\bar{D}$.

   c) Suppose $X,\tau$ is any topological space. Find an equivalence relation $R$ on $X$ such that the identification space $X/R$ is $T_0$ and there is a natural one-one correspondence between the open sets of $X$ and the open sets of $X/R$.

4. Let $X=\{1,2,3\}$. Find all topologies on $X$ which are either $T_0$ or $T_1$.

5. Suppose a space $X$ with topology $\tau$ is $T_0$ or $T_1$. Prove that if $\tau'$ is any topology on $X$ which is finer than $\tau$, then the space $X,\tau'$ is also $T_0$ or $T_1$.

6. a) Prove that every subspace of a $T_1$-space is $T_1$.

   b) Prove that every subspace of a $T_0$-space is $T_0$.

   c) Prove that the product space of a countable family of nonempty $T_0$-spaces is $T_0$ if and only if each component space is $T_0$. Prove the corresponding statement for $T_1$-spaces.

7. Prove that if a space $X$ is homeomorphic to a space $Y$ and $X$ is $T_0(T_1)$, then $Y$ is also.

8. Prove that a space $X,\tau$ is $T_0$ if and only if distinct one-point subsets of $X$ have distinct closures.

<span id="printed-page-95"></span>

<!-- Source: PDF page 94, printed page 95; section 5.1 fragment. -->

9. Prove or disprove: A space $X$ is $T_0$ if and only if every proper subspace of $X$ is $T_0$. Does this statement become true if $X$ contains more than two points? Prove or disprove the corresponding statement with $T_1$ substituted for $T_0$ and with the added assumption that $X$ contains at least three points.

10. Prove or disprove: A space $X$ is $T_1$ if and only if $\{x\}'=\phi$ for each $x\in X$.

<style scoped>
.topology-chapter-figure {
  max-width: 28rem;
  margin: 1.75rem auto;
  text-align: center;
}
.topology-chapter-figure img {
  width: 100%;
  background: white;
}
.topology-chapter-figure figcaption {
  font-family: Georgia, serif;
  font-size: 0.875rem;
  margin-top: 0.4rem;
}
.topology-chapter-figure-pair {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 1.5rem;
  margin: 1.75rem 0;
}
.topology-chapter-figure-pair .topology-chapter-figure {
  margin: 0;
}
@media (max-width: 480px) {
  .topology-chapter-figure-pair {
    grid-template-columns: 1fr;
  }
}
</style>

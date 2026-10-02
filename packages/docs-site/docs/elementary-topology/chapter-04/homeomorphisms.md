---
title: Homeomorphisms — Elementary Topology
description: The available text of section 4.4 of the supplied second-edition scan.
---

# 4.4 Homeomorphisms

::: warning Missing source page 75
Printed page 75 is absent. This section begins with the available Figures 4.1–4.2 and Example 13 on printed page 76. The title is retained from the running header on printed page 77. Definition 4, Example 12, and any unavailable section opening are not reconstructed. References to that missing material are retained as printed.
:::

::: info Transcription note
Source: printed pages 76–79 (PDF pages 77–80). Printed page 79 continues with §4.5 after Exercise 5. Handwritten marks over the codomain in the composition formula on printed page 78 are not treated as source prose; the printed $Z$ is retained.
:::

<span id="printed-page-76"></span>

<!-- Source: PDF page 77, printed page 76. -->

<div class="topology-chapter-figure-pair">

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-4.1.svg" alt="Figure 4.1: Projection between two segments from a common vertex." /><figcaption>Figure 4.1</figcaption></figure>

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-4.2.svg" alt="Figure 4.2: Radial projection from a circle to a triangle." /><figcaption>Figure 4.2</figcaption></figure>

</div>

**Example 13.** Let $R^2$ again be the plane with the Pythagorean metric topology. Then any triangle in $R^2$ is homeomorphic to any circle. We can prove this by positioning the triangle and circle so that the center of the circle lies inside the triangle. We can then “project” the circle onto the triangle as shown in Fig. 4.2. An argument similar to that of Example 12 shows that this projection is a homeomorphism.

**Proposition 13.** Let $f$ be a one-one function from a space $X,\tau$ onto a space $Y,\tau'$. Then the following statements are equivalent:

a) $f$ is a homeomorphism.

b) A subset $U$ of $Y$ is open if and only if $f^{-1}(U)$ is open in $X$.

c) A subset $F$ of $Y$ is closed if and only if $f^{-1}(F)$ is closed in $X$.

d) If $\mathfrak{B}$ is a basis for $\tau$, then $f(\mathfrak{B})=\{f(B)\mid B\in\mathfrak{B}\}$ is a basis for $\tau'$.

_Proof._ Statement (a) implies statement (b): If $f$ is a homeomorphism, then both $f$ and $f^{-1}$ are continuous. Therefore if $U$ is any open subset of $Y$, $f^{-1}(U)$ is open in $X$. Suppose $U$ is a subset of $Y$ such that $f^{-1}(U)$ is open in $X$. Since $f$ is onto, $f(f^{-1}(U))=U$. But $f=(f^{-1})^{-1}$, and since $f^{-1}$ is continuous and $f^{-1}(U)$ is open in $X$,

$$
(f^{-1})^{-1}(f^{-1}(U))=U
$$

is open in $Y$. Therefore $U$ is open in $Y$ if and only if $f^{-1}(U)$ is open in $X$.

Statement (b) implies statement (c): Suppose $F$ is a closed subset of $Y$. Then $Y-F$ is an open subset of $Y$; hence $f^{-1}(Y-F)$ is open in $X$. But $f^{-1}(Y-F)=X-f^{-1}(F)$, and thus $f^{-1}(F)$ is a closed subset of $X$. Suppose that $F$ is a subset of $Y$ such that $f^{-1}(F)$ is a closed subset of $X$. Then

$$
f^{-1}(Y-F)=X-f^{-1}(F)
$$

is an open subset of $X$; hence $Y-F$ is an open subset of $Y$; hence $F$ is closed. Therefore (b) implies (c).

Statement (c) implies statement (a): By Proposition 8, (c) states that $f$ and $f^{-1}$ are continuous; hence $f$ is a homeomorphism.

<span id="printed-page-77"></span>

<!-- Source: PDF page 78, printed page 77. -->

Statement (a) implies statement (d): Suppose $U$ is any open subset of $Y$. Since $f$ is continuous, $f^{-1}(U)$ is an open subset of $X$. Therefore $f^{-1}(U)=\bigcup_I B_i$, where each $B_i\in\mathfrak{B}$ and $I$ is a suitable index set. Now

$$
f(f^{-1}(U))=U=f\left(\bigcup_I B_i\right)=\bigcup_I f(B_i).
$$

But each $B_i$ is open in $X$, and it has been shown that (a), (b), and (c) are equivalent (a implies b implies c implies a); hence, by (b), $f(B_i)$ is an open subset of $Y$. Then $U$ is the union of members of $f(\mathfrak{B})$, and each member of $f(\mathfrak{B})$ is an open subset of $Y$. Therefore $f(\mathfrak{B})$ is a basis for $\tau'$.

Statement (d) implies statement (a): Suppose $U$ is an open subset of $Y$. Then $U=\bigcup_I f(B_i)$, where $B_i\in\mathfrak{B}$ and $I$ is again a suitable index set. It follows that

$$
f^{-1}(U)=f^{-1}\left(\bigcup_I f(B_i)\right)=\bigcup_I f^{-1}(f(B_i))=\bigcup_I B_i,
$$

which is an open subset of $X$. Therefore $f$ is continuous. On the other hand, if $V$ is any open subset of $X$, then $V=\bigcup_J B_j$, where each $B_j\in\mathfrak{B}$. Thus

$$
f(V)=f\left(\bigcup_J B_j\right)=\bigcup_J f(B_j)
$$

is the union of open subsets of $Y$ and is therefore open. But $f=(f^{-1})^{-1}$; hence $(f^{-1})^{-1}(V)$ is open in $Y$ if $V$ is open in $X$. Therefore $f^{-1}$ is continuous, and $f$ is a homeomorphism.

We see from Proposition 13 that homeomorphic spaces are essentially equivalent from a topological point of view. If two spaces are homeomorphic, there is not only a one-one function from one space to the other, but also a natural one-one correspondence between their open sets (Proposition 13b). Put another way, if $X,\tau$ and $Y,\tau'$ are homeomorphic spaces, then by suitably relabeling the points of $Y$, we obtain $X$, and $\tau'$ becomes $\tau$.

We must keep in mind, however, that homeomorphic spaces can appear quite different from other points of view than the topological. We have seen, for example, that a circle and a triangle are homeomorphic. From a geometric point of view, a circle and a triangle are quite different, even though from a topological point of view they are indistinguishable. Recall that Euclidean geometry is primarily concerned with the properties of objects which are preserved under rigid motions, that is, in Euclidean geometry, we are interested in studying properties common to all objects which are congruent. Almost all geometric studies can be classified according to the type of properties they study; in particular, these types of properties are those which are preserved by certain kinds of functions. Topology, considered as a branch of geometry, studies properties preserved by a very special type of function, the homeomorphism.

<span id="printed-page-78"></span>

<!-- Source: PDF page 79, printed page 78. -->

**Proposition 14.** Let $\mathcal{T}$ be the class of all topological spaces. Then the relation $R$ defined on $\mathcal{T}$ by “is homeomorphic to” is an equivalence relation on $\mathcal{T}$.

_Proof._ If $X,\tau$ is any topological space, then $X,\tau$ is homeomorphic to $X,\tau$ by the identity function. Therefore $X,\tau$ is $R$-equivalent to $X,\tau$.

Suppose $X,\tau$ is homeomorphic to $Y,\tau'$ by some homeomorphism $f$. Then $f^{-1}$ is a homeomorphism from $Y,\tau'$ to $X,\tau$. Hence if $X,\tau$ is $R$-equivalent to $Y,\tau'$, then $Y,\tau'$ is $R$-equivalent to $X,\tau$.

Now suppose that $f$ is a homeomorphism from $X,\tau$ to $Y,\tau'$, and that $g$ is a homeomorphism from $Y,\tau'$ to $Z,\tau''$. Since both $f$ and $g$ are one-one and onto, $g\circ f:X\to Z$ is one-one and onto. Since $f$, $g$, $f^{-1}$, and $g^{-1}$ are all continuous, $g\circ f$ and $(g\circ f)^{-1}=f^{-1}\circ g^{-1}$ are also continuous (Proposition 10). Therefore

$$
g\circ f:X,\tau\to Z,\tau''
$$

is a homeomorphism. Hence the relation $R$ is transitive, and, consequently, $R$ is an equivalence relation on $\mathcal{T}$.

From a topological point of view then, any two homeomorphic spaces are equivalent, or interchangeable. The question of determining whether or not two given spaces are homeomorphic is often extremely difficult. As a matter of fact, it is usually very difficult to determine whether there is even a continuous function from one space onto another. In most instances, this problem has not been solved.

**Proposition 15.** Suppose that $X$ is any set and that $\tau$ and $\tau'$ are two topologies for $X$. Then $\tau=\tau'$ if and only if the identity function $i$ on $X$ is a homeomorphism from $X,\tau$ to $X,\tau'$.

The proof is left as an exercise.

## Exercises

1. Prove Proposition 15.

2. Let $R$ be the set of real numbers with the absolute value topology.

   a) Prove that any open interval $(a,b)$ is homeomorphic to the interval $(0,1)$. [*Hint:* Use $f(x)=(x-a)/(b-a)$.]

   b) Prove that the ray $(a,\infty)$ is homeomorphic to $(1,\infty)$.

   c) Prove that $(a,\infty)$ is homeomorphic to $(-\infty,-a)$.

   d) Prove that $R$ is homeomorphic to $(-\pi/2,\pi/2)$. [*Hint:* Use $f(x)=\tan^{-1}x$.]

   e) Prove that $(1,\infty)$ is homeomorphic to $(0,1)$. [*Hint:* Use $g(x)=1/x$.]

   We thus conclude that any two open intervals of the real line are homeomorphic.

   f) Prove that any two closed intervals of the real line are homeomorphic.

<span id="printed-page-79"></span>

<!-- Source: PDF page 80, printed page 79; section 4.4 fragment. -->

3. Homeomorphic spaces have essentially the same topological properties, that is, properties related exclusively to their topologies. Although the reader has yet encountered very few topological properties, he should be able to make an intelligent conjecture about whether the spaces in each of the following pairs are homeomorphic to one another. If the spaces are homeomorphic, try to describe a homeomorphism. If they are not homeomorphic, try to find a topological property which one of the spaces has, but which the other space does not have.

   a) the open interval $(0,1)$ and the closed interval $[0,1]$ considered as subspaces of the real numbers with the absolute value topology

   b) $(0,1)$ and $[0,1]$ considered as subspaces of the real numbers with the discrete topology

   c) a circle $C$ considered as a subspace of the plane $R^2$ with the usual topology and the interval $(0,1)$ from (a)

   d) $\{x\mid x\text{ is a rational number}\}$ and $\{n\mid n\text{ is an integer}\}$, both considered as subspaces of the real numbers with the absolute value topology

4. Let $X$ be the space of functions described in Example 5, Chapter 2, with the topology induced by the metric $D$.

   a) Prove that $X$ cannot be homeomorphic to the space $R$ of real numbers with the absolute value topology. [_Hint:_ Prove that $X$ has greater cardinality than $[0,1]$, which has the same cardinality as $R$. Do this by assuming that there is a one-one correspondence between the elements of $[0,1]$ and the elements of $X$ and then constructing a function which does not correspond to any element of $[0,1]$. More particularly, if $x\leftrightarrow f$, set $g(x)\ne f(x)$, for each $x\in[0,1]$.]

   b) Find an embedding of $R$ as a subspace of $X$.

5. Let $N$ be the set of positive integers. Define a subset $F$ of $N$ to be closed if $F$ contains a finite number of positive integers, or $F=N$ (cf. Exercise 6 of Section 3.1). Prove that any infinite subspace of $N$ is homeomorphic to $N$. Is it possible to find a nontrivial topology on the set $R$ of real numbers such that every uncountable subspace of $R$ is homeomorphic to $R$?

<style>
.topology-chapter-figure { max-width: 28rem; margin: 1.75rem auto; text-align: center; }
.topology-chapter-figure img { width: 100%; background: white; }
.topology-chapter-figure figcaption { font-family: Georgia, serif; font-size: 0.875rem; margin-top: 0.4rem; }
.topology-chapter-figure-pair { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 1.5rem; margin: 1.75rem 0; }
.topology-chapter-figure-pair .topology-chapter-figure { margin: 0; }
@media (max-width: 480px) { .topology-chapter-figure-pair { grid-template-columns: 1fr; } }
</style>

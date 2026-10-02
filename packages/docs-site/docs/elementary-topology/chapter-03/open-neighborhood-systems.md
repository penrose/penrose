---
title: Open Neighborhood Systems — Elementary Topology
description: The available text of section 3.3, printed pages 49 and 51, of Elementary Topology, second edition.
---

# 3.3 Open Neighborhood Systems

::: info Transcription note
Source: printed pages 49 and 51 (PDF pages 54–55). Printed pages 50 and 52 are missing. The source's “snd” in Example 10(iii) is retained as printed. The Gothic neighborhood-system notation is retained as $\mathfrak{N}$.
:::

<span id="printed-page-49"></span>

<!-- source: PDF 54, printed 49 -->

Although we already have four ways to specify a topology on a set, we have not as yet formally introduced one of the most widely used manners of determining a topology. Actually, however, we have already encountered this method, since it is nothing more than the generalization of the $\rho$-neighborhoods of a point in a metric space.

**Definition 5.** Suppose $X,\tau$ to be a topological space, and suppose that for each point $x\in X$ we have a collection $\mathfrak{N}_x$ of open sets having the following properties:

i) $\mathfrak{N}_x\neq\phi$.

ii) $x\in N$ for each $N\in\mathfrak{N}_x$.

iii) If $N_1$ and $N_2$ are in $\mathfrak{N}_x$, then there is $N_3\in\mathfrak{N}_x$ such that $N_3\subset N_1\cap N_2$.

iv) Given $N\in\mathfrak{N}_x$ and any $y\in N$, there is $N'\in\mathfrak{N}_y$ such that $N'\subset N$.

v) A subset $U$ of $X$ is open if and only if for each $x\in U$, there is $N\in\mathfrak{N}_x$ such that $N\subset U$.

Then the collection of families of members of $\tau$ (one for each $x\in X$) is called an _open neighborhood system_ for $\tau$.

**Example 10.** Suppose $X,D$ is a metric space. Set $\mathfrak{N}_x=\{N(x,\rho)\mid\rho>0\}$, for each $x\in X$. We will verify that the collection of $\mathfrak{N}_x$ forms an open neighborhood system for the topology induced on $X$ by $D$.

i) Since $N(x,1)\in\mathfrak{N}_x$ for each $x\in X$, $\mathfrak{N}_x\neq\phi$ for each $x\in X$.

ii) $D(x,x)=0$ for any $x\in X$ implies that $x\in N(x,\rho)$ for any $\rho>0$. Therefore $x\in N$ for any $N\in\mathfrak{N}_x$.

iii) Suppose $N_1$ snd $N_2$ are in $\mathfrak{N}_x$. Then $N_1=N(x,\rho_1)$ and $N_2=N(x,\rho_2)$, where $\rho_1$ and $\rho_2$ are positive numbers. We may suppose $\rho_1\geq\rho_2$. Then

$$
N_1\cap N_2=N(x,\rho_2)\in\mathfrak{N}_x.
$$

iv) This is essentially Proposition 1, Chapter 2.

v) This is the definition of open set in the topology induced by $D$.

The following proposition relates open neighborhood systems and bases for a topology.

**Proposition 6.** Suppose that $X,\tau$ is a topological space. Then if $\mathfrak{N}$ is any open neighborhood system for $\tau$, the collection of subsets of $X$ contained in $\mathfrak{N}$ forms a basis for $\tau$. On the other hand, if $\mathfrak{B}$ is any basis for $\tau$, then, setting $\mathfrak{N}_x=\{B\in\mathfrak{B}\mid x\in B\}$ for each $x\in X$, we obtain an open neighborhood system for $\tau$.

::: warning Missing source page 50
Printed page 50 is absent. The next available page begins within a proof that is subsequently identified as the proof of Proposition 7. The missing material, including that proposition's statement and the beginning of its proof, has not been reconstructed.
:::

<span id="printed-page-51"></span>

<!-- source: PDF 55, printed 51 -->

$N_2\subset V$. Therefore

$$
x\in N_1\cap N_2\subset U\cap V.
$$

By Definition 5(iii), there is $N_3\in\mathfrak{N}_x$ such that

$$
x\in N_3\subset N_1\cap N_2\subset U\cap V;
$$

hence $U\cap V$ is also an open set. The intersection of any two open sets is again an open set.

iii) Suppose $\{U_i\}$, $i\in I$, is a family of open sets, and $x\in\bigcup_I U_i$. Then $x\in U_i$ for some $i$; hence there is $N\in\mathfrak{N}_x$ such that $x\in N\subset U_i\subset\bigcup_I U_i$. The union of any family of open sets is thus again an open set. Therefore $\tau$ is a topology on $X$.

The reader should compare the proof of Proposition 7 with the proof of Proposition 2, Chapter 2. Why might one expect to see many similarities?

**Example 11.** Let $R$ be the set of real numbers. For each $x\in R$, let $\mathfrak{N}_x$ be the set of all half-open intervals having $x$ as a left-hand endpoint; that is,

$$
\mathfrak{N}_x=\{[x,a)\mid a\in R,x<a\}.
$$

The reader should verify at once (Exercise 3) that the collection of $\mathfrak{N}_x$ satisfies (i) through (iv) in Definition 5. In accordance with Proposition 7, then the collection of $\mathfrak{N}_x$ determines a topology on $X$. Exercise 2 shows that this topology is unique.

**Example 12.** Let $R^2$ be the coordinate plane. For each $x\in R^2$, let $\mathfrak{N}_x$ be the set of interiors of all triangles which contain $x$ in their interior. Then the collection of $\mathfrak{N}_x$ forms an open neighborhood system for a topology on $X$.

**Example 13.** The following topology has applications in algebraic geometry. Let $S$ be a ring. For each $s\in S$, define

$$
\mathfrak{N}_s=\{s+A\mid A\text{ is a nonzero ideal of }S\},
$$

that is, $\mathfrak{N}_s$ is the set of cosets of $s$. It can be verified that the collection of $\mathfrak{N}_s$ satisfies (i) through (iv) of Definition 5 and hence forms an open neighborhood system for a topology on $S$ (Exercise 5).

The reader should note that even though the definition of an open neighborhood system seems more cumbersome than other methods of specifying a topology, in actual practice it is often the easiest and most natural way.

::: warning Missing source page 52
Printed page 52 is absent. The next available page has the running section label 3.4 and begins with Definition 6. No intervening text or exercises have been reconstructed.
:::

[Continue to the available text of 3.4 Finer and Coarser Topologies](./finer-and-coarser-topologies)

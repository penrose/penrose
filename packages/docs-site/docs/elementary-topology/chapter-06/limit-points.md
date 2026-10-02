---
title: Limit Points — Elementary Topology
description: The available text of section 6.5 of the supplied second-edition scan.
---

# 6.5 Limit Points

::: info Transcription note
Source: printed pages 127–128 and 130 (PDF pages 123–125). Printed page 129 is absent; the remainder of Proposition 9's proof, its corollary, and Exercise 1 are unavailable. Printed pages 127 and 130 also contain neighboring section fragments, transcribed separately. Proposition 9's printed order-preservation argument is retained.
:::

<span id="printed-page-127"></span>

<!-- Source: PDF page 123, printed page 127; section 6.5 fragment. -->

**Definition 4.** Let $\{s_i\}$, $i\in I$, be any net in a space $X,\tau$. A point $y$ of $X$ is said to be a _limit point_ of $\{s_i\}$, $i\in I$, (not to be confused with _limit_) if $\{s_i\}$, $i\in I$, is cofinally in every neighborhood of $y$. That is, $y$ is a limit point of $\{s_i\}$, $i\in I$, if given any neighborhood $U$ of $y$ and any elements $i$ and $i'$ of $I$, there is $i''\in I$ such that $i\leq i''$, $i'\leq i''$, and $s_{i''}\in U$.

Note that if $s_i\longrightarrow y$, then $y$ is a limit point of $\{s_i\}$, $i\in I$. On the other hand, a net need not converge to a limit point, as the following example demonstrates.

<span id="printed-page-128"></span>

<!-- Source: PDF page 124, printed page 128. -->

**Example 13.** Let $R$ be the space of real numbers with the absolute value topology. Let $\{s_n\}$, $n\in N$, be the sequence in $R$ defined by $s_n=(-1)^n$. If $n$ is odd, then $s_n=-1$, and if $n$ is even, $s_n=1$. Then $\{s_n\}$, $n\in N$, has both $-1$ and 1 as limit points. For if $U$ is any neighborhood of 1 and $m$ and $m'$ are any two elements of $N$, then there is an even integer $m''$ greater than both $m$ and $m'$, and $s_{m''}\in U$. Thus 1 is a limit point of $\{s_n\}$, $n\in N$; similarly, $-1$ is also a limit point. We note that even though the space involved is $T_2$, a net, here a sequence, may have a number of different limit points.

However, $\{s_n\}$, $n\in N$, does not converge to either 1 or $-1$. For suppose $s_n\longrightarrow1$. Since $R$ is $T_2$, we may find neighborhoods $U$ and $V$ of 1 and $-1$, respectively, such that $U\cap V=\phi$. Then $\{s_n\}$, $n\in N$, is residually in $U$, a contradiction to the fact that $\{s_n\}$, $n\in N$, is cofinally constantly $-1$ and $-1\notin U$. Similarly, $s_n\not\longrightarrow-1$.

We note, however, that if we let $N'$ represent the set of positive even integers, $N''$ represent the set of positive odd integers, and $k'$ and $k''$ be the identity mappings from $N'$ into $N$ and $N''$ into $N$, respectively, then $s\circ k'$ and $s\circ k''$ are subsequences of $\{s_n\}$, $n\in N$, which converge to 1 and $-1$, respectively. We might conjecture then that even though a net need not converge to one of its limit points, some subnet of that net might. We prove this conjecture in the next proposition.

**Proposition 9.** Let $X,\tau$ be a topological space and $\{s_i\}$, $i\in I$, be a net in $X$. Then $y$ is a limit point of $\{s_i\}$, $i\in I$, if and only if $\{s_i\}$, $i\in I$, has a subnet which converges to $y$.

_Proof._ Suppose $\{s_i\}$, $i\in I$, has a subnet which converges to $y$. Then there is a directed set $J$ and a function $k$ from $J$ into $I$ as in Definition 2 such that $s\circ k$ is a subnet of $\{s_i\}$, $i\in I$, and $s_{k_j}\longrightarrow y$. Suppose $U$ is any neighborhood of $y$. Then $\{s_{k_j}\}$, $j\in J$, is residually in $U$, that is, there is $j_0\in J$ such that $j_0\leq j$ implies $s_{k_j}\in U$. Suppose $i$ and $i'$ are any two elements of $I$. Then since $I$ is directed, there is $i''\in I$ such that $i\leq i''$ and $i'\leq i''$. But $k(j_0)$ is an element of $I$, and $\{s_{k_j}\}$, $j\in J$, is cofinal in $\{s_i\}$, $i\in I$. We can therefore find $j'\in J$ such that

$$
k(j_0)\leq k(j')\qquad\text{and}\qquad i''\leq k(j').
$$

Since $k$ is order-preserving, $j_0\leq j'$; hence $s_{k_{j'}}\in U$. Since $\leq$ is transitive, $i\leq k(j')$ and $i'\leq k(j')$. In sum then, given $i$ and $i'$ in $I$, we have found $k(j')\in I$ such that

$$
i\leq k(j'),\qquad i'\leq k(j'),\qquad\text{and}\qquad s_{k_{j'}}\in U.
$$

Therefore $\{s_i\}$, $i\in I$, is cofinally in $U$, and hence $y$ is a limit point of $\{s_i\}$, $i\in I$.

::: warning Missing source page 129
Printed page 129 is absent. The remaining proof, the corollary referred to below, and Exercise 1 have not been reconstructed. The supplied exercises resume at number 2 on printed page 130.
:::

<span id="printed-page-130"></span>

<!-- Source: PDF page 125, printed page 130; section 6.5 fragment. -->

## Exercises

2. Prove the corollary to Proposition 9.

3. Let $\{s_i\}$, $i\in I$, be a net in a space $X,\tau$ and let $A$ be the set of limit points of $\{s_i\}$, $i\in I$. Prove that $\{s_i\mid i\in I\}\cup A$ is a closed subset of $X$.

4. Find all the limit points of each of the following sequences.

   a) $s_n=1/n$ in the set of real numbers with the order topology

   b) $s_n=(-1)^n$ in the set of real numbers with the trivial topology

   c) $s_n=(-1)^n$ in the set of real numbers with the discrete topology

   d) $s_n=n$ in the set of real numbers with the absolute value topology

5. Let $X,\tau$ be a space with the property that any net in $X$ which has a limit point converges to that limit point. Discuss the various possibilities for the topology on $X$.

6. Let $R$ be the set of real numbers with the absolute value topology. A sequence $\{s_n\}$, $n\in N$, is said to be _bounded_ if there are real numbers $m$ and $M$ such that $m\leq s_n\leq M$ for all $n\in N$. Prove that any bounded sequence in $R$ has a limit point. Prove that every convergent sequence in $R$ is bounded, but that not every bounded sequence is convergent.

7. Prove or disprove: There exists a sequence of real numbers which has every real number as a limit point. Prove or disprove: There exists a net of real numbers which has $R$ as its set of limit points.

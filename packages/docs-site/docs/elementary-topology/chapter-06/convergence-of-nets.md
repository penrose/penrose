---
title: Convergence of Nets — Elementary Topology
description: The available text of section 6.4 of the supplied second-edition scan.
---

# 6.4 Convergence of Nets

::: info Transcription note
Source: printed pages 122–127 (PDF pages 118–123). Printed page 122 begins with the final §6.3 exercises, and page 127 continues into §6.5; these fragments are transcribed separately. Example 11's endpoint sums, partial-order direction, and terminology are retained as printed, as is the reference to Chapter 3 before Proposition 8.
:::

<span id="printed-page-122"></span>

<!-- Source: PDF page 118, printed page 122; section 6.4 fragment. -->

Thus far we have primarily studied the idea of a net in an arbitrary set without regard to any topological structure that might be on the set. But just as, in Chapter 2, we were interested in the convergence of sequences in metric spaces, so now we are interested in finding a notion of convergence for nets in general topological spaces which generalizes the notion of convergence of sequences. We note that the criterion for convergence of a sequence in a metric space can be restated: A sequence $\{s_n\}$, $n\in N$, converges to $y$ if and only if given any neighborhood $U$ of $y$, then $\{s_n\}$, $n\in N$, is residually in $U$. We therefore make the following definition.

**Definition 3.** Let $X,\tau$ be any topological space, and suppose $\{s_i\}$, $i\in I$, is a net in $X$. Then $\{s_i\}$, $i\in I$, is said to _converge_ to a point $y$ of $X$ if $\{s_i\}$, $i\in I$, is residually in every neighborhood of $y$. If $\{s_i\}$, $i\in I$, converges to $y$, we write $s_i\longrightarrow y$. In other words, $s_i\longrightarrow y$ if given any neighborhood $U$ of $y$, there is $i_0\in I$ such that if $i_0\leq i$, $s_i\in U$. The point $y$ is called the _limit_ of $\{s_i\}$, $i\in I$.

<span id="printed-page-123"></span>

<!-- Source: PDF page 119, printed page 123. -->

**Example 9.** If $X$ is any topological space, $x\in X$, and $T(X,x)$ is the directed set of neighborhoods of $x$, then for any selection function $s:T(X,x)\longrightarrow X$, the net $\{s_U\}$, $U\in T(X,x)$, converges to $x$. For let $V$ be any neighborhood of $x$. Then since $s_U\in U$ for each $U\in T(X,x)$ and $V\leq U$ means $U\subset V$, if $V\leq U$, $s_U\in V$.

**Example 10.** Again consider Example 1. We have seen that no sequence of elements of $A$ converges to $g$. We now show that there is a net $\{s_i\}$, $i\in I$, such that $s_i\longrightarrow g$ and $s_i\in A$ for each $i\in I$. Let $T(X,g)$ be as defined previously. We wish to prove the existence of a selection function $s$ from $T(X,g)$ into $X$ such that

$$
s(W)\in W\cap A\qquad\text{for each }W\in T(X,g).
$$

This will be accomplished if we show that for any finite subset $F$ of $R$, the set of real numbers, and for any positive number $p$,

$$
U(g,F,p)\cap A\ne\phi
$$

(for the family of $U(g,F,p)$ is $\mathfrak{N}_g$, and hence any neighborhood of $g$ contains a neighborhood of this form). This was already done, however, in proving that $g\in\operatorname{Cl}A$. Therefore the net $\{s_W\}$, $W\in T(X,g)$, where $s_W\in W\cap A$, converges to $g$. We thus see that even though no sequence of elements of $A$ converges to $g\in\operatorname{Cl}A$, there is a net of elements of $A$ which converges to $g$. We seem therefore to be well on our way to generalizing Proposition 1.

The reader may feel that at least sequences are sufficient for doing whatever has to be done pertaining to the ordinary space $R$ of real numbers (that is, $R$ with the absolute value metric), and that nets are only of use in dealing with “screwball” topological spaces such as that given in Example 1. This is not at all the case, but to help convince the reader that nets are of great value even in real analysis, we give the following example.<sup><a href="#riemann-note">\*</a></sup>

<figure>
  <img src="/elementary-topology/figures/figure-6.1.svg" alt="The bracketed interval [a,b] with partition points x₀=a through xₙ=b." />
  <figcaption>Figure 6.1</figcaption>
</figure>

**Example 11.** Let $[a,b]$ be a closed interval in $R$, the space of real numbers with the absolute value metric. A _partition_ $P$ of $[a,b]$ is a finite collection of points

$$
x_0=a<x_1<x_2<\cdots<x_{n-1}<x_n=b
$$

<aside id="riemann-note" class="source-footnote">
* Actually, Riemann integrals can be adequately handled entirely in terms of sequences, but the use of nets is more elegant. Many important notions, however, depending on the concept of convergence cannot be handled adequately without appeal to something more general than sequences.
</aside>

<span id="printed-page-124"></span>

<!-- Source: PDF page 120, printed page 124. -->

(Fig. 6.1). The _mesh_ of $P$, denoted by $m(P)$, is defined to be

$$
\max(|x_i-x_{i+1}|,\ i=0,\ldots,n-1,
$$

where $n$ is the number of points in the partition);

thus the maximum mesh of any partition of $[a,b]$ would be $b-a$. Suppose $P_1$ and $P_2$ are two partitions of $[a,b]$. Then $P_1$ is said to be _finer_ than $P_2$, if $P_2\subset P_1$; if $P_2\subset P_1$, then $m(P_1)\leq m(P_2)$, since $P_1$ has at least as many points as $P_2$. Set $P_1\leq P_2$ if $P_1$ is finer than $P_2$. If we let $\mathfrak{P}$ denote the family of all partitions of $[a,b]$, the reader can show that $\leq$ makes $\mathfrak{P}$ into a directed set.

Let $f$ be any function from $[a,b]$ into $R$. For any partition $P$, say $P$ consists of

$$
x_0=a<x_1<\cdots<x_{n-1}<x_n=b,
$$

define

$$
s(f,P)=\sum_{i=0}^{n-1}f(x_i)(x_{i+1}-x_i)
$$

and

$$
S(f,P)=\sum_{i=0}^{n-1}f(x_{i+1})(x_{i+1}-x_i).
$$

Then $S(f,-)$ and $s(f,-)$ define nets in $R$, that is,

$$
\{S(f,P)\},\quad P\in\mathfrak{P},\qquad\text{and}\qquad\{s(f,P)\},\quad P\in\mathfrak{P}.
$$

If the former net converges, its limit is called the _upper Riemann integral_ of $f$ over $[a,b]$; if the latter net converges, its limit is called the _lower Riemann integral_ of $f$ over $[a,b]$. If both nets converge to a common limit, this limit is called the _Riemann integral_ of $f$ over $[a,b]$, commonly denoted by $\int_a^b f(x)\,dx$.

Admittedly, this example has been somewhat sketchy. The interested reader, however, can find this topic developed at length in most books in real analysis. It should indicate, though, that nets do furnish a powerful and effective tool in defining and studying a concept known to the reader from elementary calculus.

We now prove some of the more fundamental properties of the convergence of nets.

**Proposition 4.** Suppose $\{s_i\}$, $i\in I$, is a net in $X$ such that $\{s_i\}$, $i\in I$, is residually constant; that is, there is $y\in X$ and $i_0\in I$ such that if $i_0\leq i$, $s_i=y$. Then $s_i\longrightarrow y$.

_Proof._ Since any neighborhood of $y$ contains $y$, $\{s_i\}$, $i\in I$, is residually in any neighborhood of $y$; hence $\{s_i\}$, $i\in I$, converges to $y$.

<span id="printed-page-125"></span>

<!-- Source: PDF page 121, printed page 125. -->

**Proposition 5.** If $s_i\longrightarrow y$, then every subnet of $\{s_i\}$, $i\in I$, also converges to $y$.

_Proof._ Since $s_i\longrightarrow y$, $\{s_i\}$, $i\in I$, has the property of being in every neighborhood of $y$ residually. Then, by Proposition 3, every subnet of $\{s_i\}$, $i\in I$, also has the property of being in every neighborhood of $y$ residually. Therefore every subnet of $\{s_i\}$, $i\in I$, converges to $y$.

**Proposition 6.** If every subnet of a net $\{s_i\}$, $i\in I$, has a subsubnet which converges to $y$, then $s_i\longrightarrow y$. This is to say that if $\{s_i\}$, $i\in I$, does not converge to $y$, then there is a subnet of $\{s_i\}$, $i\in I$, no subnet of which converges to $y$.

_Proof._ Since $\{s_i\}$, $i\in I$, does not converge to $y$, there is a neighborhood $U$ of $y$ such that there does not exist any $i_0\in I$ such that $i_0\leq i$ implies $s_i\in U$. Let $J=\{j\in I\mid s_j\notin U\}$, and let $k$ be the identity mapping from $J$ into $I$. Then $s\circ k$ is a subnet of $\{s_i\}$, $i\in I$ (Exercise 1). But each $s_{k_j}$ is not an element of $U$. Therefore there cannot be a subnet of $\{s_{k_j}\}$, $j\in J$, which converges to $y$.

We now prove the long-awaited generalization of Proposition 1.

**Proposition 7.** If $A$ is any subset of a topological space $X,\tau$, then

$$
x\in\operatorname{Cl}A
$$

if and only if there is a net $\{s_i\}$, $i\in I$, such that

$$
s_i\longrightarrow x\quad\text{and}\quad s_i\in A\qquad\text{for each }i\in I.
$$

_Proof._ Suppose first there is a net $\{s_i\}$, $i\in I$, such that

$$
s_i\longrightarrow x\quad\text{and}\quad s_i\in A\qquad\text{for each }i\in I.
$$

Then each neighborhood of $x$ contains at least one point of $A$. Therefore $x\in\operatorname{Cl}A$.

Suppose $x\in\operatorname{Cl}A$. Let $T(X,x)$ be the directed set of neighborhoods of $x$, and let $s$ be a selection function from $T(X,x)$ into $X$ such that

$$
s(W)\in W\cap A\qquad\text{for each }W\in T(X,x).
$$

We know that such a selection function exists because $x\in\operatorname{Cl}A$; hence every neighborhood of $x$ contains some point of $A$. Then the net $\{s_W\}$, $W\in T(X,x)$, converges to $x$ (Example 9).

Proposition 7 strengthens our opinion that we have not only generalized sequences properly, but have also generalized the notion of convergence properly.

<span id="printed-page-126"></span>

<!-- Source: PDF page 122, printed page 126. -->

It was shown that any sequence which converges in a metric space converges to a unique limit (Proposition 6, Chapter 3). We might then wonder, In what types of spaces do convergent nets have unique limits? The next proposition answers this question.

**Proposition 8.** A space $X,\tau$ is $T_2$ if and only if given any convergent net $\{s_i\}$, $i\in I$, in $X$, the limit of $\{s_i\}$, $i\in I$, is unique.

_Proof._ Suppose $X,\tau$ is $T_2$, but that there is some net $\{s_i\}$, $i\in I$, in $X$ such that $\{s_i\}$, $i\in I$, converges to distinct points $x$ and $y$. Since $X$ is $T_2$, there are neighborhoods $U$ and $V$ of $x$ and $y$, respectively, such that $U\cap V=\phi$. Since $s_i\longrightarrow x$ and $s_i\longrightarrow y$, $\{s_i\}$, $i\in I$, is residually in both $U$ and $V$. Therefore there are $i_0$ and $i_0'$ such that $i_0\leq i$ implies $s_i\in U$ and $i_0'\leq i$ implies $s_i\in V$. Since $I$ is a directed set, there is $j\in I$ such that $i_0\leq j$ and $i_0'\leq j$. Therefore $s_j\in U\cap V$, a contradiction.

Suppose $X,\tau$ is not $T_2$. Then there are distinct points $x$ and $y$ of $X$ such that every neighborhood of $x$ meets every neighborhood of $y$. Let $T(X,x)$ and $T(X,y)$ be the directed sets of neighborhoods of $x$ and $y$. Then $T(X,x)\times T(X,y)$ is a directed set [directed by defining $(U,V)\leq(U',V')$ if $U'\subset U$ and $V'\subset V$ as in Section 6.2, Exercise 3]. Since $U\cap V\ne\phi$ for each $(U,V)\in T(X,x)\times T(X,y)$, there is a selection function

$$
s:T(X,x)\times T(X,y)\longrightarrow X
$$

such that $s(U,V)\in U\cap V$. Then

$$
\{s_{(U,V)}\},\ (U,V)\in T(X,x)\times T(X,y),
$$

is a net in $X$ which converges to both $x$ and $y$ (Fig. 6.2).

<figure>
  <img src="/elementary-topology/figures/figure-6.2.svg" alt="Overlapping neighborhoods U and V of x and y, with selected value s(U,V) in their intersection." />
  <figcaption>Figure 6.2</figcaption>
</figure>

**Example 12.** Lest it seem peculiar to the reader that a net should be able to converge to several points, let him remember that if $X$ is a set with the trivial topology, then any sequence $\{s_n\}$, $n\in N$, in $X$ converges to every point of $X$. For if $x\in X$, then the only neighborhood of $x$ is $X$, and every sequence in $X$ is residually in $X$. Admittedly, however, the nicest spaces are those where convergent nets have unique limits. This is why many topologists restrict their attention only to spaces which are at least $T_2$.

<span id="printed-page-127"></span>

<!-- Source: PDF page 123, printed page 127; section 6.4 fragment. -->

## Exercises

1. In Proposition 6, prove that $s\circ k$ is a subnet of $\{s_i\}$, $i\in I$.

2. Suppose $X,D$ is a metric space and $\{s_i\}$, $i\in I$, is a net in $X$.

   a) Suppose $s_i\longrightarrow x$. Prove that a subsequence of $\{s_i\}$, $i\in I$, converges to $x$.

   b) Prove that if every subsequence of $\{s_i\}$ converges to $x$, then $s_i\longrightarrow x$.

   c) Prove (a) and (b) when it is merely assumed that $X,\tau$ is a first countable space (Section 6.1, Exercise 2).

3. Let $X$ be a set. Suppose a “rule” of convergence is given which satisfies

   i) $s_i\longrightarrow x$ implies every subnet of $\{s_i\}$, $i\in I$, converges to $x$, and

   ii) if a net $\{s_i\}$, $i\in I$, is residually constantly equal to $y$, then $s_i\longrightarrow y$.

   Define a subset $A$ of $X$ to be closed if and only if for each net $\{s_i\}$, $i\in I$, such that $s_i\in A$ for each $i\in I$ and $s_i\longrightarrow y$, $y\in A$.

   a) Show that the set of closed subsets of $X$ defines a topology $\tau$ on $X$.

   b) Show that each net which converges according to the original rule of convergence also converges with respect to $\tau$.

   c) Show that some net which did not converge with respect to the original rule might still converge with respect to $\tau$.

4. Prove that the only nets which converge in a space with the discrete topology are nets which are residually constant.

5. Let $N$ be the set of positive integers. Discuss the convergence of nets in $N$ when $N$ is given each of the following topologies.

   a) $\tau=\{N,\phi,\{0\}\}$

   b) $\tau=\{U\subset N\mid U\text{ contains all but finitely many elements of }N\}$

   c) the subspace topology from $R$ with the absolute value topology

6. Let $N$, the set of positive integers, have the topology in Problem 5(b). Show that every net in $N$ has a convergent subnet.

7. Suppose $X$ is any set and $\tau$ and $\tau'$ are two possible topologies for $X$. Prove that $\tau'\subset\tau$ if and only if every net in $X$ which converges with respect to $\tau$ also converges with respect to $\tau'$.

8. Find a criterion in terms of nets for a space to be $T_1$. Do likewise for $T_0$.

<style scoped>
.source-footnote {
  font-size: 0.875rem;
  border-top: 1px solid var(--vp-c-divider);
  padding-top: 0.6rem;
  margin: 1.25rem 0;
}
</style>

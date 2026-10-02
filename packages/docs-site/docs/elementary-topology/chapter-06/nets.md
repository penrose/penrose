---
title: Nets — Elementary Topology
description: The available text of section 6.2 of the supplied second-edition scan.
---

# 6.2 Nets

::: info Transcription note
Source: printed pages 116, 118, and 119 (PDF pages 113–115). Printed page 117 is missing; the completion of Definition 1 and the opening of the net examples are unavailable. Printed page 119 continues into §6.3, transcribed separately.
:::

<span id="printed-page-116"></span>

<!-- Source: PDF page 113, printed page 116; section 6.2 fragment. -->

Note that the positive integers form a partially ordered set (in this case, totally ordered) such that if $n$ and $n'$ are any integers, there is an integer $m$ with $n<m$ and $n'<m$. Since all partial orderings share the properties of “less than or equal to,” denoted by $\leq$, we will use $\leq$ to denote any partial ordering. The set of positive integers do have other properties which are not shared by every partially ordered set; for example, the positive integers are totally ordered, well-ordered, and countable. But in any generalization, experience and the problem to be solved indicates which properties must be generalized and which are incidental to the question at hand. The experience and labor of many mathematicians over many years leads us to the following definition.

**Definition 1.** Let $I$ be any partially ordered set. (Recall that $\leq$ is used to designate any partial ordering.) $I$ is said to be a _directed set_ (more accurately, an _upward directed set_) if given any $i$ and any $j$ in $I$,

::: warning Missing source page 117
Printed page 117 is absent. Definition 1 ends here in the available scan. Printed page 118 resumes within an example; neither the missing definition completion nor the intervening examples are reconstructed. Later references to Example 3 and the terms “residually” and “cofinally” are retained as supplied.
:::

<span id="printed-page-118"></span>

<!-- Source: PDF page 114, printed page 118. The opening is a source fragment. -->

$Y\leq W$ ($W\subset Y$). If $X$ is finite, then $P(X)$ is finite as well, and hence it is quite possible to have a finite directed set.

If $x\in X$, set

$$
P(X,x)=\{W\in P(X)\mid x\in W\}.
$$

Then $P(X,x)$ is also a directed set (directed by $\leq$ just as $P(X)$ is). One possible function $s$ from $P(X,x)$ into $X$ would be a selection function where, if $W\in P(X,x)$, then $s(W)\in W$. Such a selection function would then give a net in $X$, $\{s_W\}$, $W\in P(X,x)$.

If $X,\tau$ is a topological space, then we might set

$$
T(X,x)=\{U\mid U\text{ is a neighborhood of }x\}.
$$

The set $T(X,x)$ is also a directed set [in fact, a directed subset of $P(X,x)$]. Using a selection function $s:T(X,x)\longrightarrow X$, where $s(U)\in U$ for each $U\in T(X,x)$, we get a net $\{s_U\}$, $U\in T(X,x)$, in $X$. We might suspect that this net converges to $x$, since it has the property of being in every neighborhood of $x$ residually (Exercise 2).

**Example 4.** Let $\{s_n\}$, $n\in N$, be the sequence in the set $R$ of real numbers defined by $s_n=(-1)^n$. If $R$ is given any topology whatsoever, then $\{s_n\}$, $n\in N$, has the property of being in every neighborhood of 1 cofinally. This sequence is also in every neighborhood of $-1$ cofinally, but it is residually in every neighborhood of both 1 and $-1$ if and only if every neighborhood of 1 is a neighborhood of $-1$ and every neighborhood of $-1$ is a neighborhood of 1.

## Exercises

1. Which of the following sets with the orderings as given are directed sets?

   a) the set of positive integers partially ordered by “divides”

   b) the interval $[0,1]$ ordered by $\leq$

   c) the interval $(0,1)$ ordered by $\leq$

   d) the set $\{1,2,3,4\}$, where the order is defined by the relation

   $$
   R=\{(1,1),(2,2),(3,3),(4,4),(2,3)\}.
   $$

2. Prove the assertion in Example 3 that $\{s_U\}$ has the property of being in any neighborhood of $x$ residually. Using this example, make a tentative definition of what we mean by saying that a net converges to an element $y$ of a space $X,\tau$.

3. Suppose $I$ and $I'$ are sets which are directed by $\leq$ and $\leq'$, respectively. Suppose $(i,i')$ and $(j,j')$ are elements of $I\times I'$. Define

   $$
   (i,i')\leq(j,j')
   $$

   if $i\leq j$ and $i'\leq' j'$. Prove that $I\times I'$ with the relation $\leq$ as defined is a <span id="printed-page-119"></span><!-- Source: PDF page 115, printed page 119; section 6.2 fragment. --> directed set. You must prove that $I\times I'$ is both partially ordered and directed.

4. Let $\{s_n\}$, $n\in N$, be a sequence in the set of integers with the property that if $m\leq n$, $s_m\leq s_n$. Which of the following properties must such a sequence have residually? cofinally? neither cofinally nor residually?

   a) The property of being odd; b) The property of being positive;

   c) The property that $s_n\leq n$; d) The property that $s_n$ is prime.

   Suppose $\{s_n\}$, $n\in N$, is the sequence defined by $s_n=4n+1$. Which of the properties (a) through (d) does this sequence have residually? cofinally?

5. Prove that a net $\{s_i\}$, $i\in I$, has a property $P$ cofinally if and only if for any $i\in I$, there is an element $k$ of $I$ such that $i\leq k$ and $s_k$ has property $P$.

6. The following define functions from $[0,1]$ into $R^2$ and hence define nets in $R^2$. If $R^2$ has its usual topology, indicate any points to which you feel the nets should converge. Explain informally the reasons for your answers in each case.

   a) $f(x)=(x,2)$

   b) $f(x)=(1/(x+1),x^2)$

   c) $f(x)=(\cos x,\sin x)$

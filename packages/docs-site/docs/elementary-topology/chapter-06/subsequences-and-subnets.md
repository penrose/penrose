---
title: Subsequences and Subnets — Elementary Topology
description: The available text of section 6.3 of the supplied second-edition scan.
---

# 6.3 Subsequences and Subnets

::: info Transcription note
Source: printed pages 119–122 (PDF pages 115–118). Printed page 119 begins with the final exercises of §6.2, and printed page 122 continues into §6.4; these fragments are transcribed separately. Weak partial orders are retained as printed.
:::

<span id="printed-page-119"></span>

<!-- Source: PDF page 115, printed page 119; section 6.3 fragment. -->

Fundamental in any study of either sequences or nets is the concept of a _subsequence_, or its generalization, a _subnet_.

**Definition 2.** Suppose $\{s_i\}$, $i\in I$, is a net in a set $X$. Let $J$ be a directed set and $k$ a function from $J$ to $I$ such that

i) if $j\leq j'$, then $k(j)\leq k(j')$;

ii) if $i,i'\in I$, then there is $j\in J$ such that $i\leq k(j)$ and $i'\leq k(j)$.

That is, $k$ is order-preserving, and considered as a net in $I$, $k$ is cofinal in $I$. Then the composition $s\circ k$ from $J$ into $X$ is said to be a _subnet_ of the net $\{s_i\}$, $i\in I$. The subnet $s\circ k$ is usually written as $\{s_{k_j}\}$, $j\in J$.

Note that each $s_{k_j}$ is also an $s_i$ for some $i$ (specifically, for $i=k_j$), and that the $s_{k_j}$ have the property of being cofinal (but not necessarily residual) in the set of $s_i$.

**Example 5.** Let $\{s_n\}$, $n\in N$, be any sequence. Let $2N$ be the set of positive even integers, and let $k$ be the identity mapping from $2N$ into $N$. We first verify that $k$ has properties (i) and (ii) of Definition 2.

i) If $m$ and $n$ are positive even integers and $m\leq n$, then $k(m)=m$ and $k(n)=n$.

ii) Given any integer $n$, there is a positive even integer greater than $n$; therefore there is $m\in2N$ such that

$$
n\leq k(m)=m.
$$

<span id="printed-page-120"></span>

<!-- Source: PDF page 116, printed page 120. -->

The function $s\circ k$ is therefore a subnet of $\{s_n\}$, $n\in N$. In the case where a subnet is a sequence, it is customary to call such a subnet a _subsequence_; thus $s\circ k$ is a subsequence of $\{s_n\}$, $n\in N$. Explicitly, $\{s_{k_m}\}$, $m\in2N$, is the same as $\{s_{2n}\}$, $n\in N$. A subsequence of a sequence is a sequence in its own right, and a subnet of a net is itself a net.

**Example 6.** Let $X,\tau$ be any topological space, and let $x\in X$. Suppose $P(X,x)$ is as defined in Example 3. Let $s$ be any selection function from $P(X,x)$ into $X$; that is, $s(W)\in W$ for each $W\in P(X,x)$. Then $\{s_W\}$, $W\in P(X,x)$, is a net in $X$. Let $T(X,x)$ be (as in Example 3 also) the set of neighborhoods of $x$. Then

$$
T(X,x)\subset P(X,x).
$$

Let $k$ be the identity mapping from $T(X,x)$ into $P(X,x)$. Then $s\circ k$ does _not_ define a subnet of $\{s_W\}$, $W\in P(X,x)$, unless $\{x\}$ is itself a neighborhood of $x$. For if $\{x\}$ is not open, then $\{x\}\notin T(X,x)$; hence there is no $U\in T(X,x)$ such that $\{x\}\leq k(U)$ as is required by (ii) of Definition 2.

**Example 7.** Let $\{s_n\}$, $n\in N$, be any sequence. Let $k$ be a function from $N$ into $N$ defined by

$$
k(n)=n\text{ for }n<10,\qquad k(n)=10\text{ for all }n\geq10.
$$

Then $s\circ k$ satisfies (i) of Definition 2, but not (ii). On the other hand, if we define $k':N\longrightarrow N$ by

$$
k'(n)=n\text{ if }n\text{ is even},\qquad k'(n)=2\text{ if }n\text{ is odd},
$$

then $s\circ k'$ satisfies (ii), but not (i), of Definition 2.

**Example 8.** Let $\{s_i\}$, $i\in I$, be a net where $I$ is the set of real numbers greater than or equal to 1. Let $k$ be the function from the directed set $I\times I$ (directed as in Section 6.2, Exercise 3) defined by $k(i,i')=ii'$. We verify that $k$ satisfies (i) and (ii) of Definition 2.

i) If $(i_1,i_2)\leq(i_3,i_4)$, then $i_1\leq i_3$ and $i_2\leq i_4$. Since all of the numbers concerned are greater than or equal to 1,

$$
k(i_1,i_2)=i_1i_2\leq k(i_3,i_4)=i_3i_4.
$$

ii) If $i$ and $i'$ are any real numbers greater than or equal to 1, $i'\leq i$, then $i\leq i^2$ and $i'\leq i^2$; hence $i\leq k(i,i)$ and $i'\leq k(i,i)$. Therefore $s\circ k$ is a subnet of $\{s_i\}$, $i\in I$. Note that here the set which “indexes” the subnet is actually “richer” than the original index set.

We now prove some of the fundamental properties of subnets.

<span id="printed-page-121"></span>

<!-- Source: PDF page 117, printed page 121. -->

**Proposition 2.** Suppose a net $\{s_i\}$, $i\in I$, has a property $P$ cofinally. Then there is a subnet $\{s_{k_j}\}$, $j\in J$, of $\{s_i\}$, $i\in I$, which has the property $P$ residually.

_Proof._ Let $J$ be the set of all $j\in I$ such that $s_j$ has property $P$, and let $k$ be the identity map from $J$ into $I$. If $J$ has the order induced from $I$, then $J$ is at least a partially ordered set. Since the property $P$ is cofinal, given any $j,j'\in J\subset I$, there is $j''\in I$ such that $j\leq j''$, $j'\leq j''$, and $s_{j''}$ has property $P$. Therefore

$$
j''\in J,\qquad j\leq j'',\qquad\text{and}\qquad j'\leq j'';
$$

hence $J$ is a directed set. Since $k:J\longrightarrow I$ is the identity mapping, $k$ is certainly order-preserving. Since $P$ is cofinal, $k$ also satisfies (ii) of Definition 2. For if $i$ and $i'$ are any elements of $I$, there is $j\in I$ such that $i\leq j$, $i'\leq j$, and $s_j$ has $P$, and hence

$$
j\in J,\qquad i\leq k(j),\qquad\text{and}\qquad i'\leq k(j).
$$

Therefore $s\circ k$ is a subnet of $\{s_i\}$, $i\in I$. By definition of $J$, each $s_{k_j}$ has the property $P$; thus $\{s_{k_j}\}$, $j\in J$, has the property $P$ residually.

**Proposition 3.** Suppose a net $\{s_i\}$, $i\in I$, has a property $P$ residually. Then every subnet of $\{s_i\}$, $i\in I$, also has the property $P$ residually.

_Proof._ Since $\{s_i\}$, $i\in I$, has $P$ residually, there is $i_0\in I$ such that if $i_0\leq i$, then $s_i$ has $P$. Suppose $\{s_{k_j}\}$, $j\in J$, is a subnet of $\{s_i\}$, $i\in I$. Applying (i) and (ii) of Definition 2, we can find $j_0\in J$ such that if $j_0\leq j$, then $i_0\leq k(j)$. Hence if $j_0\leq j$, $s_{k_j}$ has the property $P$. The net $\{s_{k_j}\}$, $j\in J$, therefore has the property $P$ residually.

**Corollary.** A net $\{s_i\}$, $i\in I$, has a property $P$ residually if and only if every subnet of $\{s_i\}$, $i\in I$, has the property $P$ residually.

_Proof._ Each net is a subnet of itself (Exercise 1); hence if each subnet of $\{s_i\}$, $i\in I$, has $P$ residually, then $\{s_i\}$, $i\in I$, does also. The converse is Proposition 3.

## Exercises

1. Prove that every net is a subnet of itself. [Hint: Use $J=I$ and let $k$ be the identity mapping.]

2. Prove the converse of Proposition 2. That is, if a subnet $\{s_{k_j}\}$, $j\in J$, of the net $\{s_i\}$, $i\in I$, has a property residually, then $\{s_i\}$, $i\in I$, has the property cofinally.

<span id="printed-page-122"></span>

<!-- Source: PDF page 118, printed page 122; section 6.3 fragment. -->

3. Let $I$ and $J$ be directed sets and $\{s_i\}$ and $\{t_j\}$ be nets indexed by $I$ and $J$, respectively, in some set $X$. Let $I\times J$ be directed as in Section 6.2, Exercise 3. Define a function $s\times t$ from $I\times J$ into $X\times X$ by $(s\times t)(i,j)=(s_i,t_j)$.

   a) Prove that $s\times t$ defines a net in $X\times X$.

   b) Prove that if $\{s_i\}$, $i\in I$, and $\{t_j\}$, $j\in J$, both have a property $P$ residually or cofinally, then $\{(s_i,t_j)\}$, $(i,j)\in I\times J$, has the property $P$ in the same way that both nets have it.

   c) Suppose $M$ and $M'$ are directed sets and $k$ and $k'$ are functions from $M$ and $M'$ into $I$ and $J$, respectively, such that $s\circ k$ and $t\circ k'$ are subnets of $\{s_i\}$, $i\in I$ and $\{t_j\}$, $j\in J$. Define the obvious function

   $$
   k\times k':M\times M'\longrightarrow I\times J.
   $$

   Prove that $(s\times t)\circ(k\times k')$ is a subnet of $\{(s_i,t_j)\}$, $(i,j)\in I\times J$.

4. Find an example of a net which has a property $P$ cofinally, but such that no _subsequence_ of the net has the property residually. This in turn will be accomplished if we find a net no subnet of which is a subsequence. To find such a net, consider Example 1 of this chapter. Let $g\in X$ be the function which is identically 0, and let $T(X,g)$ be the set of all neighborhoods of $g$. Let $s$ be a selection function from $T(X,g)$ into $X$. Prove that the net $\{s_V\}$, $V\in T(X,g)$, has the property of being in every neighborhood of $g$ residually, but that no subsequence of this net has the property; in fact, prove that there are no subsequences of $\{s_V\}$, $V\in T(X,g)$, at all.

5. Find a net in the set $N$ of positive integers which has the property cofinally of being equal to every positive integer. Describe explicitly the subnet of this net which is residually equal to 3.

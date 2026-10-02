---
title: Continuity — Elementary Topology
description: Section 4.3 of the supplied second-edition scan.
---

# 4.3 Continuity

::: info Transcription note
Source: printed pages 70–74 (PDF pages 72–76). The following printed page 75 is missing; no later exercises or missing section opening are reconstructed.
:::

<span id="printed-page-70"></span>

<!-- Source: PDF page 72, printed page 70. -->

We now come to one of the central notions in all of topology: continuity. We have already encountered continuity in connection with metric spaces. Then a continuous function was a “nearness-preserving” function. Neighborhoods of a point in a general topological space are in a sense measures of nearness, just as the term “neighborhood” implies. As we have seen, however, many topological spaces are not metric spaces, nor can they be made into metric spaces by defining an appropriate metric. We therefore need a definition of continuity which will reduce to the metric definition of continuity when we are dealing with a metric space, generalize appropriately the idea of a “nearness-preserving” function, and not depend on metrics (or anything else which is not common to all topological spaces) for its definition. In Chapter 2 we found at least one criterion for the continuity of a function from one metric space to another which does not include any mention of the metrics in its statement (Proposition 8, Chapter 2). We will therefore use this proposition for a generalized definition of continuity.

**Definition 3.** Let $X,\tau$ and $Y,\tau'$ be topological spaces. Then a function $f$ from $X$ to $Y$ is said to be _continuous_ if given any open subset $U$ of $Y$, then $f^{-1}(U)$ is an open subset of $X$.

**Example 7.** If $X$ is any space with the discrete topology and $Y$ is any topological space, then any function $f$ from $X$ into $Y$ is continuous. For if $U$ is any subset (open or not) of $Y$, then $f^{-1}(U)$ is an open subset of $X$, since every subset of $X$ is open.

**Example 8.** If $X$ is any space with the trivial topology and $f$ is any function from $X$ onto a space $Y$, then $f$ is continuous if and only if $Y$ has the trivial topology. For if $Y$ has the trivial topology, then $Y$ and $\phi$ are the only open subsets of $Y$; hence $f^{-1}(U)$ is open in $X$ (being either $\phi$ for $U=\phi$, or $X$ for $U=Y$) for any open subset $U$ of $Y$. On the other hand, if $Y$ does not have the trivial topology, then there is an open subset $U$ of $Y$ which is neither $Y$ nor $\phi$. Then $f^{-1}(U)$ is neither $X$ nor $\phi$, and hence is not an open subset of $X$. Therefore $f$ could not be continuous.

The following proposition is the generalized version of Definition 6 of Chapter 2.

**Proposition 6.** A function $f$ from a topological space $X,\tau$ to a space $Y,\tau'$ is continuous if and only if given any $f(x)\in Y$ and any neighborhood $V$ of $f(x)$, there is a neighborhood $U$ of $x$ such that

$$
f(U)\subset V.
$$

<span id="printed-page-71"></span>

<!-- Source: PDF page 73, printed page 71. -->

_Proof._ Suppose $f$ is continuous. Then if $V$ is any neighborhood of $f(x)$, $V$ is an open subset of $Y$. Therefore $f^{-1}(V)$ is an open subset of $X$ which contains $x$; that is, $f^{-1}(V)$ is a neighborhood of $x$. Setting $U=f^{-1}(V)$, we have the desired result.

Suppose that given any $f(x)\in Y$ and any neighborhood $V$ of $f(x)$, there is a neighborhood $U$ of $x$ such that $f(U)\subset V$. Let $W$ be any open subset of $Y$; we must show that $f^{-1}(W)$ is an open subset of $X$. Suppose $z\in f^{-1}(W)$. Then $f(z)\in W$, that is, $W$ is a neighborhood of $f(z)$. Then there is a neighborhood $U$ of $z$ such that $f(U)\subset W$. But then $U\subset f^{-1}(W)$. We therefore have that for each $z\in f^{-1}(W)$, $z\in U\subset f^{-1}(W)$, where $U$ is an open subset of $X$. Hence $f^{-1}(W)$ is the union of open subsets of $X$, and thus is an open subset of $X$. Therefore $f$ is continuous.

Propositions 7 and 8 give further criteria for the continuity of a function.

**Proposition 7.** Suppose that $X,\tau$ and $Y,\tau'$ are topological spaces, and that $f$ is a function from $X$ to $Y$. Let $\mathfrak{B}$ be any basis for $\tau'$. Then $f$ is continuous if and only if for each $B\in\mathfrak{B}$, $f^{-1}(B)$ is an open subset of $X$. (Compare this with Proposition 9, Chapter 2.)

_Proof._ Assume $f$ continuous. Then since each $B\in\mathfrak{B}$ is an open subset of $Y$, $f^{-1}(B)$ is an open subset of $X$. Suppose instead that $f^{-1}(B)$ is an open subset of $X$ for each $B\in\mathfrak{B}$. Let $V$ be any open subset of $Y$. Then $V=\bigcup_I B_i$, where each $B_i$ is a member of $\mathfrak{B}$ and $I$ is a suitable index set. It follows that

$$
f^{-1}(V)=f^{-1}\left(\bigcup_I B_i\right)=\bigcup_I f^{-1}(B_i).
$$

But $f^{-1}(B_i)$ is open in $X$ for each $i\in I$; hence $f^{-1}(V)$ is the union of a family of open sets and is therefore open. Consequently, $f$ is continuous.

**Corollary.** Suppose that $f$ is a function from $X,\tau$ into $Y,\tau'$ and that $\{\mathfrak{N}_y\}$, $y\in Y$, is an open neighborhood system for $\tau'$. Then $f$ is continuous if and only if given any $N\in\mathfrak{N}_y$ for any $y\in Y$, $f^{-1}(N)$ is an open subset of $X$.

_Proof._ The collection of all $N$ contained in some $\mathfrak{N}_y$ forms a basis for $\tau'$ by Proposition 6 of Chapter 3. The corollary then follows at once from Proposition 7.

**Example 9.** Let $R^2$ be the coordinate plane with the usual metric topology. A “rotation” of $R^2$ is best described using polar coordinates. If $A_0$ is an angle measured in radians, define

$$
R_{A_0}(r,A)=(r,A+A_0)
$$

<span id="printed-page-72"></span>

<!-- Source: PDF page 74, printed page 72. -->

for any point $(r,A)$ (expressed in polar coordinates) of $R^2$. The reader may recall from analytic geometry that $R_{A_0}$ is a rotation through angle $A_0$. Then $R_{A_0}^{-1}=R_{(-A_0)}$, the rotation through angle $-A_0$. Any rotation preserves congruences; in particular, if $U$ is the interior of some triangle or square, then $R_{A_0}^{-1}(U)$ is also the interior of a triangle or square. Since the family of interiors of triangles, or the family of interiors of squares, forms a basis for the standard topology on $R^2$, any rotation is continuous. The inverse of any rotation, also being a rotation, is continuous.

**Proposition 8.** Let $X,\tau$ and $Y,\tau'$ be topological spaces. Then a function $f$ from $X$ to $Y$ is continuous if and only if given any closed subset $F$ of $Y$, $f^{-1}(F)$ is a closed subset of $X$.

The proof is left as an exercise.

**Proposition 9.** Suppose that $X$ is any set and that $\tau$ and $\tau'$ are topologies for $X$. Then $\tau$ is finer than $\tau'$ if and only if the identity function $i$ from $X$ to $X$ defined by

$$
i(x)=x\qquad\text{for all }x\in X
$$

is continuous from the topological space $X,\tau$ to the topological space $X,\tau'$.

_Proof._ Assume $i$ continuous. If $U\in\tau'$, then $i^{-1}(U)=i(U)=U$ is in $\tau$. Therefore $\tau'\subset\tau$, that is, $\tau$ is finer than $\tau'$. Suppose $\tau$ is finer than $\tau'$. Then if $U\in\tau'$,

$$
i^{-1}(U)=U\in\tau
$$

(since any $\tau'$-open set is $\tau$-open). Therefore $i$ is continuous.

The following proposition shows that the composition of continuous functions is continuous.

**Proposition 10.** If $f$ is a continuous function from the space $X,\tau$ to the space $Y,\tau'$ and if $g$ is a continuous function from $Y,\tau'$ to $Z,\tau''$, then $g\circ f$ is a continuous function from $X,\tau$ to $Z,\tau''$.

_Proof._ Suppose $U$ is an open subset of $Z$. Then $g^{-1}(U)$ is an open subset of $Y$, since $g$ is continuous. But then since $f$ is continuous,

$$
f^{-1}(g^{-1}(U))=(g\circ f)^{-1}(U)
$$

is an open subset of $X$. Therefore $g\circ f$ is continuous.

Propositions 11 and 12 pertain to continuous functions as they are related to subspaces. Proposition 11 deals with a function which is known to be continuous on certain subspaces of a space $X$.

<span id="printed-page-73"></span>

<!-- Source: PDF page 75, printed page 73. -->

**Proposition 11.** If $f$ is a function from $X,\tau$ to $Y,\tau'$, $X=A\cup B$, and $f\mid A$ and $f\mid B$ are both continuous (where $A$ and $B$ are considered as subspaces of $X$), then if $A$ and $B$ are both open or both closed, $f$ is continuous.

_Proof._ We will prove Proposition 11 for the case when $A$ and $B$ are both closed. The case when $A$ and $B$ are both open is left as an exercise. We use Proposition 8. Let $F$ be any closed subset of $Y$. We must show that $f^{-1}(F)$ is a closed subset of $X$. $(f\mid A)^{-1}(F)$ is closed in $A$ and $(f\mid B)^{-1}(F)$ is closed in $B$, since $f\mid A$ and $f\mid B$ are both assumed to be continuous. But since $A$ and $B$ are closed, $(f\mid A)^{-1}(F)$ and $(f\mid B)^{-1}(F)$ are closed subsets of $X$ (see Section 4.1, Exercise 2). Since $A\cup B=X$,

$$
f^{-1}(F)=(f\mid A)^{-1}(F)\cup(f\mid B)^{-1}(F),
$$

which is a closed subset of $X$ since it is the union of two closed subsets of $X$. Therefore $f$ is continuous.

**Example 10.** Let $R$ be the set of real numbers with the topology induced by the absolute value metric. Define $f:R\to R$ by

$$
f(x)=\begin{cases}x,&\text{if }x\geq 0,\\0,&\text{if }x\leq 0.\end{cases}
$$

Set $A=\{x\mid x\geq 0\}$ and $B=\{x\mid x\leq 0\}$. Then $A\cup B=R$, $A$ and $B$ are closed, and $f\mid A$ and $f\mid B$ are easily seen to be continuous. Therefore, by Proposition 11, $f$ is continuous.

Note that $A$ and $B$ must either both be closed, or both be open. One cannot be closed and the other open. For if we continue to let $R$ be the space of real numbers with the absolute value topology and set

$$
\begin{gathered}
A=\{x\mid x\geq 0\},\\
B=\{x\mid x<0\},
\end{gathered}
$$

and define $g:R\to R$ by

$$
g(x)=\begin{cases}3,&\text{if }x\in A,\\0,&\text{if }x\in B,\end{cases}
$$

then $A$ is closed, $B$ is open; but $g$ is not continuous.

The next proposition answers the following questions:

a) Suppose that $f$ is a continuous function from $X,\tau$ onto a subspace $Y$ of $Z,\tau''$. Is $f$ then continuous as a function from $X$ to $Z$?

b) Suppose that $f$ is a continuous function from $X,\tau$ to $Y,\tau'$ and that $W$ is a subspace of $X$. Is $f\mid W$ continuous?

<span id="printed-page-74"></span>

<!-- Source: PDF page 76, printed page 74. -->

**Proposition 12.** Suppose $f$ is a continuous function from $X,\tau$ to $Y,\tau'$.

a) If $Y$ is a subspace of $Z,\tau''$, then $f$ is a continuous function from $X$ to $Z$.

b) If $W$ is a subspace of $X$, then $f\mid W$ is a continuous function from $W$ to $Y$.

_Proof_

a) Suppose $U$ is an open subset of $Z$. Then $Y\cap U$ is open in $Y$; hence $f^{-1}(Y\cap U)$ is an open subset of $X$. But $f(x)\in Y$ for every $x\in X$, and thus

$$
f^{-1}(U)=f^{-1}(Y\cap U)
$$

is an open subset of $X$. Therefore $f$ is continuous as a function from $X$ to $Z$.

The proof of (b) is left as an exercise.

**Example 11.** Let $W$ be a subspace of an $X,\tau$. The identity function restricted to $W$, $i\mid W$, is sometimes called the _inclusion mapping_ of $W$ into $X$. If $\tau_W$ denotes the subspace topology on $W$, then

$$
i\mid W:W,\tau_W\to W,\tau_W
$$

is continuous; hence, applying Proposition 12(a), $i\mid W:W,\tau_W\to X,\tau$ is continuous. Looking at it another way, $i:X,\tau\to X,\tau$ is continuous, and therefore, by Proposition 12(b), $i\mid W$ is also continuous.

Note that Proposition 12(a) implies that we never would lose any generality by assuming that a continuous function was onto.

## Exercises

1. Prove Proposition 8.

2. Prove Proposition 12(b).

3. Suppose that $f$ is a function from a space $X,\tau$ to a set $Y$. Define a subset $U$ of $Y$ to be open if $f^{-1}(U)$ is an open subset of $X$. Prove that the set of open subsets of $Y$ then forms a topology $\tau'$. Further, prove that $f$ is continuous from $X,\tau$ to $Y,\tau'$. Show that $\tau'$ is the finest topology for which $f$ is continuous.

4. Let $f$ be a function from a set $X$ to a topological space $Y,\tau'$. Define a subset $U$ of $X$ to be open if $U=f^{-1}(V)$ for some open subset $V$ of $Y$. Prove that the family of open subsets of $X$ thus obtained forms a topology $\tau$ on $X$. Prove that $f:X,\tau\to Y,\tau'$ is continuous. Prove that $\tau$ is the coarsest topology for which $f$ is continuous.

5. In Example 11, show that the subspace topology is the coarsest topology for which $i\mid W$ is continuous.

::: warning Missing source page 75
Printed page 75 is absent. The next supplied page, printed page 76, has section label 4.4 and begins with Figures 4.1–4.2 and Example 13. Any intervening exercises and the opening of §4.4, including Definition 4 and Example 12, are unavailable and have not been reconstructed.
:::

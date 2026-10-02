---
title: The Topologically Derived Sets in Subspaces — Elementary Topology
description: Section 4.2 of the supplied second-edition scan.
---

# 4.2 The Topologically Derived Sets in Subspaces

::: info Transcription note
Source: printed pages 67–69 (PDF pages 69–71). Printed page 67 begins with the final exercise of §4.1, transcribed on that section's page. The source's closure, interior, frontier, and derived-set notation is retained.
:::

<span id="printed-page-67"></span>

<!-- Source: PDF page 69, printed page 67; section 4.2 fragment. -->

Suppose that $X,\tau$ is a topological space and that $Y$ is a subspace of $X$. If $A$ is a subset of $Y$, then $A$ is also a subset of $X$. We may wish to know the sets topologically associated with $A$ either with respect to the topology on $X$ or with respect to the subspace topology on $Y$. As we shall see, the corresponding sets are not always equal, nor should we expect them to be, since it is not at all true that a set open in $Y$ is necessarily open in $X$. Nevertheless, the subspace topology on $Y$ is defined in terms of the topology on $X$; there should therefore be some relationships between the corresponding sets. It is the purpose of this section to investigate these relationships.

**Example 4.** Assume that $R$ is the set of real numbers with the topology induced by the absolute value metric. Let

$$
Y=(0,1)\qquad\text{and}\qquad A=\{x\mid 0<x<1\text{ and }x\text{ is rational}\}.
$$

Then the closure of $A$ in $Y$ is $(0,1)$, while the closure of $A$ in $R$ is $[0,1]$. $\operatorname{Fr}Y$ in $Y$ is $\phi$, since no subset of $Y$, open or otherwise, contains any elements of $Y-Y=\phi$. But $\operatorname{Fr}Y$ in $R$ is $\{0,1\}$.

**Example 5.** Let the plane $R^2$ have the topology described in Example 5 of Chapter 3. Let

$$
Y=\{(x,y)\mid x^2+y^2\leq 1\}\qquad\text{and}\qquad A=\{(x,y)\mid x^2+y^2<1\}.
$$

There is no closed subset of $R^2$, except $R^2$, which contains $A$; that is, no union of finitely many lines and points contains all of $A$. Therefore the closure of $A$ in $R^2$ is all of $R^2$. Suppose $F$ is a subset of $Y$ which is closed in $Y$ and which contains $A$. Applying Proposition 3 below, then $F=Y\cap F'$, where $F'$ is a closed subset of $R^2$. But $F'$ contains $F$; hence $A\subset F'$. Since $R^2$ is the only closed subset of $R^2$ which contains $A$, it follows that $F'=R^2$. Thus $F=Y\cap R^2=Y$. We have then that the closure of $A$ in $Y$ is $Y$.

**Proposition 3.** If $X,\tau$ is a topological space and $Y$ is a subspace of $X$, then a subset $F$ of $Y$ is closed in $Y$ if and only if $F=Y\cap F'$, where $F'$ is a closed subset of $X$.

_Proof._ Suppose $F$ is closed in $Y$. Then $Y-F$ is open in $Y$; therefore

$$
Y-F=Y\cap U,
$$

<span id="printed-page-68"></span>

<!-- Source: PDF page 70, printed page 68. -->

where $U$ is an open subset of $X$. But then $Y-(Y-F)=F=Y\cap(X-U)$. Since $U$ is open, $X-U$ is closed; hence $F$ is of the desired form.

Suppose further that $F=Y\cap F'$, where $F'$ is a closed subset of $X$. Then

$$
Y-F=Y\cap(X-F').
$$

But $X-F'$ is open; hence $Y-F=Y\cap(X-F')$ is open in $Y$. Therefore $F$ is closed in $Y$.

Note that this proposition tells us that the subspace topology on $Y$ could equally well have been defined by defining the subsets of $Y$ which are closed in $Y$ in the same way that the subsets of $Y$ which are open in $Y$ are defined (substituting closed for open, of course) in Definition 1.

**Proposition 4.** Let $X,\tau$ be a topological space, and let $Y$ be a subspace of $X$. If $A\subset Y$, then

$$
\operatorname{Cl}A\text{ in }Y=Y\cap\operatorname{Cl}A\text{ (in }X\text{)}.
$$

_Proof._ $\operatorname{Cl}A$ is closed in $X$ and $A\subset\operatorname{Cl}A$; hence $Y\cap\operatorname{Cl}A$ is a closed (in $Y$) subset of $Y$ (Proposition 3) which contains $A$. Therefore $\operatorname{Cl}A$ in $Y\subset Y\cap\operatorname{Cl}A$. Now $\operatorname{Cl}A$ in $Y$ is a closed (in $Y$) subset of $Y$, and thus, again by Proposition 3, there is a closed subset $F$ of $X$ such that $\operatorname{Cl}A$ in $Y=Y\cap F$. But $A\subset F$; hence $\operatorname{Cl}A\subset F$. It follows that

$$
Y\cap\operatorname{Cl}A\subset Y\cap F=\operatorname{Cl}A\text{ in }Y.
$$

We therefore have $\operatorname{Cl}A$ in $Y=Y\cap\operatorname{Cl}A$.

The reader might be tempted to conjecture that $A^\circ$ in $Y=Y\cap A^\circ$. This is not true, as is shown by the next example.

**Example 6.** Let $R^2$ be the coordinate plane with the topology induced by the Pythagorean metric, $Y=\{(x,y)\mid y=0\}$, that is, $Y$ is the $x$-axis, and $A=Y$. Then $A^\circ$ in $Y=Y$, while

$$
Y\cap A^\circ=Y\cap\phi=\phi,
$$

since $Y$ contains no open subset of $R^2$. We might also note that $\operatorname{Fr}A$ in $Y=\phi$, while $\operatorname{Fr}A=A$; hence it is also false that $\operatorname{Fr}A$ in $Y=Y\cap\operatorname{Fr}A$.

**Proposition 5.** Suppose that $X,\tau$ is a topological space and that for each $x\in X$, we have a collection $\mathfrak{N}_x$ of subsets of $X$ such that the $\mathfrak{N}_x$ form an open neighborhood system for $X$. Let $Y\subset X$. Then setting

$$
\mathfrak{N}'_y=\{Y\cap N\mid N\in\mathfrak{N}_y\}
$$

<span id="printed-page-69"></span>

<!-- Source: PDF page 71, printed page 69. -->

for each $y\in Y$, we obtain an open neighborhood system for the subspace topology of $Y$.

_Proof._ We must show that the $\mathfrak{N}'_y$ satisfy Definition 5 of Chapter 3.

i) Since there is at least one $N\in\mathfrak{N}_y$ for each $y\in Y\subset X$, there is $N\cap Y\in\mathfrak{N}'_y$; hence $\mathfrak{N}'_y\ne\phi$.

ii) Definition 5(ii) follows at once from the fact that $y\in N$ for each $N\in\mathfrak{N}_y$.

iii) Suppose $N'_1$ and $N'_2$ are in $\mathfrak{N}'_y$. Then $N'_1=Y\cap N_1$ and $N'_2=Y\cap N_2$ for some $N_1$ and $N_2$ in $\mathfrak{N}_y$. There is, however, $N_3\in\mathfrak{N}_y$ such that $N_3\subset N_1\cap N_2$. Therefore

$$
N'_3=Y\cap N_3\subset N'_1\cap N'_2\qquad\text{and}\qquad N'_3\in\mathfrak{N}'_y.
$$

The proofs of (iv) and (v) are left as exercises.

Since $N\in\mathfrak{N}_y$ is an open subset of $X$, $N'=Y\cap N$ is an open (in $Y$) subset of $Y$. Therefore we do have an open neighborhood system for the subspace topology on $Y$.

## Exercises

1. Prove (iv) and (v) in Proposition 5.

2. Assume $U$ to be an open subset of a topological space $X,\tau$ and $A\subset U$. Is it true that $\operatorname{Fr}A$ in $U=\operatorname{Fr}A\cap U$? Does this equality hold if $U$ is a closed subset of $X$ rather than an open subset?

3. Why is it true that $\operatorname{Cl}A$ in $Y=\operatorname{Cl}A\cap Y$, but that $A^\circ$ in $Y\ne A^\circ\cap Y$? (See Proposition 4 and Example 6.) Try to reproduce the proof of Proposition 4 for $A^\circ$ instead of $\operatorname{Cl}A$ and see where the proof fails.

4. Let $R^2$ be the coordinate plane with the usual Pythagorean metric. Let

   $$
   Y=\{(x,y)\mid x^2+y^2<1,\text{ or }x=0\text{ or }1\text{ and }y=0\text{ or }1\}.
   $$

   For each of the following subsets of $Y$ compare the sets topologically derived from these sets in $Y$ with those topologically derived in $X$. That is, compute $\operatorname{Cl}A$ in $Y$ and compare it with $\operatorname{Cl}A$ in $X$, etc.

   a) $\{(x,y)\mid x=0\text{ and }y=1/n,\ n\text{ a positive integer, or }y=0\}$

   b) $\{(x,y)\mid x^2+y^2<1\}$

   c) $\{(x,y)\mid\text{either }x\text{ or }y\text{ is irrational}\}$

5. Suppose that $Y$ is a subspace of $X,\tau$ and that $A\subset Y$. Prove

   a) $\operatorname{Fr}A$ in $Y\subset\operatorname{Fr}A\cap Y$;

   b) $A'\subset A'$ in $Y$.

6. Compute $\operatorname{Ext}A$, $A'$, $A^\circ$, and $\operatorname{Fr}A$ in $Y$, for $A$ and $Y$ in Example 5.

7. Find a necessary and sufficient condition for each subset $A$ of a subspace $W$ of a space $X,\tau$ to have the same frontier relative to $W$ as $A$ has relative to $X$. Find such a condition on $W$ in order to have $A'$ in $W$ equal to $A'$ (in $X$) for each subset $A$ of $W$.

---
title: Subspaces — Elementary Topology
description: Section 4.1 of the supplied second-edition scan.
---

# 4.1 Subspaces

::: info Transcription note
Source: printed pages 64–67 (PDF pages 66–69). Printed page 67 continues with §4.2 after Exercise 8. Source wording and notation are retained; the source uses $\subset$ for inclusion, including equality.
:::

<span id="printed-page-64"></span>

<!-- Source: PDF page 66, printed page 64. -->

We have already noted that if $X,D$ is a metric space and $Y\subset X$, then $Y,D\mid Y$ is also a metric space (Section 2.1, Example 4). Recall that a subset $W$ of $Y$ is $D\mid Y$-open if and only if $W=Y\cap U$, where $U$ is a $D$-open subset of $X$ (Section 2.4, Exercise 2). However, $Y$ is not merely a subset of $X$, but is a _subspace_ of $X$, and the topology which $D\mid Y$ induces on $Y$ can be defined by means of the topology which $D$ induces on $X$. Suppose $Y$ is a subset of a topological space $X,\tau$. It is reasonable then to inquire whether there is any topology on $Y$ which is “induced” by $\tau$. Using what we learned about metric spaces, the following definition seems in order.

**Definition 1.** Let $X,\tau$ be a topological space and $Y\subset X$. A subset $W$ of $Y$ is said to be _open in $Y$_ if

$$
W=Y\cap U,\qquad\text{where }U\in\tau.
$$

**Proposition 1.** If $X,\tau$ is a topological space and $Y\subset X$, then the set of all subsets of $Y$ which are open in $Y$ forms a topology for $Y$.

_Proof._ We shall show that the set of all subsets of $Y$ which are open in $Y$ satisfies the definition of a topology on $Y$ (Chapter 3, Definition 1).

i) $X$ and $\phi$ are members of $\tau$. Therefore $Y\cap X=Y$ and $Y\cap\phi=\phi$ are open in $Y$.

ii) Suppose $U$ and $V$ are open in $Y$. Then $U=Y\cap U'$ and $V=Y\cap V'$, where $U'$ and $V'$ are open subsets of $X$. Then

$$
U\cap V=(Y\cap U')\cap(Y\cap V')=Y\cap(U'\cap V').
$$

But $U'\cap V'$ is an open subset of $X$; hence $U\cap V$ is an open subset of $Y$.

iii) Suppose $\{U_i\}$, $i\in I$, is a family of open subsets of $Y$. Then $U_i=Y\cap U'_i$, where $U'_i$ is an open subset of $X$ for each $i\in I$.

<span id="printed-page-65"></span>

<!-- Source: PDF page 67, printed page 65. -->

Then

$$
\bigcup_I U_i=\bigcup_I(Y\cap U'_i)=Y\cap\left(\bigcup_I U'_i\right).
$$

Since $\bigcup_I U'_i$ is the union of open sets, it is open, and therefore $\bigcup_I U_i$ is open in $Y$. The set of subsets of $Y$ which are open in $Y$ therefore forms a topology on $Y$.

Proposition 1 enables us to make the following definition.

**Definition 2.** The topology on $Y$ described in Definition 1 and Proposition 1 is called the _subspace topology_ on $Y$. $Y$ with this topology is said to be a _subspace_ of $X$. If $X,\tau$ is a topological space and $Y\subset X$, then $Y$ will be assumed to have the subspace topology when considered as a topological space, unless explicitly stated otherwise.

**Example 1.** If $X,D$ is a metric space and $Y\subset X$, then the topology induced on $Y$ by $D\mid Y$ is the same as the subspace topology on $Y$ induced by the metric topology on $X$. It was in fact this example which inspired us to define the subspace topology as we did. (See the remarks opening this chapter.)

**Example 2.** Let $N$ be the set of positive integers with the topology defined by declaring a set to be open if it contains all but at most finitely many elements of $N$. Let $Y=\{1,2,3,4,5\}$. We now show that the subspace topology on $Y$ is the discrete topology. Suppose $n\in Y$. Then

$$
U(n)=\{n\}\cup(N-Y)
$$

is an open subset of $N$, since it excludes only four positive integers. Therefore $U(n)\cap Y=\{n\}$ is open in $Y$. Every one-point subset of $Y$ is therefore open in $Y$, and hence every subset of $Y$ (being the union of one-point subsets) is open in $Y$. Consequently $Y$ has the discrete topology. We thus see that it is quite possible for a subspace to have the discrete topology even when the space itself does not have the discrete topology.

**Example 3.** If a set $X$ has the discrete topology, then every subspace of $X$ has the discrete topology. If $X$ has the trivial topology, then every subspace of $X$ has the trivial topology. The proof of these assertions is left as an exercise.

**Proposition 2.** If $Y$ is a subspace of $X$ and $W$ is a subspace of $Y$, then $W$ is a subspace of $X$. That is, if $Y$ is given the subspace topology from $X$, and then a subset $W$ of $Y$ is given the subspace topology considered as a subset of the topological space $Y$, then $W$ would be given the same topology as though $W$ were considered as a subset of $X$ and were given the subspace topology.

<span id="printed-page-66"></span>

<!-- Source: PDF page 68, printed page 66. -->

_Proof._ Let $\tau_Y$ be the topology on $W$ considered as a subspace of $Y$, and let $\tau_X$ be the topology on $W$ considered as a subspace of $X$. We must show that $\tau_X=\tau_Y$. Suppose $U\in\tau_X$. Then $U=W\cap U'$, where $U'$ is open in $X$. But then

$$
U=W\cap U'=(W\cap Y)\cap U'=W\cap(Y\cap U'),
$$

since $W\subset Y$; hence $U\in\tau_Y$. On the other hand, if $U\in\tau_Y$, then $U=W\cap U'$, where $U'$ is open in $Y$. But since $U'$ is open in $Y$, $U'=Y\cap U''$, where $U''$ is open in $X$. It follows that

$$
U=W\cap U'=W\cap(Y\cap U'')=(W\cap Y)\cap U''=W\cap U'',
$$

and hence $U\in\tau_X$. Therefore $\tau_X=\tau_Y$.

## Exercises

1. a) Prove that every subspace of a topological space with the discrete topology has the discrete topology.

   b) Prove that every subspace of a space with the trivial topology has the trivial topology.

2. Suppose that $X,\tau$ is a topological space and that $F$ is a closed subset of $X$. Prove that a subset $W$ of $F$ is closed in $F$ if and only if $W$ is a closed subset of $X$. Make and prove the corresponding statement about open subsets of $X$.

3. Assume $X,\tau$ a topological space and $\mathfrak{B}$ a basis for $\tau$. If $Y\subset X$, define

   $$
   \mathfrak{B}_Y=\{B\cap Y\mid B\in\mathfrak{B}\}.
   $$

   Prove that $\mathfrak{B}_Y$ is a basis for the subspace topology on $Y$.

4. Suppose $R$ is the space of real numbers with the topology induced by the absolute value metric. Prove that the following subsets of $R$ do not have the discrete subspace topology.

   a) the set of rational numbers

   b) $\{x\mid x=0,\text{ or }x=1/n,\text{ where }n\text{ is a positive integer}\}$

   c) $\{x\mid x=q\pi,\text{ where }q\text{ is a rational number}\}$

   d) any subset of $R$ which is somewhere dense

5. Let $X,D$ be any metric space. Prove that if $Y$ is a finite subset of $X$, then the subspace $Y$ has the discrete topology. Is this true if we replace _finite_ by _countable_?

6. Prove or disprove: A subset $A$ of a topological space $X,\tau$ is nowhere dense if and only if the subspace $A$ has the discrete topology.

7. Suppose $X,\tau$ is a topological space with the property that every two-point subspace of $X$ has the trivial topology. Prove that $X$ has the trivial topology. Show that the corresponding statement about the discrete topology is not true.

<span id="printed-page-67"></span>

<!-- Source: PDF page 69, printed page 67; section 4.1 fragment. -->

8. Find the coarsest topology on $R^2$ for which each finite subspace of $R^2$ has the discrete topology. Find the finest topology on $R^2$ for which each finite subspace of $R^2$ has the trivial topology.

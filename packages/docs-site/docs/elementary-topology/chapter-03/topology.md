---
title: The Notion of a Topology — Elementary Topology
description: Section 3.1, printed pages 40–43, of Elementary Topology, second edition.
---

# 3.1 The Notion of a Topology

::: info Transcription note
Source: printed pages 40–43 (PDF pages 45–48). Printed page 43 continues with §3.2 after the exercises. The Gothic letter $\mathfrak{F}$ denotes the source's family of closed sets; $\mathfrak{N}$ denotes the collection of neighborhoods in Exercise 3.
:::

<span id="printed-page-40"></span>

<!-- source: PDF 45, printed 40 -->

The fundamental properties of open subsets of a metric space are outlined in Proposition 2 of Chapter 2. Mathematicians have found from experience that families of subsets having these same properties arise in contexts other than those of metric spaces; hence it is reasonable to study these properties in their own right, abstracted from the limitations that metric spaces impose. In particular, the properties of open sets in metric spaces inspire the following definition.

**Definition 1.** Let $X$ be any set. A collection $\tau$ of subsets of $X$ is said to be a _topology_ on $X$ if the following axioms are satisfied:

i) $X$ and $\phi$ are members of $\tau$.

ii) The intersection of any two members of $\tau$ is a member of $\tau$.

iii) The union of any family of members of $\tau$ is again in $\tau$.

The members of $\tau$ are then said to be _$\tau$-open_ subsets of $X$, or merely _open_ subsets of $X$ if no confusion may result.

**Example 1.** If $X,D$ is a metric space, then the $D$-open subsets of $X$ form a topology on $X$. This topology is called the _metric topology induced on $X$ by $D$_. It was, of course, this topology that we studied in Chapter 2.

**Example 2.** Let $X$ be any set. Then the family of all subsets of $X$ forms a topology on $X$. This topology consisting of all of the subsets of $X$ is called the _discrete topology_ on $X$. The discrete topology contains the maximum possible number of open sets since, relative to the discrete topology, every subset of $X$ is open.

**Example 3.** If $X$ is any set, then the collection $\{X,\phi\}$ of subsets of $X$ also forms a topology on $X$. This topology is called the _trivial_ (by some, the _indiscrete_) _topology_ on $X$. It contains the fewest possible open sets compatible with having a topology on $X$.

The discrete and trivial topologies represent opposite extremes. Topologies which are of genuine interest usually lie somewhere in between; for example, the topology induced on the set of real numbers by the

<span id="printed-page-41"></span>

<!-- source: PDF 46, printed 41 -->

absolute value metric is neither the trivial nor the discrete topology. We now give an example of a topology which is neither discrete nor trivial, but also is not related to any metric.

**Example 4.** Let $X=\{a,b\}$. Define $\tau=\{X,\phi,\{a\}\}$. It is easily verified that $\tau$ is a topology on $X$. Suppose $D$ to be any metric on $X$, and set $\rho=D(a,b)$. Then $N(b,\rho)=\{b\}$, and it follows that $\{b\}$ is a $D$-open set. But $\{b\}$ is not a $\tau$-open set; hence $\tau$ could not be the topology induced on $X$ by $D$.

**Definition 2.** A set $X$ with topology $\tau$ is called a _topological space_. Just as $X,D$ was used to denote a set $X$ with metric $D$, so $X,\tau$ will be used to denote a set $X$ with topology $\tau$.

As in metric spaces, so in a topological space $X,\tau$ we say that a subset $F$ of $X$ is _$\tau$-closed_ (or merely _closed_) if $F=X-U$, where $U$ is a $\tau$-open set. (Compare this to Definition 4 of Chapter 2.)

**Proposition 1.** Let $X,\tau$ be a topological space. Then the closed subsets of $X$ have the following properties.

a) $X$ and $\phi$ are closed subsets of $X$.

b) The union of any two closed subsets of $X$ is again a closed subset of $X$.

c) The intersection of any family of closed subsets of $X$ is again a closed subset of $X$.

The proof is the same as the proof of Proposition 4, Chapter 2.

The next proposition shows that rather than defining a topology on a set by specifying the open subsets, we may equally well determine the topology by specifying the closed subsets.

**Proposition 2.** Let $X$ be any set, and suppose that $\mathfrak{F}$ is a family of subsets of $X$ such that

i′) $X$ and $\phi$ are in $\mathfrak{F}$;

ii′) the union of any two members of $\mathfrak{F}$ is a member of $\mathfrak{F}$;

iii′) the intersection of any family of members of $\mathfrak{F}$ is a member of $\mathfrak{F}$.

If we now define a subset $U$ of $X$ to be open if and only if $U=X-F$, where $F$ is some element of $\mathfrak{F}$, then the set $\tau$ of open sets thus formed is a topology on $X$ with $\mathfrak{F}$ as the set of ($\tau$-) closed subsets of $X$.

_Proof._ We first show that $\tau$ is a topology on $X$ by verifying that $\tau$ satisfies Definition 1.

i) $X$ and $\phi$ are in $\tau$. Since $X$ and $\phi$ are in $\mathfrak{F}$, and since $X=X-\phi$ and $\phi=X-X$, then $X$ and $\phi$ are in $\tau$.

<span id="printed-page-42"></span>

<!-- source: PDF 47, printed 42 -->

ii) The intersection of any two members of $\tau$ is a member of $\tau$. Suppose $U$ and $V$ are members of $\tau$. Then

$$
U=X-F_1\quad\text{and}\quad V=X-F_2,
$$

where $F_1$ and $F_2$ are in $\mathfrak{F}$. Therefore

$$
U\cap V=(X-F_1)\cap(X-F_2)=X-(F_1\cup F_2).
$$

But, by (ii′), $F_1\cup F_2\in\mathfrak{F}$; hence $U\cap V\in\tau$.

iii) The union of any family of members of $\tau$ is a member of $\tau$. Suppose $\{U_i\}$, $i\in I$, is a family of members of $\tau$. It follows that for each $i\in I$, $U_i=X-F_i$, where $F_i\in\mathfrak{F}$. Then

$$
\bigcup_I U_i=\bigcup_I(X-F_i)=X-\bigcap_I F_i.
$$

But, by (iii′), $\bigcap_I F_i\in\mathfrak{F}$; hence $\bigcup_I U_i\in\tau$.

Therefore $\tau$ satisfies the definition of a topology on $X$.

It remains to be shown that $\mathfrak{F}$ is the set of closed sets for the topology $\tau$. Suppose $F$ is $\tau$-closed. Then $F=X-U$, where $U\in\tau$. But $U=X-F'$, where $F'\in\mathfrak{F}$; therefore

$$
F=X-(X-F')=F'\in\mathfrak{F}.
$$

On the other hand, if $F\in\mathfrak{F}$, then $X-F\in\tau$. Then, since $F=X-(X-F)$, $F$ is $\tau$-closed. The members of $\mathfrak{F}$ are therefore precisely the $\tau$-closed subsets of $X$.

**Example 5.** We define a family $\mathfrak{F}$ of subsets of $R^2$, the coordinate plane, as follows: Let $F\in\mathfrak{F}$ if and only if $F=R^2$, $F=\phi$, or $F$ is a set consisting of finitely many points together with the union of finitely many straight lines. By hypothesis, $X$ and $\phi$ are in $\mathfrak{F}$. It follows from the fact that two straight lines can only intersect in either a straight line (if they coincide), a point, or the empty set, that the intersection of any family of members of $\mathfrak{F}$ is again a member of $\mathfrak{F}$. Since the union of finitely many lines and points with finitely many more lines and points still consists of finitely many lines and points, the union of any two members of $\mathfrak{F}$ is also a member of $\mathfrak{F}$. Therefore, $\mathfrak{F}$ satisfies (i′) through (iii′) of Proposition 2, and hence determines a topology on $R^2$. The topology which $\mathfrak{F}$ determines is in fact the smallest topology in which lines and points are closed sets. It is not, however, the topology induced on $R^2$ by the Pythagorean metric $D$; for a subset of $R^2$ can exclude at most finitely many lines and still be open in the topology determined by $\mathfrak{F}$, but $\{(x,y)\mid x^2+y^2<1\}$ excludes infinitely many lines and still is $D$-open.

<span id="printed-page-43"></span>

<!-- source: PDF 48, printed 43; section 3.1 fragment -->

## Exercises

1. Prove that the intersection of finitely many open sets is open and that the union of finitely many closed sets is closed.
2. Suppose $X,\tau$ a topological space, and $A\subset X$. The _interior_ of $A$, denoted by $A^\circ$, is defined by $A^\circ=\bigcup\{U\in\tau\mid U\subset A\}$. Prove the following:

   a) $(A^\circ)^\circ=A^\circ$.

   b) $A^\circ\subset A$.

   c) $(A\cap B)^\circ=A^\circ\cap B^\circ$.

   d) A subset $U$ of $X$ is open if and only if $U=U^\circ$.

3. Let $R^2$ be the plane with the Pythagorean metric. Let $\mathfrak{N}$ be the set of all $\rho$-neighborhoods in $R^2$. Which properties of a topology for $R^2$ does $\mathfrak{N}$ fail to satisfy? Let $\tau$ be the set of all unions of elements of $\mathfrak{N}$. Prove that $\tau$ is a topology for $R^2$. What topology is this?
4. Find all possible topologies for the set $\{1,2,3\}$.
5. Suppose $X$ a set with more than one element. Prove that there is no metric on $X$ which induces the trivial topology on $X$. Find a metric for $X$ which induces the discrete topology on $X$.
6. Let $N$ be the set of positive integers. Define a subset $F$ of $N$ to be closed if $F$ contains a finite number of positive integers, or $F=N$. Show that the closed subsets of $N$ thus defined satisfy the conditions of Proposition 2, and hence can be used to define a topology on $N$. Prove that this topology is not induced by any metric. [_Hint:_ Show that the topology does not satisfy Proposition 12 of Chapter 2.]
7. Prove that the topology defined on $R^2$ in Example 5 is really the smallest topology in which lines and points are closed sets. Would it be possible to have a topology on $R^2$ in which every line was a closed set, but every one-point subset was not? in which every one-point subset was closed, but every line was not? Try to find a topology satisfying the latter condition.
8. a) Define explicitly, that is, characterize completely the members of, the topology on $R^2$ which has the fewest members and relative to which each one point subset of $R^2$ is closed.

   b) Characterize the smallest topology on $R^2$ relative to which each straight line of $R^2$ is closed. Is this the same topology found in (a)? Does it contain the topology found in (a)?

[Continue to 3.2 Bases and Subbases](./bases-and-subbases)

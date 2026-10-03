---
title: Filters — Elementary Topology
description: The available text of section 6.7 of the supplied second-edition scan.
---

# 6.7 Filters

::: info Transcription note
Source: printed pages 133–136 (PDF pages 128–131). Printed page 136 also begins §6.8, transcribed separately. The source's Fraktur collection notation and the phrase “a filter on $x$” at the end of printed page 133 are retained.
:::

<span id="printed-page-133"></span>

<!-- Source: PDF page 128, printed page 133. -->

There is an alternative approach to the concept of convergence in a general topological space through the notion of a _filter_. While the study of filters is of great importance in point set topology, we will accomplish much of what filters might be useful for by using nets. Nevertheless, we will introduce the notion of a filter now and study some of its basic properties for two reasons: (1) the notion of a filter is sufficiently important that anyone studying even introductory point set topology should at least know what a filter is; and (2) the reader should come to realize that, even in mathematics, there may be many means to the same end. A proposition in mathematics may have many proofs, and different machinery can be developed to accomplish the same task. We will in this section try to stress the relations between nets and filters and the analogies in their use. We would expect that since nets and filters are both designed for the study of convergence, there will have to be many theorems about filters completely analogous to theorems stated in terms of nets.

**Definition 5.** Let $X$ be any set. A collection $\mathfrak{a}$ of nonempty subsets of $X$ is said to be a _filter_ on $X$ if

i) $\mathfrak{a}\ne\phi$;

ii) if $A$ and $B$ are in $\mathfrak{a}$, then $A\cap B$ is also in $\mathfrak{a}$;

iii) if $A\in\mathfrak{a}$ and $A\subset B$, then $B\in\mathfrak{a}$.

If $X,\tau$ is a topological space and $\mathfrak{a}$ is a filter on $X$, then $\mathfrak{a}$ is said to _converge_ to $x$, denoted by $\mathfrak{a}\longrightarrow x$, if every neighborhood of $x$ is a member of $\mathfrak{a}$. $\mathfrak{a}$ is said to have $x$ as a _limit point_ if every neighborhood of $x$ meets every member of $\mathfrak{a}$. That is, $\mathfrak{a}\longrightarrow x$ if given any neighborhood $U$ of $x$, $U\in\mathfrak{a}$. $x$ is a limit point of $\mathfrak{a}$ if given any neighborhood $U$ of $x$, and any $A\in\mathfrak{a}$, then $U\cap A\ne\phi$.

**Example 15.** If $X$ is any set and $Y$ is any nonempty subset of $X$, then the family $\mathfrak{a}$ of all subsets of $X$ which contain $Y$ is a filter on $X$. We verify that $\mathfrak{a}$ satisfies Definition 5. Since $Y\ne\phi$, each set which contains $Y$ is nonempty.

i) Since $Y\subset Y$, $Y\in\mathfrak{a}$; hence $\mathfrak{a}\ne\phi$.

ii) If $A$ and $B$ are in $\mathfrak{a}$, then $Y\subset A$ and $Y\subset B$; thus $Y\subset A\cap B$, and therefore $A\cap B\in\mathfrak{a}$.

iii) If $A\in\mathfrak{a}$, then $Y\subset A$. Therefore if $A\subset B$, then $Y\subset A\subset B$, and thus $B\in\mathfrak{a}$. Hence $\mathfrak{a}$ is a filter on $X$.

If $X,\tau$ is a space and $x\in X$, then $T(X,x)$, the family of all neighborhoods of $x$, is not a filter on $X$, since given any neighborhood $U$ of $x$, it is not necessarily true that any subset of $X$ which contains $U$ is also a neighborhood of $x$. If we let $T^*(X,x)$ be the family of all subsets $A$ of $X$ such that $A$ contains a neighborhood of $x$, then $T^*(X,x)$ is a filter on $x$. Moreover, <span id="printed-page-134"></span><!-- Source: PDF page 129, printed page 134. --> since $T(X,x)\subset T^*(X,x)$,

$$
T^*(X,x)\longrightarrow x.
$$

(Compare this to Examples 3, 6, and 9.)

**Example 16.** Let $X$ be any set and let $\mathfrak{D}$ be a nonempty collection of nonempty subsets of $X$ with the property that if $B$ and $B'$ are in $\mathfrak{D}$, then there exists $B''\in\mathfrak{D}$ such that $B''\subset B\cap B'$. Let

$$
\mathfrak{a}=\{A\mid B\subset A,\ B\in\mathfrak{D}\}.
$$

Then $\mathfrak{a}$ is a filter on $X$ (Exercise 1). $\mathfrak{a}$ is said to be the filter _generated_ by $\mathfrak{D}$ and $\mathfrak{D}$ is said to be a _basis_ for the filter $\mathfrak{a}$.

**Example 17.** Suppose $\{s_i\}$, $i\in I$, is a net in a set $X$. Let $\mathfrak{D}$ be the family of all subsets of the form $B_j=\{s_i\mid j\leq i\}$, for all $j\in I$. Then the family $\mathfrak{D}$ is a nonempty collection of nonempty subsets of $X$ having the property that if $B_j$ and $B_{j'}$ are members of $\mathfrak{D}$, then there is $B_{j''}\in\mathfrak{D}$ such that

$$
B_{j''}\subset B_j\cap B_{j'}
$$

(Exercise 2). $\mathfrak{D}$ thus forms the basis for a (unique) filter $\mathfrak{a}$ as in Example 16; specifically,

$$
\mathfrak{a}=\{A\mid B_j\subset A\text{ for some }j\in I\}.
$$

$\mathfrak{a}$ is said to be the _filter generated_ by the net $\{s_i\}$, $i\in I$.

**Proposition 14.** Let $\{s_i\}$, $i\in I$, be a net in a space $X,\tau$ and let $\mathfrak{a}$ be the filter generated by $\{s_i\}$, $i\in I$. Then

a) $\mathfrak{a}\longrightarrow x$ if and only if $s_i\longrightarrow x$;

b) $\mathfrak{a}$ has $x$ as a limit point if and only if $x$ is a limit point of $\{s_i\}$, $i\in I$.

_Proof_

a) Suppose $s_i\longrightarrow x$. Then given any neighborhood $U$ of $x$, there is $j\in I$ such that $B_j$ (using the notation of Example 17) is a subset of $U$. Since $B_j\subset U$, $U\in\mathfrak{a}$. Therefore every neighborhood of $x$ is in $\mathfrak{a}$, and hence $\mathfrak{a}\longrightarrow x$.

Suppose $\mathfrak{a}\longrightarrow x$. Then given any neighborhood $U$ of $x$, there is $j\in I$ such that $B_j\subset U$. Hence if $j\leq i$, $s_i\in U$. Thus $\{s_i\}$, $i\in I$, is residually in every neighborhood of $x$, and therefore $s_i\longrightarrow x$.

The proof of (b) is left as an exercise.

In Example 17 we associated a filter with any net. In order for this association to be at all meaningful, Proposition 14 was a necessity. For if Proposition 14 were not true, then a net might converge without the corresponding filter converging, or we might have net and filter converging <span id="printed-page-135"></span><!-- Source: PDF page 130, printed page 135. --> to different points. But if nets and filters are merely to be different approaches to the same concept, this would be intolerable. We now show that starting with a filter $\mathfrak{a}$, we can associate a net with $\mathfrak{a}$ which has the same convergence properties as $\mathfrak{a}$.

Let $\mathfrak{a}$ be a filter on a set $X$. We will construct a net _based_ on $\mathfrak{a}$. $\mathfrak{a}$ is a collection of nonempty subsets of $X$. If $A$ and $B$ are in $\mathfrak{a}$, let

$$
A\leq B\qquad\text{if}\qquad B\subset A.
$$

It is easy to verify that this makes $\mathfrak{a}$ into a directed set. Since each $A\in\mathfrak{a}$ is a nonempty set, we can find a selection function $s$ from the directed set $\mathfrak{a}$ into $X$ such that $s(A)\in A$. Then $\{s_A\}$, $A\in\mathfrak{a}$, is a net in $X$ called a _net based_ on $\mathfrak{a}$.

**Proposition 15.** $\mathfrak{a}\longrightarrow y$ if and only if every net $\{s_A\}$, $A\in\mathfrak{a}$, based on $\mathfrak{a}$ also converges to $y$.

_Proof._ Suppose $\mathfrak{a}\longrightarrow y$ and $\{s_A\}$, $A\in\mathfrak{a}$, is a net based on $\mathfrak{a}$. Let $U$ be any neighborhood of $y$; since $\mathfrak{a}\longrightarrow y$, $U\in\mathfrak{a}$. Then if $U\leq A$, $A\subset U$, for any $A\in\mathfrak{a}$. Hence if $U\leq A$, $s_A\in A\subset U$; therefore $\{s_A\}$, $A\in\mathfrak{a}$, is residually in $U$. Thus $s_A\longrightarrow y$.

Suppose $\mathfrak{a}\not\longrightarrow y$. Then there is a neighborhood $U$ of $y$ which is not a member of $\mathfrak{a}$. If $A\in\mathfrak{a}$, select $s_A\in A-U$; such a selection is always possible, for if $A-U=\phi$, then $A\subset U$, which would make $U$ a member of $\mathfrak{a}$. Then $\{s_A\}$, $A\in\mathfrak{a}$, is a net based on $\mathfrak{a}$ which does not converge to $y$.

**Proposition 16.** Suppose $\mathfrak{a}$ is a filter on a space $X,\tau$. Then $x$ is a limit point of $\mathfrak{a}$ if and only if there is a filter $\mathfrak{a}'$ such that $\mathfrak{a}\subset\mathfrak{a}'$ and $\mathfrak{a}'\longrightarrow x$.

_Proof._ Suppose $x$ is a limit point of $\mathfrak{a}$. Let

$$
\mathfrak{D}'=\{A\cap U\mid A\in\mathfrak{a}\text{ and }U\text{ is a neighborhood of }x\}.
$$

Then $\mathfrak{D}'$ is the basis for a filter $\mathfrak{a}'$ on $X$ (Exercise 3). Since $A\cap U\subset U$ for each $A\in\mathfrak{a}$ and any neighborhood $U$ of $x$, $U\in\mathfrak{a}'$. Therefore every neighborhood of $x$ is a member of $\mathfrak{a}'$, and thus $\mathfrak{a}'\longrightarrow x$. It remains to be shown that $\mathfrak{a}\subset\mathfrak{a}'$. This follows at once from the fact that $X$ is a neighborhood of $x$; hence if $A\in\mathfrak{a}$, $A\cap X=A$ is a member of $\mathfrak{D}'$, and hence of $\mathfrak{a}'$.

Suppose on the other hand that there is a filter $\mathfrak{a}'$ such that $\mathfrak{a}\subset\mathfrak{a}'$ and $\mathfrak{a}'\longrightarrow x$. Let $U$ be any neighborhood of $x$ and $A$ be an element of $\mathfrak{a}$. Then $U$ and $A$ are both members of $\mathfrak{a}'$; hence

$$
A\cap U\in\mathfrak{a}'
$$

by (ii) of Definition 5. Since $A\cap U\in\mathfrak{a}'$, then $A\cap U\ne\phi$. Therefore, given any neighborhood $U$ of $x$, and any member $A$ of $\mathfrak{a}$, $A\cap U\ne\phi$. Hence $x$ is a limit point of $\mathfrak{a}$.

<span id="printed-page-136"></span>

<!-- Source: PDF page 131, printed page 136; section 6.7 fragment. -->

Comparing Proposition 16 with Proposition 9, we find that the filter analog of a subnet is a _finer filter_, where a filter $\mathfrak{a}'$ is _finer_ than a filter $\mathfrak{a}$ if $\mathfrak{a}\subset\mathfrak{a}'$.

## Exercises

1. Prove that $\mathfrak{a}$ in Example 16 is a filter.

2. Prove in Example 17 that there is $B_{j''}\in\mathfrak{D}$ such that $B_{j''}\subset B_j\cap B_{j'}$.

3. In Proposition 16, prove $\mathfrak{D}'$ is a filter basis.

4. Prove (b) in Proposition 14.

5. Suppose $f$ is a function from a space $X,\tau$ into a space $Y,\tau'$ and $\mathfrak{a}$ is a filter on $X$.

   a) Set $f(\mathfrak{a})=\{f(A)\mid A\in\mathfrak{a}\}$. Prove that $f(\mathfrak{a})$ is the basis for a filter $\mathfrak{a}'$ on $Y$.

   b) Prove that $f$ is continuous if and only if given any filter $\mathfrak{a}$ on $X$ such that $\mathfrak{a}\longrightarrow x$, $\mathfrak{a}'$ [the filter for which $f(\mathfrak{a})$ is a basis] $\longrightarrow f(x)$.

6. Suppose $A$ is a subset of $X,\tau$. Prove $x\in\operatorname{Cl}A$ if and only if there is a filter $\mathfrak{a}$ on $X$ such that $\mathfrak{a}\longrightarrow x$ and $A\in\mathfrak{a}$.

7. Prove that a space $X,\tau$ is $T_2$ if and only if any convergent filter on $X$ converges to a unique limit.

8. Which of the following are filters? Which are filter bases? For those which are neither filters nor filter bases, indicate which properties are lacking.

   a) the family of subsets of a set $X$ which contain $Y\subset X$

   b) the set of all closed half-planes in $R^2$ which contain $(0,0)$

   c) the set of all open half-planes of $R^2$

   d) the union of two filters on a set $X$

9. State and prove the filter analogs of Propositions 4 and 5 of this chapter.

10. Find a criterion in terms of filters for a space to be $T_1$.

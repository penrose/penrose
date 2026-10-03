---
title: Bases and Subbases — Elementary Topology
description: Section 3.2, printed pages 43–48, of Elementary Topology, second edition.
---

# 3.2 Bases and Subbases

::: info Transcription note
Source: printed pages 43–48 (PDF pages 48–53). The Gothic letters $\mathfrak{B}$ and $\mathfrak{S}$ denote the collections in the source. The references to Exercise 6 in Example 6 and the unrestricted “rational number” in Exercise 5 are retained as printed.
:::

<span id="printed-page-43"></span>

<!-- source: PDF 48, printed 43; section 3.2 fragment -->

Quite often it is impractical to explicitly specify all the open sets in order to define a topology on some set. Note that when we defined an open subset of a metric space, it was done in terms of $\rho$-neighborhoods and not by listing each open set separately. Proposition 2 has also shown us that we could equally well define a topology by giving the closed sets instead of

<span id="printed-page-44"></span>

<!-- source: PDF 49, printed 44 -->

the open ones. We now investigate other methods of determining a topology on a given set. In the first of these methods, a collection of sets is furnished which “generates” the topology (in much the same way that a basis of a vector space generates the vector space).

**Definition 3.** Suppose that $X,\tau$ is a topological space. A subset $\mathfrak{B}$ of $\tau$ (i.e., a collection of open sets) is said to be a _basis_ for the topology $\tau$ if each member of $\tau$ is the union of members of $\mathfrak{B}$.

There is no analog of linear independence in Definition 3. Any topology has at least one basis, namely itself. Generally it is of no consequence whether or not a basis is in any sense minimal.

**Example 6.** Suppose $R^2$ to be the plane with the Pythagorean metric $D$. Then the $\rho$-neighborhoods of $R^2$ form a basis for the topology induced by $D$ (see Section 3.1, Exercise 3). In fact, if $X,D$ is any metric space, then the $\rho$-neighborhoods of $X$ form a basis for the topology induced by $D$.

Note that if $X,D$ is any metric space $x\in X$ and $\rho>0$, then there is a rational number $q$, $0<q<\rho$, with $N(x,q)\subset N(x,\rho)$. This fact can be used to prove that

$$
\{N(x,q)\mid x\in X,q\text{ a positive rational number}\}
$$

is also a basis for the topology induced by $D$ (Exercise 6). It can also be proved that

$$
\{N(x,1/n)\mid x\in X\text{ and }n\text{ a positive integer}\}
$$

is a basis for the metric topology (Exercise 6). Thus we see that bases for a topology can be quite diverse.

**Proposition 3.** Suppose that $X,\tau$ is a topological space and that $\mathfrak{B}$ is a basis for $\tau$. Then the intersection of any two members of $\mathfrak{B}$ is the union of members of $\mathfrak{B}$, and $X$ itself is the union of members of $\mathfrak{B}$.

_Proof._ Since both $X$ and the intersection of any two members of $\mathfrak{B}$ are members of $\tau$, such sets must be the union of members of $\mathfrak{B}$.

The case often occurs when rather than being given a topology for a set $X$, we are merely given a collection of subsets of $X$. For example, in our study of metric spaces, it was the $\rho$-neighborhoods which arose most naturally; the open sets were defined after the $\rho$-neighborhoods had been introduced. We might, then, reasonably ask, When is a collection of subsets of $X$ the basis for a topology on $X$? The following proposition answers this question.

<span id="printed-page-45"></span>

<!-- source: PDF 50, printed 45 -->

**Proposition 4.** Let $X$ be any set. Assume $\mathfrak{B}$ to be a family of subsets of $X$ such that

i′) $X$ is the union of members of $\mathfrak{B}$;

ii′) the intersection of any two members of $\mathfrak{B}$ is the union of members of $\mathfrak{B}$.

Define $\tau=\{U\subset X\mid U\text{ is the union of members of }\mathfrak{B}\}$. Then $\tau$ is a topology on $X$ and $\mathfrak{B}$ is a basis for $\tau$. (The topology $\tau$ for which $\mathfrak{B}$ is a basis is, in fact, unique; see Exercise 3.)

_Proof._ We must verify that $\tau$ satisfies Definition 1.

i) $X$ is the union of members of $\mathfrak{B}$, by (i′), and $\phi$ is the union of the empty subfamily of $\mathfrak{B}$. Therefore $X$ and $\phi$ are members of $\tau$.

ii) Suppose that $U$ and $V$ are in $\tau$. Then $U=\bigcup_I B_i$ and $V=\bigcup_J B_j$, where $I$ and $J$ are appropriate index sets and where $B_i$ and $B_j$ are members of $\mathfrak{B}$ for each $i\in I$ and $j\in J$. Then

$$
U\cap V=\bigcup_{I,J}(B_i\cap B_j).
$$

Since each $B_i\cap B_j$ is the union of members of $\mathfrak{B}$ by (ii′), $U\cap V$ is the union of members of $\mathfrak{B}$, and hence is in $\tau$. The intersection of any two members of $\tau$ is again a member of $\tau$.

iii) If $\{U_k\}$, $k\in K$, is any family of members of $\tau$, then $U_k$ is the union of members of $\mathfrak{B}$ for each $k\in K$. Therefore $\bigcup_K U_k$ is the union of members of $\mathfrak{B}$, and hence is in $\tau$. That is, the union of any family of members of $\tau$ is again a member of $\tau$. Therefore $\tau$ satisfies the definition of a topology on $X$. Since each member of $\tau$ is by definition the union of members of $\mathfrak{B}$, $\mathfrak{B}$ is a basis for $\tau$.

**Example 7.** Let $R$ be the set of real numbers. Clearly $R$ is the union of open intervals. Since the intersection of any two open intervals in $R$ is either empty or again an open interval, condition (ii′) of Proposition 4 is satisfied by the collection of open intervals in $R$. The family of open intervals in $R$ thus forms the basis for a topology on $R$. This topology is the same as the topology induced on $R$ by the absolute value metric (Exercise 1).

Suppose $X$ to be any set, and $\mathfrak{S}$ any collection of subsets of $X$. Combining Propositions 3 and 4, we see that $\mathfrak{S}$ is the basis for a topology on $X$ if and only if $\mathfrak{S}$ satisfies conditions (i′) and (ii′) of Proposition 4; but not every family of subsets of $X$ satisfies these conditions. We may ask, therefore, in what topologies on $X$ the given sets are open. There is, however, generally no unique topology on $X$ for which the given sets are open.

<span id="printed-page-46"></span>

<!-- source: PDF 51, printed 46 -->

For example, the $\rho$-neighborhoods in $R^2$ with the Pythagorean metric $D$ are open in the topology induced by $D$, but they are also open with respect to the discrete topology. We may therefore rephrase our question by asking, What is the “smallest” topology $\tau$ on $X$, that is, the topology having the “fewest” open sets, for which $\mathfrak{S}\subset\tau$? This question is answered in the following proposition.

**Proposition 5.** Let $X$ be any set and suppose $\mathfrak{S}$ to be a collection of subsets of $X$. Set

$$
\mathfrak{B}=\{B\mid B\text{ is the intersection of finitely many sets in }\mathfrak{S}\text{ or }B=X\}.
$$

Then $\mathfrak{B}$ is the basis for a topology $\tau$ on $X$ defined by

$$
\tau=\{U\mid U\text{ is the union of members of }\mathfrak{B}\}.
$$

Moreover, $\mathfrak{S}\subset\tau$, and if $\tau'$ is any topology on $X$ such that $\mathfrak{S}\subset\tau'$, then $\tau\subset\tau'$; that is, $\tau$ is the smallest topology on $X$ for which $\mathfrak{S}$ is a collection of open sets.

_Proof._ We first show that $\mathfrak{B}$ satisfies (i′) and (ii′) of Proposition 4.

i′) $X$ is itself the union of members of $\mathfrak{B}$, since $X\in\mathfrak{B}$ by hypothesis.

ii′) If $B_1$ and $B_2$ are both in $\mathfrak{B}$, then both $B_1$ and $B_2$ are the intersection of finitely many members of $\mathfrak{S}$. Therefore $B_1\cap B_2$ is itself the intersection of finitely many members of $\mathfrak{S}$, and is hence in $\mathfrak{B}$. The intersection of any two members of $\mathfrak{B}$ is thus again a member of $\mathfrak{B}$. By Proposition 4, then, $\mathfrak{B}$ is the basis for a topology, the topology $\tau$ as defined above, on $X$. Clearly $\mathfrak{S}\subset\tau$.

If $\mathfrak{S}$ is to be a collection of open subsets in any topology on $X$, then all finite intersections of members of $\mathfrak{S}$ must also be open sets (Section 3.1, Exercise 1), and hence any union of a family of these intersections must also be open. Since $\tau$ is the smallest topology on $X$ in which these conditions are fulfilled, it is therefore the smallest topology on $X$ for which $\mathfrak{S}$ is a family of open sets.

Proposition 5 inspires the following definition.

**Definition 4.** Let $X,\tau$ be a topological space. A subset $\mathfrak{S}$ of $\tau$ is said to be a _subbasis_ for $\tau$ if the set

$$
\mathfrak{B}=\{B\mid B\text{ is the intersection of finitely many members of }\mathfrak{S}\}
$$

is a basis for $\tau$.

Proposition 5 tells us that any collection of subsets of $X$ whose union is $X$ is the subbasis for a unique topology on $X$.

<span id="printed-page-47"></span>

<!-- source: PDF 52, printed 47 -->

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-3.1.svg" alt="Figure 3.1: An open interval formed by the intersection of two open rays." /><figcaption>Figure 3.1</figcaption></figure>

**Example 8.** We saw in Example 7 that the family of open intervals in the set $R$ of real numbers is the basis for a topology on $R$. Each open interval (Fig. 3.1) is, however, the intersection of two half-lines (rays) without endpoints, and each such half-line is open in the topology determined by the open intervals. Thus the set of all half-lines without endpoints forms a subbasis for the open-interval topology.

**Example 9.** Let $R^2$ be the coordinate plane with metric $D_3$ as in Chapter 2, Examples 3 and 6. As in any metric space, the $D_3$-$\rho$-neighborhoods in $R^2$ form a basis for the topology induced on $R^2$ by $D_3$. Note that each $D_3$-$\rho$ neighborhood of $R^2$ is the intersection of finitely many open half-planes (Fig. 3.2), and that each of these half-planes is open in the topology induced by $D_3$. The collection of open half-planes is therefore a _subbasis_ for this topology.

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-3.2.svg" alt="Figure 3.2: An open square formed by intersecting four open half-planes." /><figcaption>Figure 3.2</figcaption></figure>

Thus far we have four means of specifying a topology on a set $X$: (1) by explicitly giving the open sets, that is, the members of the topology; (2) by explicitly giving the closed sets; (3) by giving a basis for the topology; or (4) by giving a subbasis for the topology.

## Exercises

1. Prove that the topology on $R$, the set of real numbers, for which the collection of open intervals is a basis is the same as the topology induced on $R$ by the absolute value metric.
2. Let $R^2$ be the coordinate plane and let $D,D_1,D_2$, and $D_3$ be the metrics described in Chapter 2, Examples 2 and 3.

<span id="printed-page-48"></span>

<!-- source: PDF 53, printed 48 -->

a) Prove that the collection of open half-planes is a subbasis for the topologies induced by $D$ and $D_1$.

b) Prove that the collection of closed half-planes (that is, a half-plane and its bounding line) is a subbasis for the topology induced by $D_2$.

c) Suppose that $X,\tau$ is a topological space and $\mathfrak{S}$ a subbasis for $\tau$. Prove that $\tau$ is the only possible topology on $X$ for which $\mathfrak{S}$ is a subbasis; that is, if $\tau'$ is a topology on $X$ for which $\mathfrak{S}$ is also a subbasis, then $\tau=\tau'$.

d) Using (c), prove that the topologies induced by $D,D_1$ and $D_3$ on $R^2$ are equal.

3. Let $X$ be any set. Suppose a collection $\mathfrak{B}$ of subsets of $X$ is the basis for topologies $\tau$ and $\tau'$ on $X$. Prove $\tau=\tau'$. Thus any collection of subsets of $X$ which satisfies (i′) and (ii′) of Proposition 4 is a basis for one and only one topology on $X$.
4. Prove that each of the following are bases for topologies on the prescribed sets.

   a) the set of intervals of the form $[a,b)$ in the set of real numbers

   b) $X=\{f\mid f\text{ is a function from }[0,1]\text{ into }[0,1]\}$ and the collection of subsets of $X$ of the form

   $$
   B_S=\{f\in X\mid f(x)=0\text{ for }x\in S\},
   $$

   where $S$ is some subset of $[0,1]$

   c) $X=\{p\mid p\text{ is a polynomial with real coefficients}\}$ and the collection of subsets of $X$ of the form

   $$
   B_n=\{p\in X\mid\text{degree of }p=n\},
   $$

   where $n$ is a nonnegative integer

5. Prove that

   $$
   \{N(x,q)\mid q\text{ is a rational number},x\in X\}
   $$

   and

   $$
   \{N(x,1/n)\mid x\in X\text{ and }n\text{ a positive integer}\}
   $$

   are both bases for the topology induced on $X$ by $D$ as is claimed in Example 6.

6. Let $N$ be the set of positive integers. Find explicitly all the open sets in the smallest topologies on $N$ for which each of the following is a collection of open sets.

   a) $N$ and $\phi$

   b) $N,\{1,2\},\{3,4,5\}$

   c) $N,\{1,2\},\{3,4,5\},\{1,4,7\}$

7. Let $X,D$ be a metric space. For each $x,y\in X$, define $H_1(x,y)$ to be $\{w\in X\mid D(x,w)>D(y,w)\}$ and $H_2(x,y)=\{w\in X\mid D(x,w)<D(y,w)\}$.

   a) Prove that $H_1(x,y)$ and $H_2(x,y)$ are open with respect to $D$.

   b) Describe these sets relative to two distinct points of $R^2$ with the Pythagorean metric.

   c) Prove or disprove: $\{H_1(x,y)\mid(x,y)\in X\times X,x\neq y\}$ is a subbasis for the topology induced on $X$ by $D$.

[Continue to 3.3 Open Neighborhood Systems](./open-neighborhood-systems)

<style>
.topology-chapter-figure { max-width: 28rem; margin: 1.75rem auto; text-align: center; }
.topology-chapter-figure img { width: 100%; background: white; }
.topology-chapter-figure figcaption { font-family: Georgia, serif; font-size: 0.875rem; margin-top: 0.4rem; }
</style>

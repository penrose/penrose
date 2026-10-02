---
title: Compactifications — Elementary Topology
---

# 8.3 Compactifications

<span id="printed-page-174"></span>

<!-- Source: PDF165, printed174, Section8.3 fragment. -->

Compact spaces are perhaps the most important of all topological spaces. It is therefore of interest to know if and how any given space can be embedded as a subspace of a compact space. If any space $X,\tau$ can be embedded as a subspace $W$ of a compact space $Y,\tau'$, then $X$ can be embedded as a dense subspace of some compact space. For $W$ is a dense subspace of $\operatorname{Cl}W$; this follows from Propositions 13 and 14 of Chapter 3. But $\operatorname{Cl}W$ is a closed subset of a compact space and hence is compact. We therefore restrict our attention to considering whether or not a given space can be embedded as a dense subspace of a compact space. Accordingly, we make the following definition.

::: warning Missing printed page 175
Printed page 175 is absent. The promised definition and any intervening definitions, construction, and Example 7 are unavailable. Printed page 176 resumes at Proposition 11. Its references to Definitions 3 and 4 are retained without reconstructing those definitions.
:::

<span id="printed-page-176"></span>

<!-- Source: PDF166, printed176. -->

**Proposition 11.** Let $X,\tau$ be any $T_2$-space. Then the Alexandroff compactification $Y$ of $X$ is a topological space and is a compactification of $X$ in the sense of Definition 3.

_Proof._ If $X$ is already compact, the proposition is trivial. Suppose $X$ is not compact. If $y\in Y$, set

$$
\mathfrak{N}_y=\{U\mid U\text{ is open in }Y\text{ and }y\in U\},
$$

that is, $\mathfrak{N}_y$ is the family of all neighborhoods of $y$. We will show that the collection of $\mathfrak{N}_y$ forms an open neighborhood system for $\tau'$, a topology on $Y$ in which the open sets are those described in Definition 4. Since, if $y\in X$, the neighborhoods of $y$ in $Y$ are the same as the neighborhoods of $y$ in $X$, we need only consider the case when $y=P$, the ideal point. We now verify Definition 5 of Chapter 3 for $\mathfrak{N}_P$.

i) Since any one-point subset $\{x\}$ of $X$ is compact, $X-\{x\}$ is a neighborhood of $P$; therefore $\mathfrak{N}_P\ne\phi$.

ii) By assumption, $P\in U$ for each $U\in\mathfrak{N}_P$.

iii) Suppose $U$ and $U'$ are neighborhoods of $P$. Then $Y-U=K$ and $Y-U'=K'$, where $K$ and $K'$ are compact subsets of $X$. Then

$$
(Y-U)\cup(Y-U')=Y-(U\cap U')=K\cup K'.
$$

But $K\cup K'$ is compact since it is the union of two compact sets (Section 7.3, Exercise 8). Therefore $U\cap U'$ is a neighborhood of $P$.

iv) Suppose $U$ is any neighborhood of $P$ and $z\in U$. If $z=P$, then $z\in U\in\mathfrak{N}_z$ and $U\subset U$. If $z\in X$, then $U-\{P\}$ is an open subset of $X$; for $X-U$ is compact, and $X$ is $T_2$. Therefore $X-U$ is closed; hence

$$
X-(X-U)=U-\{P\}
$$

is open in $X$, and hence also in $Y$. Therefore

$$
z\in U-\{P\}\subset U\qquad\text{and}\qquad U-\{P\}\in\mathfrak{N}_z.
$$

The verification of (v) is left as a simple exercise. Therefore the collection of $\mathfrak{N}_x$ forms an open neighborhood system for a topology $\tau'$ on $Y$ which is precisely the family of open sets defined for $Y$.

Since every neighborhood of any point in $Y$ meets $X$, $X$ is dense in $Y$. It remains to be shown that $Y$ is compact. Let $\{U_i\}$, $i\in I$, be any open cover of $Y$. Then $P\in U_i$ for some $i$, say $i'$. Since $U_{i'}$ is a neighborhood of $P$, $Y-U_{i'}$ is a compact subset of $X$ and $\{U_i\}$, $i\in I$, is an open cover of $Y-U_{i'}$. Then finitely many of the $U_i$, say $U_{i_1},\ldots,U_{i_n}$, cover <span id="printed-page-177"></span><!-- Source: PDF167, printed177. -->$Y-U_{i'}$; hence

$$
\{U_{i'},U_{i_1},\ldots,U_{i_n}\}
$$

is a finite subcover of $\{U_i\}$, $i\in I$. Therefore $Y$ is compact.

**Example 8.** The one-point compactification of $(0,1)$ as in Example 7 is the circle. Note that $(0,1)$ also has a two-point compactification $[0,1]$. The one-point compactification of the space $R$ of real numbers (with the usual topology) is again a circle, since $R$ is homeomorphic to $(0,1)$. It is given as an exercise to prove that homeomorphic spaces have homeomorphic one-point compactifications. Usually the ideal point for the space of real numbers is taken to be $\infty$.

**Example 9.** If $X$ is an infinite set with the discrete topology and $P$ is an ideal point for the one-point compactification $Y$ of $X$, then the neighborhoods of $P$ will be all subsets of $Y$ which contain all but finitely many points of $X$, since the finite subsets of $X$ are the only compact subsets of $X$.

Note that $X$ is always an open subset of its one-point compactification, since $X$ is open in $X$. This implies that the subset containing only the ideal point of any one-point compactification is closed. We thus see that if $X$ is $T_2$, then its one-point compactification is at least $T_1$. We now investigate conditions under which a one-point compactification is $T_2$.

**Proposition 12.** The Alexandroff compactification of any space $X,\tau$ is $T_2$ if and only if $X$ is $T_2$ and locally compact.

_Proof._ Suppose $X$ is compact. Then the Alexandroff compactification of $X$ is $X$ itself. Now $X$ is $T_2$ if $X$ is $T_2$ and locally compact; on the other hand, if $X$ is $T_2$, then $X$ is $T_2$ and is also locally compact by the corollary to Proposition 5. Assume that $X$ is not compact, and let $Y$ be the Alexandroff compactification of $X$. If $Y$ is $T_2$, then $X$ is $T_2$, since $X$ is a subspace of $Y$. Now $Y$ is $T_2$ and compact and is therefore locally compact. But $X$ is an open subspace of $Y$; hence $X$ is locally compact (Proposition 7).

On the other hand, suppose $X$ is $T_2$ and locally compact. Let $x$ and $y$ be distinct points of $Y$. If $x$ and $y$ are both in $X$, then since $X$ is $T_2$, there are neighborhoods $U$ and $V$ of $x$ and $y$, respectively, such that $U\cap V=\phi$. Suppose $x=P$. Then $y\in X$, and hence there is a compact subset $A$ of $X$ such that $y\in A^\circ\subset A$. We have then that $Y-A$ is a neighborhood of $x=P$ and that $A^\circ$ is a neighborhood of $y$ with

$$
A^\circ\cap(Y-A)=\phi.
$$

Therefore $Y$ is $T_2$.

**Corollary 1.** If $X,\tau$ is a locally compact $T_2$-space, then the one-point compactification of $X$ is normal.

<span id="printed-page-178"></span>

<!-- Source: PDF168, printed178. -->

_Proof._ Any compact $T_2$-space is both $T_1$ and $T_4$ (Corollary 3, Proposition 11, Chapter 7).

**Corollary 2.** Any locally compact $T_2$-space $X,\tau$ is regular.

_Proof._ The one-point compactification of $X$ is normal, and hence is also regular. Since $X$ is a subspace of a regular space, $X$ is regular.

Note that we had already proved Corollary 2 previously (Proposition 6), but that the use of compactifications gives a simple, elegant proof for the result. Note too that since we have found examples of $T_2$-spaces which are not locally compact (e.g., as in Example 5), we therefore have the one-point compactifications of such spaces as examples of compactifications of $T_2$-spaces which are not $T_2$.

::: info Source notation
Proposition 11(i) prints $X-\{x\}$ as a neighborhood of the ideal point, and the paragraph following (iv) uses $\mathfrak{N}_x$; these readings remain unchanged. The exercise numbering below jumps from 4 to 6 on the same available printed page. Exercise 6 uses $f(Z)$ and Exercise 7 uses $f(P)$ as printed.
:::

## Exercises

1. Verify that the circle is the one-point compactification of $(0,1)$ (Example 8).
2. Prove that if a $T_2$-space $X,\tau$ is homeomorphic to a space $X',\tau'$, then the one-point compactifications of these spaces are homeomorphic. Show that two spaces might have homeomorphic one-point compactifications even though the spaces are not homeomorphic to one another.
3. Describe the one-point compactifications of each of the following subspaces of $R^2$ with the usual Pythagorean topology. Where practicable, sketch the compactification.

   a) $\{(x,y)\mid x\in(0,1],\ y=0\}$

   b) $\{(1/n,1/n)\mid n=1,2,3,\ldots\}$

   c) $\{(x,y)\mid x^2+y^2<1\}$

   d) $\{(x,y)\mid x^2+y^2<1\}\cup\{(0,1)\}$

   e) $\{(x,y)\mid-1\le x\le1\}$

4. Which of the following are compactifications of $\{(x,y)\mid x^2+y^2<1\}$ with the Pythagorean topology? Each of the following spaces is to be considered as a subspace of Euclidean $n$-space $R^n$ for an appropriate $n$.

   a) $\{(x,y,z)\mid x^2+y^2+z^2=1\}$

   b) $\{(x,y,z)\mid x^2+y^2=1,\ 0\le z\le1\}$

   c) $\{(x,y)\mid x^2+y^2\le1\}$

   d) $\{(x,y,z)\mid x^2+y^2+z^2\le1\}$

   e) $\{(x,y)\mid |x|\le1,\ |y|\le3\}$

<!-- The source skips Exercise5; a separate list preserves the printed numbering. -->

6. Suppose $f$ is a continuous function from a $T_2$-space $X,\tau$ into a $T_2$-space $Y,\tau'$. Can $f$ necessarily be extended to a continuous function from the one-point compactification of $X$ into $Y$? Suppose $f$ can be extended to a continuous function $F$ from $Z$, the one-point compactification of $X$, into $Y$. Is $f(Z)$ necessarily a compactification of $f(X)$? Is $f(Z)$ necessarily the one-point compactification of $f(X)$ if it is a compactification?
7. Suppose $f$ is a continuous function from $X,\tau$ onto $Y,\tau'$, and let $X'$ and $Y'$ be the one-point compactifications of $X$ and $Y$, respectively. Define $F:X'\to Y'$ by $F(x)=f(x)$ if $x\in X$, and $f(P)=P'$, where $P$ and $P'$ are the ideal points of $X$ and $Y$, respectively. Is $F$ necessarily continuous?

<span id="printed-page-179"></span>

<!-- Source: PDF169, printed179, Section8.3 fragment. -->

8. a) Describe the _two-point compactification_ of the open interval $(0,1)$ with its usual topology. How can this two-point compactification be defined rigorously, that is, constructed from $(0,1)$.

   b) Sketch the one-, two-, three-, and four-point compactifications of $(0,1)\cup(2,3)\cup(4,5)$.

[Chapter 8 contents](./index.md) · [Next: 8.4 Sequential and Countable Compactness](./sequential-and-countable-compactness.md)

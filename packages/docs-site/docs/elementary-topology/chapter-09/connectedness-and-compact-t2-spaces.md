---
title: Connectedness and Compact T₂-Spaces — Elementary Topology
---

# 9.5 Connectedness and Compact $T_2$-Spaces

<span id="printed-page-202"></span>

<!-- Source: PDF189, printed202, Section9.5 fragment. -->

Two of the most important properties in topology are compactness and being $T_2$; hence a compact $T_2$-space is doubly important. In this section we study the properties of compact $T_2$-spaces with regard to connectedness and local connectedness. We first introduce some pertinent definitions.

**Definition 5.** A space $X,\tau$ can be _split_ between two of its points $x$ and $y$ if there are disjoint open subsets $U$ and $V$ of $X$ such that $x\in U$, $y\in V$, and $X=U\cup V$.

A compact connected $T_2$-space $X,\tau$ is called a _continuum_. The continuum $X$ is said to be _irreducible about_ $A\subset X$ if $X$ is a minimal continuum which contains $A$.

A point $x$ of a space $X,\tau$ (not necessarily a continuum) is said to be a _cut point_ of $X$ if $X$ is connected but $X-\{x\}$ is not connected. Otherwise, $x$ is said to be a _noncut point_. See Section 9.3, Exercise 4.

**Example 17.** Evidently if a space $X,\tau$ can be split between two points $x$ and $y$ then $x$ and $y$ are in different components of $X$. It is not true, however, that if $x$ and $y$ are in different components of $X$ that $X$ can necessarily be split between them. Let

$$
Y=\{C_n\}\cup\{(x,y)\mid y=1,\text{ or }y=-1\}\subset R^2
$$

<span id="printed-page-203"></span>

<!-- Source: PDF190, printed203. -->

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-9.11.svg" alt="Nested concentric circles between two horizontal lines through (0,1) and (0,-1), on coordinate axes; a connected example with distinct components." />
<figcaption>Figure 9.11. <a href="/docs/elementary-topology/reader?page=203">View the interactive figure and its Substance program.</a></figcaption>
</figure>

(with the usual topology), where $C_n=\{(x,y)\mid x^2+y^2=(1-(1/n))^2\}$, $n=1,2,3,\ldots$. Then the components of $Y$ are each $C_n$ and the two straight lines indicated in Fig. 9.11. It should be intuitively evident that although $(0,1)$ and $(0,-1)$ are in different components of $Y$, nevertheless $Y$ cannot be split between these two points.

**Example 18.** Any closed, bounded, and connected subset of $R^n$ for any $n$ is a continuum. Any path in a $T_2$-space is a continuum, since any subspace of a $T_2$-space is $T_2$ and compactness and connectedness are both preserved by continuous functions.

Every point of the closed interval $[0,1]$ except $0$ or $1$ is a cut point of $[0,1]$; thus $\{0,1\}$ is the set of noncut points of $[0,1]$. It is easy to show in fact that $[0,1]$ is a continuum which is irreducible about $\{0,1\}$. Similarly, if $P$ and $Q$ are any two points of $R^n$, then the closed segment $\overline{PQ}$ is a continuum irreducible about $\{P,Q\}$.

**Proposition 18.** Let $X,\tau$ be a compact $T_2$-space and $\{A_i\}$, $i\in I$, be a family of closed subsets of $X$ such that $\{A_i\}$, $i\in I$, is directed by $\le$ where $A_i\le A_j$ if $A_j\subset A_i$ (cf. Definition 1 and Example 3 of Chapter 6). Suppose that each $A_i$ has the property that it cannot be split between $x$ and $y$. Then $\bigcap_I A_i$ cannot be split between $x$ and $y$.

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-9.12.svg" alt="Two separated inner regions U and V and surrounding regions G and H inside a common outer region, with labeled points a and b." />
<figcaption>Figure 9.12. <a href="/docs/elementary-topology/reader?page=203">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<span id="printed-page-204"></span>

<!-- Source: PDF191, printed204. -->

_Proof._ Let $B=\bigcap_I A_i$. Suppose $B=U\cup V$, where $U$ and $V$ are disjoint, open (in $B$) subsets of $B$, and $x\in U$ and $y\in V$. Now $U$ and $V$ are also closed in $B$. But $B$ is closed in $X$ (Exercise 1); hence $U$ and $V$ are closed in $X$. Since $X$ is compact and $T_2$, $X$ is $T_4$, and thus there are open sets $G$ and $H$ such that $U\subset G$, $V\subset H$, and $G\cap H=\phi$ (Fig. 9.12).

For each $i\in I$, $A_i\not\subset G\cup H$, or otherwise we would have

$$
x\in A_i\cap G,\quad y\in A_i\cap H,\quad\text{and}\quad(A_i\cap G)\cap(A_i\cap H)=\phi;
$$

that is, we could split $A_i$ between $x$ and $y$. For each $A_i$, we can therefore find $x_i\in A_i-(G\cup H)$. Since $\{A_i\}$, $i\in I$, is by assumption a directed set, $\{x_i\}$, $i\in I$, is a net. Since $X$ is compact, this net has a limit point $z$ (Proposition 9, Chapter 7). Since any neighborhood $N$ of $z$ contains $\{x_i\}$, $i\in I$, cofinally, any neighborhood $N$ of $z$ meets $\{A_i\}$, $i\in I$, cofinally. But then $N$ meets every $A_i$. For given $A_i$, there is $A_j\subset A_i$ such that $N\cap A_j\ne\phi$ (remember that $\{A_i\}$, $i\in I$, is directed); hence $N\cap A_i\ne\phi$. Therefore

$$
z\in\operatorname{Cl}A_i=A_i,
$$

for each $i$, and thus $z\in B=\bigcap_I A_i$. We therefore have that $z\in G\cup H$; hence $G\cup H$ is a neighborhood of $z$. But $G\cup H$ does not contain any of the $x_i$, contradicting the choice of $z$ as a limit point of $\{x_i\}$, $i\in I$. It must be then that $B$ cannot be split between $x$ and $y$.

We have already seen that in a general topological space, the intersection of a family of connected subsets need not be connected. This is not true even in a compact $T_2$-space, but the following is true.

**Proposition 19.** Suppose $\{A_i\}$, $i\in I$, is a family of closed connected subsets of a compact $T_2$-space and $\{A_i\}$, $i\in I$, directed by $\le$ as in Proposition 18. Then $\bigcap_I A_i$ is connected.

_Proof._ If $x$ and $y$ are in $\bigcap_I A_i$, then each $A_i$ cannot be split between $x$ and $y$; therefore, by Proposition 18, $\bigcap_I A_i$ cannot be split between $x$ and $y$. Proposition 19 then follows at once from the following proposition.

**Proposition 20.** If $X,\tau$ is a compact $T_2$-space and $x$ and $y$ are in $X$, then $X$ cannot be split between $x$ and $y$ if and only if $x$ and $y$ are in the same component of $X$.

_Proof._ Let $C_x$ be the component of $x$ in $X$ and $Q_x=\{z\mid X\text{ cannot be split between }x\text{ and }z\}$. We already know that $C_x\subset Q_x$. Suppose $y\in Q_x$. We must show that $y\in C_x$. Consider the family $\{A_i\}$, $i\in I$, of closed subsets of $X$ such that $A_i$ cannot be split between $x$ and $y$. This family in nonempty, since $Q_x$ is a member. Then if $\{A_{i_j}\}$, $j\in J$, is a chain in $\{A_i\}$, $i\in I$, by Proposition 18, $\bigcap_J A_{i_j}$ cannot be split between $x$ and $y$. Apply<span id="printed-page-205"></span><!-- Source: PDF192, printed205. -->ing Zorn’s lemma to the directed (and hence partially ordered) set $\{A_i\}$, $i\in I$, we can find a minimal closed subset $B$ which cannot be split between $x$ and $y$. We will now show that $B$ is connected.

Suppose $B$ is not connected. Then $B=U\cup V$, where $U$ and $V$ are open and closed in $B$ and nonempty, but $U\cap V=\phi$. If $x\in U$ and $y\in V$, then $B$ splits between $x$ and $y$, a contradiction. On the other hand, if $x$ and $y$ are in $U$ (or $V$) and $U$ could be split between $x$ and $y$, then $B$ could also be split between $x$ and $y$; if $U$ could not be split between $x$ and $y$, then $B$ would not be minimal. Therefore $B$ must be connected. But then $B\subset C_x$; hence $y\in C_x$. Then $C_x=Q_x$.

**Proposition 21.** If $X,\tau$ is a continuum and $A\subset X$, then there is a subcontinuum $Y$ of $X$ which is irreducible about $A$. (As the name indicates, $Y$ is a _subcontinuum_ of $X$ if the subspace $Y$ is itself a continuum.)

The proof is left as an exercise.

**Example 19.** If $P$ and $Q$ are any two distinct points in $R^n$, then

$$
\{P,Q\}\subset\operatorname{Cl}N(P,p)
$$

for a suitable $p>0$. Now $\operatorname{Cl}N(P,p)$ is a continuum. There is therefore a minimal (or irreducible) subcontinuum of $\operatorname{Cl}N(P,p)$ about $\{P,Q\}$. The closed segment $\overline{PQ}$ is an example in this case of such an irreducible subcontinuum.

**Proposition 22.** A compact $T_2$-space $X,\tau$ is locally connected if and only if every open cover of $X$ has a refinement consisting of a finite number of connected sets.

_Proof._ Suppose $X$ is locally connected and $\{U_i\}$, $i\in I$, is an open cover of $X$. Let $\{V_j\}$, $j\in J$, be the family of components of the $U_i$. Since $X$ is locally connected, $\{V_j\}$, $j\in J$, is an open cover of $X$ (Proposition 15); moreover, $\{V_j\}$, $j\in J$, is a refinement of $\{U_i\}$, $i\in I$. Since $X$ is compact, there is a finite subcover $\{V_{j_1},\ldots,V_{j_n}\}$ of $\{V_j\}$, $j\in J$, and this finite subcover is a refinement of $\{U_i\}$, $i\in I$, by connected sets.

Conversely, suppose that every open cover of $X$ has a finite refinement by connected sets. Suppose $U$ is a neighborhood of $x\in X$. In order to show that $X$ is locally connected, we must find a connected neighborhood $V$ of $x$ with $V\subset U$. Since $X$ is $T_2$ and compact, $X$ is $T_3$; hence there is a neighborhood $W$ of $x$ such that $W\subset\operatorname{Cl}W\subset U$. The set $\{U,X-\operatorname{Cl}W\}$ is an open cover of $X$. There is therefore a refinement $\{H_1,\ldots,H_n\}$ of $\{U,X-\operatorname{Cl}W\}$ by open, connected subsets of $X$. Either

$$
H_i\subset U\qquad\text{or}\qquad H_i\subset X-\operatorname{Cl}W
$$

<span id="printed-page-206"></span>

<!-- Source: PDF193, printed206. -->

for $i=1,\ldots,n$ by the definition of a refinement. Let $x\in H_i$. Then $x\in H_i\subset U$. Therefore $H_i$ is a connected neighborhood of $x$ which is contained in $U$. Hence $X$ is locally connected.

A path is one of the most important types of continua but, as has already been pointed out, paths can be rather peculiar. The purpose of the next proposition is to help establish certain properties of paths. We will then give without proof a proposition which completely characterizes paths.

**Proposition 23.** If $X,\tau$ is a compact $T_2$-space and if $f$ is a continuous function from $X$ onto a $T_2$-space $Y$, then if $X$ is locally connected, $Y$ is also.

The proof of this proposition is outlined in Exercise 4 below. As an immediate consequence of Proposition 23, we have the following.

**Corollary 1.** Any path in a $T_2$-space is compact, connected, and locally connected.

**Example 20.** The space $Y'$ in Example 16 is connected and compact, but is not locally connected, and hence could not be a path.

It can also be proved without much trouble (see Exercise 4) that any path is also second countable; hence, as we shall prove in the next chapter, any path in a $T_2$-space is a compact, connected, locally connected metric space. As a matter of fact, although we will not prove it in this text, the following is also true.

**Proposition 24** (_the Hahn-Mazurkiewicz theorem_). A metric space is compact, connected, and locally connected if and only if it is a path.

This is a somewhat surprising result, since it implies that even spheres, cubes, etc. and their counterparts in $R^n$ for any $n$, are continuous images of $[0,1]$. As a rule, the continuous functions from $[0,1]$ onto such spaces are not one-one; for if they were, they would be homeomorphisms (Proposition 14, Chapter 7), which, of course, is not generally true.

::: info Source notation and missing reference
Example 17 prints $\{C_n\}$ before adjoining the two lines. Proposition 20 prints “This family in nonempty”; the visible typo is retained. Example 20 refers to Example 16, whose text is unavailable in the missing printed page 199 and is not reconstructed.
:::

## Exercises

1. Prove that the set $B$ in the proof of Proposition 18 is closed. [*Hint:* The proof is extremely simple.]
2. Let $X,\tau$ be any space. Define a relation $E$ on $X$ by $xEy$ if $X$ cannot be split between $x$ and $y$. Prove that $E$ is an equivalence relation on $X$. An $E$-equivalence class is said to be a _quasi-component_. Let $x\in X$; denote the component of $x$ by $C_x$ and the quasi-component of $x$ by $Q_x$. Proposition 20 states that in a compact $T_2$-space $C_x=Q_x$. Always $C_x\subset Q_x$. Find an example of a space in which $C_x\ne Q_x$.

<span id="printed-page-207"></span>

<!-- Source: PDF194, printed207. -->

3. In Example 17, prove that $Y$ cannot be split between $(0,1)$ and $(0,-1)$.
4. a) Prove that any continuous function from a compact $T_2$-space onto a $T_2$-space is _closed_, that is, $f(F)$ is closed if $F$ is closed.

   b) Prove that local connectedness is preserved by continuous closed functions.

   c) Use (b) to prove Proposition 23.

   d) Use (a) to show that any path $Y,\tau'$ in a $T_2$-space is second countable. [_Hint:_ Let $\{U_n\}$, $n\in N$, be a countable basis for the usual metric topology on $[0,1]$, which is known to be second countable. Then $[0,1]-U_n$ is closed for each $n$. Prove that $\{Y-f([0,1]-U_n)\}$, $n\in N$, is a basis for $\tau'$.]

5. Prove Proposition 21.
6. Prove that any continuum is irreducible about its set of noncut points.
7. Find an example of a collection $\{A_i\}$, $i\in I$, of closed, connected subsets of a space $X,\tau$ such that $\{A_i\}$, $i\in I$, is directed by $\le$ as in Proposition 18, but $\bigcap_I A_i$ is not connected.
8. Find an example of a continuum in $R^2$ (with the usual topology) which is irreducible about each of the following subsets of $R^2$.

   a) $\{(0,0),(1,1),(0,1)\}$

   b) $\{(x,y)\mid x\text{ and }y\text{ are rational and }x^2+y^2<1\}$

   c) $\{(x,y)\mid x=0;\ 1<y<2\}$

   Even before they were found, how could we be certain that such continua existed?

9. Suppose that $X$ is a compact $T_2$-space which is irreducibly connected about two points $a$ and $b$. Prove that if $A$ and $B$ are connected subsets of $X$ each of which contains $a$, then $A\subset B$ or $B\subset A$.

[Chapter 9 contents](./index.md)

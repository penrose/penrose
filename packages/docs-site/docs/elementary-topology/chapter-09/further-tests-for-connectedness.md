---
title: Further Tests for Connectedness — Elementary Topology
---

# 9.2 Further Tests for Connectedness

<span id="printed-page-187"></span>

<!-- Source: PDF176, printed187, Section9.2 fragment. -->

In this section we continue to investigate criteria for determining if a space is connected.

If the reader did Exercise 5 of Section 9.1 and his example for (b) was correct, it must have been that $A\cap B=\phi$, as we see from the next proposition.

**Proposition 5.** If $X,\tau$ is a space and $X=\bigcup_I A_i$, where $\{A_i\}$, $i\in I$, is a collection of connected subspaces of $X$, then if $\bigcap_I A_i\ne\phi$, $X$ itself is connected.

_Proof._ Suppose $X=U\cup V$, where $U$ and $V$ are disjoint open subsets of $X$. Then for each $i$, either $A_i\subset U$ or $A_i\subset V$ (Proposition 4). If some $A_i\subset U$, then since $\bigcap_I A_i\ne\phi$, some element from each $A_i$ must be in $U$, and hence every $A_i$ is in $U$. Therefore we would have $\bigcup_I A_i=X\subset U$ and $V=\phi$. Similarly, if some $A_i\subset V$, then $X\subset V$ and $U=\phi$. We have <span id="printed-page-188"></span><!-- Source: PDF177, printed188. -->therefore shown that $X$ could not be expressed as the union of two disjoint, nonempty, open subsets; hence $X$ is connected.

**Example 5.** We have seen that the space $R$ of real numbers with the absolute value topology is connected (Section 9.1, Exercise 3). Any straight line in Euclidean $n$-space is homeomorphic to the real line $R$. This implies that $R^n$ is connected for any $n$, since $R^n$ is the union of all straight lines in $R^n$ which pass through the origin. The family of such lines therefore fulfills the hypotheses of Proposition 5.

**Proposition 6.** Let $X,\tau$ be a space such that any two elements $x$ and $y$ of $X$ are contained in some connected subspace of $X$. Then $X$ is connected.

_Proof._ Let $x$ be a fixed element of $X$. For any $y\in X$, let $C(x,y)$ be a connected subspace of $X$ which contains $x$ and $y$. Then $\{C(x,y)\}$, $y\in X$, is a family of connected subspaces of $X$ whose union is $X$ and whose intersection is nonempty (since the intersection at least contains $x$). Proposition 5 tells us that $X$ is connected.

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-9.2.svg" alt="Coordinate axes with points Q, Q prime, Q double prime, and an excluded point P; a two-segment polygonal route avoids P." />
<figcaption>Figure 9.2. <a href="/docs/elementary-topology/reader?page=188">View the interactive figure and its Substance program.</a></figcaption>
</figure>

**Example 6.** Any closed line segment in Euclidean $n$-space $R^n$ is homeomorphic to the closed interval $[0,1]$, and is hence a connected subspace of $R^n$. Using this fact, we can show that $R^n-\{P\}$, where $P$ is any point of $R^n$ and $2\le n$, is connected. For suppose $Q$ and $Q'$ are any two points of $R^n-\{P\}$. Choose

$$
Q''\in R^n-\{P\}
$$

such that $P\notin\overline{QQ''}\cup\overline{Q''Q'}$ (Fig. 9.2). Then $\overline{QQ''}\cup\overline{Q''Q'}$ is connected by Proposition 5, that is, it is the union of the connected subspaces

$$
\overline{QQ''},\qquad\overline{Q''Q'},
$$

and

$$
\overline{QQ''}\cap\overline{Q''Q'}=\{Q''\}\ne\phi.
$$

Therefore $Q$ and $Q'$ are in the same connected subspace of $R^n-\{P\}$. By Proposition 6, then $R^n-\{P\}$ is connected.

<span id="printed-page-189"></span>

<!-- Source: PDF178, printed189. -->

We have already seen that since $[0,1]$ is connected, any homeomorphic image of $[0,1]$, for example, a closed line segment in $R^n$, is connected. More generally, of course, any continuous image of $[0,1]$ is connected. The continuous images of $[0,1]$ form an important class of spaces known as _paths_. More formally, we make the following definition.

**Definition 2.** Let $X,\tau$ be any space. A subspace $Y$ of $X$ is said to be a _path_ in $X$ if there is a continuous function from $[0,1]$ (with the absolute value topology) onto $Y$. $X$ is said to be _path connected_ if, given any two points $x$ and $y$ in $X$, there is a path in $X$ containing $x$ and $y$.

Suppose $X=R^m$, Euclidean $m$-space. A subset $W$ of $R^m$ is said to be _polygonally connected_ if given any two points $x$ and $y$ in $W$, there are points

$$
x_0=x,x_1,\ldots,x_{n-1},x_n=y
$$

such that $\bigcup_{i=1}^n\overline{x_{i-1}x_i}\subset W$, where $\overline{x_{i-1}x_i}$ is the closed segment joining $x_{i-1}$ and $x_i$ (Fig. 9.3).

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-9.3.svg" alt="An irregular region W containing a polygonal path from x=x0 to y=xn, with several intermediate vertices labeled." />
<figcaption>Figure 9.3. <a href="/docs/elementary-topology/reader?page=189">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-9.4.svg" alt="The graph of sin(1/x) for positive x, shown with rapidly accumulating oscillations beside the vertical axis and the origin." />
<figcaption>Figure 9.4. <a href="/docs/elementary-topology/reader?page=189">View the interactive figure and its Substance program.</a></figcaption>
</figure>

Any subset of $R^n$ which is path connected is not necessarily polygonally connected. For example, the circle

$$
\{(x,y)\mid x^2+y^2=1\}\subset R^2
$$

is path connected (it is itself a path), but it is not polygonally connected. On the other hand, any subspace of $R^n$ which is polygonally connected is path connected (Exercise 1). Paths can actually be rather exotic, and may not look anything like $[0,1]$. For example, it can be shown that $([0,1])^n$ is a path for any finite $n$.

The next example gives a connected subspace of $R^2$ which is not path connected.

<span id="printed-page-190"></span>

<!-- Source: PDF179, printed190. -->

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-9.5.svg" alt="A wireframe cube containing a straight segment joining two interior points, illustrating a convex set." />
<figcaption>Figure 9.5. <a href="/docs/elementary-topology/reader?page=190">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-9.6.svg" alt="An irregular open region U containing a polygonal route from x to y and a small rectangular neighborhood V about y, with a short continuation toward z." />
<figcaption>Figure 9.6. <a href="/docs/elementary-topology/reader?page=190">View the interactive figure and its Substance program.</a></figcaption>
</figure>

**Example 7.** Let

$$
Y=\{(x,y)\mid y=\sin(1/x),\ x>0\}\cup\{(0,0)\}\subset R^2
$$

(Fig. 9.4). We shall see from Proposition 11 that $Y$ is connected. If $P$ is any point in $Y$ other than $(0,0)$, then there is no path in $Y$ which contains $(0,0)$ and $P$. For if there were such a path, it would be possible to show that the function $f$ from the space of nonnegative real numbers in $R$ for which $Y$ is the graph is continuous. But $f$ is not continuous (Section 7.4, Exercise 5). Also see Exercise 2 below.

**Proposition 7.** If $X,\tau$ is a path-connected space, then $X$ is connected.

_Proof._ Suppose $X$ is path connected and $x\in X$. For each $y\in X$, let $P(x,y)$ be a path which contains $x$ and $y$. Then

$$
X=\bigcup\{P(x,y)\mid y\in X\}\qquad\text{and}\qquad\bigcap\{P(x,y)\mid y\in X\}\ne\phi.
$$

Therefore $X$ is connected by Proposition 6.

Recall that a subset $W$ of Euclidean $n$-space $R^n$ is _convex_ if, given any points $x$ and $y$ of $W$, the closed segment $\overline{xy}$ is a subset of $W$. We note that the basic neighborhoods in $R^n$, considered as the $n$-fold product of $R$, are convex subsets of $R^n$ (Fig. 9.5). The “open balls” of the form

$$
\{(x_1,\ldots,x_n)\mid x_1^2+\cdots+x_n^2<p\},
$$

where $p>0$, are also convex subsets of $R^n$. Of course any convex subset of $R^n$ is polygonally connected, and hence path connected, and therefore connected (in a very “strong” way).

We now prove a theorem that has wide use in analysis.

**Proposition 8.** Let $U$ be a connected open subset of $R^n$. Then $U$ is polygonally connected.

_Proof._ Choose $u\in U$. Let $A=\{a\in U\mid a\text{ can be polygonally connected to }u\text{ in }U\}$ (Fig. 9.6) and $B=U-A$. Then $A$ is open. For since $U$ is open, given any $a\in A$, there is a basic product neighborhood $V$ of $a$ such

::: warning Missing printed page 191
The proof ends here on printed page 190. Printed page 191 is absent. The rest of the proof, any intervening statements, and Exercises 1–4 are unavailable. The supplied text resumes on printed page 192 with Exercise 5.
:::

<span id="printed-page-192"></span>

<!-- Source: PDF180, printed192, Section9.2 fragment. -->

## Available Exercises

5. Let $R^2$ be the plane with the Pythagorean metric. Prove that $R^2-C$, where $C$ is any countable set, is polygonally connected. In particular, prove that

   $$
   R^2-\{(x,y)\mid x\text{ and }y\text{ are rational}\}
   $$

   is polygonally connected. [*Hint:* Through any point in $R^2-C$, there is a line which does not intersect $C$.]

6. Which of the following subspaces of $R^2$ are connected? Indicate clearly how you arrived at your conclusion.

   a) $\{(x,y)\mid y=(1/n)x,\ n=1,2,3,\ldots\}$

   b) $\{(x,y)\mid\text{either }x\text{ or }y,\text{ but not both, is irrational}\}$

   c) $\{(x,y)\mid x\ne1\}$

   d) $\{(x,y)\mid x\ne1\}\cup\{(0,1)\}$

7. Suppose $X,\tau$ is a space such that $X=A_1\cup\cdots\cup A_n$, where each $A_i$ is connected and $A_{i-1}\cap A_i\ne\phi$, $i=2,\ldots,n$. Is $X$ necessarily connected?
8. Suppose $A$ is a compact subspace of Euclidean $n$-space $R^n$, $n\ge2$. Prove that $R^n-A$ need not be connected.
9. Prove directly (that is, do not refer to Corollary 2 to Proposition 11 of the next section) that if $X$ is connected, then the one-point compactification of $X$ is also connected.

::: info Source convention
Here the source defines a path as an image subspace. Chapter 5 defined a path as a continuous function. Both chapter-specific wordings are preserved. Proposition 8’s printed heading remains legible under handwritten marks and is transcribed.
:::

[Chapter 9 contents](./index.md) · [Next: 9.3 Connectedness and the Derived Spaces](./connectedness-and-derived-spaces.md)

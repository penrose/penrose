---
title: Connectedness and the Derived Spaces — Elementary Topology
---

# 9.3 Connectedness and the Derived Spaces

<span id="printed-page-192"></span>

<!-- Source: PDF180, printed192, Section9.3 fragment. -->

We have already seen that if $X,\tau$ is a connected space, then any identification space derived from $X$ is also connected. In this section we investigate the behavior of connectedness as regards subspaces and product spaces.

It is, of course, false that any subspace of a connected space is connected. The following proposition gives a criterion for determining whether or not a subspace of a given space is connected.

**Proposition 10.** If $A$ is a subspace of the space $X,\tau$, then $A$ is connected if and only if $A$ cannot be expressed as $S\cup T$, where $S$ and $T$ are nonempty subsets of $X$ and

$$
S\cap\operatorname{Cl}T=\operatorname{Cl}S\cap T=\phi.
$$

(Note that no demand is made that $S$ and $T$ be open or closed in $A$.)

_Proof._ If $A$ is not connected, then $A=S\cup T$, where $S$ and $T$ are disjoint, nonempty subsets of $A$ which are open and closed in $A$. Suppose

$$
x\in S\cap\operatorname{Cl}T.
$$

Then since $S\subset A$, $x\in A\cap\operatorname{Cl}T=\operatorname{Cl}T$ in $A$ (Chapter 4, Proposition <span id="printed-page-193"></span><!-- Source: PDF181, printed193. -->4) $=T$. Therefore $x\in S\cap T=\phi$, a contradiction. Then $S\cap\operatorname{Cl}T=\phi$; similarly, $\operatorname{Cl}S\cap T=\phi$.

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-9.7.svg" alt="Two shaded unit disks S and T on coordinate axes, centered at (0,0) and (2,0); their circular boundaries touch at (1,0)." />
<figcaption>Figure 9.7. <a href="/docs/elementary-topology/reader?page=193">View the interactive figure and its Substance program.</a></figcaption>
</figure>

Suppose that $A=S\cup T$, where $\operatorname{Cl}S\cap T=S\cap\operatorname{Cl}T=\phi$, and $S$ and $T$ are nonempty. Then

$$
\begin{aligned}
\operatorname{Cl}S\text{ in }A&=A\cap\operatorname{Cl}S=(S\cup T)\cap\operatorname{Cl}S\\
&=(S\cap\operatorname{Cl}S)\cup(T\cap\operatorname{Cl}S)=S\cup\phi=S.
\end{aligned}
$$

Therefore $S$ is closed in $A$; similarly, $T$ is closed in $A$. Hence $A$ is disconnected.

**Corollary.** Two subsets $S$ and $T$ of a space $X,\tau$ are said to be _mutually separated_ if

$$
\operatorname{Cl}S\cap T=S\cap\operatorname{Cl}T=\phi.
$$

Suppose $S$ and $T$ are mutually separated subsets of $X$ and $A$ is a connected subspace of $S\cup T$. Then either $A\subset S$, or $A\subset T$.

The proof of this corollary is left as an exercise.

**Example 8.** Let $R^2$ be the plane with the Pythagorean topology,

$$
S=\{(x,y)\mid x^2+y^2<1\}
$$

and

$$
T=\{(x,y)\mid(x-2)^2+y^2<1\}.
$$

(Fig. 9.7). Then $\operatorname{Cl}S\cap T=\operatorname{Cl}T\cap S=\phi$. Therefore $S\cup T$ is a disconnected subspace of $R^2$. Note, however, that

$$
\operatorname{Cl}S\cap\operatorname{Cl}T=\{(1,0)\}\ne\phi.
$$

**Proposition 11.** Suppose $A$ is a connected subspace of $X,\tau$ and

$$
A\subset Y\subset\operatorname{Cl}A.
$$

Then $Y$ is also a connected subspace of $X$.

_Proof._ If $Y$ is disconnected, then $Y=S\cup T$, where $S$ and $T$ are mutually separated (Proposition 10). Since $A$ is connected, either $A\subset S$ or $A\subset T$, by the corollary to Proposition 10. Suppose $A\subset S$. Then $\operatorname{Cl}A\subset\operatorname{Cl}S$; <span id="printed-page-194"></span><!-- Source: PDF182, printed194. -->hence $Y\subset\operatorname{Cl}A\subset\operatorname{Cl}S$. But then since $Y=S\cup T$, $T\subset\operatorname{Cl}S$. Since $T\cap\operatorname{Cl}S=\phi$, we have arrived at a contradiction. Therefore $Y$ is connected.

**Corollary 1.** If a space $X,\tau$ contains a connected dense subspace, then $X$ is connected.

_Proof._ Suppose $A$ is a connected dense subspace of $X$. Then $\operatorname{Cl}A=X$ is connected by Proposition 11.

**Corollary 2.** If $X,\tau$ is connected, then any compactification $Y$ of $X$ is connected.

_Proof._ If $Y$ is a compactification of $X$, then $X$ is a dense connected subspace of $Y$; therefore $Y$ is connected, by Corollary 1.

**Example 9.** We see that the space $Y$ in Example 7 is connected as follows: Set $A=Y-\{(0,0)\}$, and define a function $h$ from $\{x\mid0<x\}\subset R$ into $R^2$ by

$$
h(x)=(x,\sin(1/x)).
$$

Then $h$ is easily seen to be continuous; in fact, $h$ is a homeomorphism onto its image $A$. Therefore, since $\{x\mid0<x\}$ is connected, $A$ is connected. Now $(0,0)$ is in $\operatorname{Cl}A$ since every neighborhood of $(0,0)$ contains infinitely many points of $A$ (cf. Fig. 9.4). Then $A\subset Y\subset\operatorname{Cl}A$; hence, by Proposition 11, $Y$ is connected.

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-9.8.svg" alt="A spiral approaching a limiting circular boundary, shown beside the discussion of a connected spiral together with its closure." />
<figcaption>Figure 9.8. <a href="/docs/elementary-topology/reader?page=194">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-9.9.svg" alt="Coordinate axes with horizontal and vertical slices A1 and A2 and a small product rectangle W1 times W2 around (x0,y0)." />
<figcaption>Figure 9.9. <a href="/docs/elementary-topology/reader?page=194">View the interactive figure and its Substance program.</a></figcaption>
</figure>

**Example 10.** Consider the graph $Y$ of the equation $r=1-1/A$, $A>1$, in polar coordinates (considered as a subspace of $R^2$ with the usual topology) (Fig. 9.8). It may be verified that

$$
\operatorname{Cl}Y=Y\cup\{(r,A)\mid r=1\}
$$

(again in polar coordinates.) Thus $Y$ together with any set of points on the unit circle forms a connected subspace of $R^2$.

<span id="printed-page-195"></span>

<!-- Source: PDF183, printed195. -->

We now investigate connectedness and product spaces.

**Proposition 12.** The product space $\mathop{\Large\times}_I X_i$ of the countable family of nonempty spaces

$$
\{X_i,\tau_i\},\qquad i\in I,
$$

is connected if and only if each $X_i$ is connected.

_Proof._ Suppose each $X_i$ is connected, but $\mathop{\Large\times}_I X_i=U\cup V$, where $U$ and $V$ are disjoint, open, nonempty subsets of $\mathop{\Large\times}_I X_i$. Choose $u\in U$ and $v\in V$ (Fig. 9.9). Since $U$ is open, there is a basic neighborhood $\mathop{\Large\times}_I W_i$ of $u$ such that $\mathop{\Large\times}_I W_i\subset U$, $W_i$ is open in $X_i$, and $W_i=X_i$, except for $i_1,\ldots,i_n$. Define $c_i^0=v_i$ (the $i$th coordinate of $v$) if $i\ne i_1,\ldots,i_n$, but $c_i^0=u_i$ if $i=i_1,\ldots,i_n$. Then

$$
c^0=(c_1^0,\ldots,c_i^0,\ldots)\in\mathop{\Large\times}_I W_i\subset U.
$$

Define $c^1$ by letting $c_i^1=v_i$ if $i\ne i_1,\ldots,i_n$; $c_i^1=v_i$ if $i=i_1$; and $c_i^1=u_i$ if $i=i_2,\ldots,i_n$. Generally, define $c^m$ by setting

$$
c_i^m=v_i,\quad i\ne i_{m+1},\ldots,i_n,\qquad\text{and}\qquad c_i^m=u_i,\quad i=i_{m+1},\ldots,i_n.
$$

Then $c^n=v$.

Let

$$
A_m=\{x\mid x_{i_m}\text{ is arbitrary, }x_{i_{m+1}}=u_{i_{m+1}},\ldots,x_{i_n}=u_{i_n},\text{ and otherwise, }x_i=v_i\}.
$$

Then $c^{m-1}\in A_{m-1}\cap A_m$, $1\le m\le n$; $v=c^n\in A_n$. $A_1,\ldots,A_n$ form a “chain” from $U$ to $V$, for, since $c^m\in A_m\cap A_{m+1}$,

$$
A_m\cap A_{m+1}\ne\phi.
$$

We now show that each $A_i$ is connected.

Define a function $g_m$ from $X_{i_m}$ onto $A_m$ as follows: $g(x_{i_m})$ is the point of $A_m$ with $i_m$th coordinate $x_{i_m}$; $i_{m+1}$th coordinate $u_{i_{m+1}},\ldots,i_n$th coordinate $u_{i_n}$; and for all other $i$ the $i$th coordinate $v_i$. Not only is $g_m$ continuous, but also it is a homeomorphism (since it is the inverse of the projection from $A_m$ onto $X_{i_m}$. Since each $X_{i_m}$ is connected, each $A_m$ is also connected. But then $\bigcup_{i=1}^m A_m$ is connected (Section 9.2, Exercise 7). But $\bigcup_{i=1}^m A_m$ meets both of the disjoint, nonempty, open sets $U$ and $V$, a contradiction of Proposition 4. Therefore $Y$ must be connected.

If $\mathop{\Large\times}_I X_i$ is connected, then since the projection mapping $p_i$ from $\mathop{\Large\times}_I X_i$ onto $X_i$ is continuous, $X_i$ is also connected.

::: info Source notation
Example 10 uses $A$ as its polar angle. Proposition 12’s proof prints $A_{m-1}$ for $1\le m\le n$, $g(x_{i_m})$ after defining $g_m$, unions $\bigcup_{i=1}^m A_m$, and “$Y$ must be connected”; its unmatched explanatory parenthesis also remains as printed.
:::

<span id="printed-page-196"></span>

<!-- Source: PDF184, printed196, Section9.3 fragment. -->

## Exercises

1. Prove the corollary to Proposition 10.
2. a) Prove that a space $X,\tau$ is connected if and only if given any continuous function $f$ from $X$ into $R$, the space of real numbers with the absolute value topology, then if $a$ and $b$ are in $f(X)$ and $c$ is a real number such that $a\le c\le b$, there is $y\in X$ such that $f(y)=c$.

   b) Use (a) to prove that any connected normal space which contains at least two points contains at least $c$ points, where $c$ is the cardinality of $[0,1]$.

   c) Prove that any connected separable metric space $X,D$ has either at most one point, or exactly $c$ points. [*Hint:* By (b), if $X$ has 2 points, it has at least $c$ points. Take a countable dense subset $S=\{s_n\mid n\in N\}$. Each point of $X$ is the limit of some sequence of elements of $S$, and each sequence of elements of $S$ has at most one limit. How many sequences of elements of $S$ are there?]

3. Determine whether each of the spaces below is connected or disconnected.

   a) $\{(x,y)\mid y=1/x\}\cup\{(x,y)\mid y=0\}\subset R^2$ with the usual topology

   b) $\{(x,y)\mid x^2+y^2<1\}\cup(x,y)\mid x=1\}\subset R^2$ with the usual topology

   c) the metric space described in Example 5 of Chapter 2

   d) the plane $R^2$ with the topology described in Example 5 of Chapter 3

4. A subset $A$ of a space $X,\tau$ is said to _disconnect_ $X$ if $X-A$ is disconnected. We say that $A$ is a _minimal disconnecting subset_ of $X$ if $X-A$ is disconnected, but $X-B$ is not disconnected, where $B$ is any proper subset of $A$.

   a) Describe some minimal disconnecting subsets for Euclidean $n$-space, $R^n$.

   b) If $x\in X$ and $\{x\}$ is a minimal disconnecting subset of $X$, then $x$ is said to be a _cut point_ of $X$.

   i) Prove that if $f$ is a homeomorphism from a space $X,\tau$ onto a space $Y,\tau'$, then if $x$ is a cut point of $X$, $f(x)$ is a cut point of $Y$.

   ii) Prove that a circle, closed interval, open interval, and half-open interval are not homeomorphic in pairs. [*Hint:* Examine the cut points of each.]

5. A connected subset $A$ of a space $X$ is said to be _irreducibly connected about_ $B\subset A$ if $A$ is the smallest connected set which contains $B$. For example, $[0,1]$ is irreducibly connected about $\{0,1\}$ in the usual space $R$ of real numbers. Find sets about which the following subspaces of $R^2$ are irreducibly connected. If no such set exists, write _none_.

   a) $\{(x,y)\mid0\le x\le1,\ y=0\}$

   b) $\{(x,y)\mid0\le x\le1,\ 0\le y\le1\}$

   c) $\{(x,y)\mid x^2+y^2=1\}$

   d) $\{(x,y)\mid x^2+y^2>1\}$

::: info Source delimiter
Exercise 3(b) lacks an opening brace for its second set in the supplied print; that reading is retained.
:::

[Chapter 9 contents](./index.md) · [Next: 9.4 Components. Local Connectedness](./components-and-local-connectedness.md)

---
title: The Notion of Connectedness — Elementary Topology
---

# 9.1 The Notion of Connectedness

::: warning Missing printed page 183
The supplied scan omits printed page 183, the chapter’s opening page. The section title above is verified by the running header on printed page 185. No missing introduction, definition, example, or proposition is reconstructed. Printed page 184 begins with the following proof fragment.
:::

<span id="printed-page-184"></span>

<!-- Source: PDF173, printed184. -->

_Proof._ Suppose $[0,1]$ is disconnected. Then $[0,1]=U\cup V$, where $U$ and $V$ are disjoint, nonempty, open subsets of $[0,1]$. Suppose $u\in U$ and $v\in V$. We may assume $u<v$ (relabeling $U$ and $V$, if necessary). Let $S$ be the set of numbers $s$ such that $s<u$ or $[u,s]\subset U$. Then $S$ has $1$ as an upper bound, and hence $S$ has a least upper bound $a$ with $0<a<1$. Since $[0,1]=U\cup V$, either $a\in U$ or $a\in V$. Suppose $a\in U$. Since $U$ is open, there is $p>0$ such that $(a-p,a+p)\subset U$. Then

$$
[a-p/2,a+p/2]\subset U
$$

(Fig. 9.1); hence $a+p/2\in S$. This contradicts the assumption that $a$ is an upper bound for $S$. Suppose $a\in V$. Then there is $p>0$ such that $(a-p,a+p)\subset V$, and thus $a-p/2$ is an upper bound for $S$, contradicting the assumption that $a$ is the least upper bound. Therefore $[0,1]$ is not disconnected; hence $[0,1]$ is connected.

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-9.1.svg" alt="The interval [0,1] with two purported separating open sets U and V, a point a, and a neighborhood about a crossing the proposed split." />
<figcaption>Figure 9.1. <a href="/docs/elementary-topology/reader?page=184">View the interactive figure and its Substance program.</a></figcaption>
</figure>

Note that since connectedness has been defined in a negative way, that is, a space is connected if it is not disconnected, most proofs that a space is connected are by contradiction: a space is assumed to be disconnected and a contradiction is proved.

Connectedness, like compactness, is a property which is preserved by continuous functions.

**Proposition 2.** Suppose $f$ is a continuous function from a space $X,\tau$ onto a space $Y,\tau'$. If $X$ is connected, then so is $Y$.

_Proof._ Assume $Y$ is not connected. Then $Y=U\cup V$, where $U$ and $V$ are nonempty, disjoint, open subsets of $Y$. Then $f^{-1}(U)$ and $f^{-1}(V)$ are disjoint, nonempty (since $f$ is onto) subsets of $X$ whose union is $X$. But since $f$ is continuous, $f^{-1}(U)$ and $f^{-1}(V)$ are also open subsets of $X$; therefore $X$ is disconnected, a contradiction. Hence $Y$ must be connected.

**Corollary 1.** Any closed interval in $R$, any closed line segment in $R^2$, and, in general, any image of $[0,1]$ under a continuous function is connected.

**Corollary 2.** If $X,\tau$ is any connected space and $R$ is an equivalence relation on $X$, then the identification space $X/R$ is connected.

_Proof._ The identification mapping from $X$ onto $X/R$ is continuous.

<span id="printed-page-185"></span>

<!-- Source: PDF174, printed185. -->

**Example 2.** Since a circle in $R^2$ (with the Pythagorean topology) can be thought of as an identification space formed from $[0,1]$, the circle is also connected.

We now give some more criteria for connectedness.

**Proposition 3.** Let $X,\tau$ be any topological space. Then the following statements are equivalent.

a) $X$ is connected.

b) $X$ cannot be expressed as the union of two disjoint, nonempty, closed subsets.

c) The only subsets of $X$ which are open and closed are $X$ and $\phi$.

d) If $A$ is any subset of $X$ other than $X$ or $\phi$, then $\operatorname{Fr}A\ne\phi$.

e) Let $Y=\{0,1\}$ have the discrete topology. Then there is no continuous function from $X$ onto $Y$.

_Proof._ Statement (a) implies statement (b). Suppose $X=A\cup B$, where $A$ and $B$ are disjoint, nonempty, closed subsets of $X$. Then $X-A=B$ and $X-B=A$ are both the complements of closed sets, and hence are open. Thus $X=A\cup B$ is also the expression of $X$ as the union of two disjoint, nonempty, open subsets of $X$. Hence $X$ is not connected.

Statement (b) implies statement (c). Suppose $A$ is a subset of $X$ which is both open and closed, but that $A$ is neither $X$ nor $\phi$. Then $X-A$ is also open and closed, and nonempty. Thus

$$
X=(X-A)\cup A
$$

is the expression of $X$ as the union of two disjoint, nonempty, closed subsets, contradicting (b).

Statement (c) implies statement (d). If $A$ is a subset of $X$ other than $X$ or $\phi$, and $\operatorname{Fr}A=\phi$, then since $\operatorname{Cl}A=A^\circ\cup\operatorname{Fr}A$, we have $\operatorname{Cl}A=A^\circ$. On the other hand, $A^\circ\subset A$ and $A\subset\operatorname{Cl}A$, and hence $A=A^\circ=\operatorname{Cl}A$; thus $A$ is both open and closed in $X$. Therefore if (c) holds, there can be no subset $A$ of $X$, other than $X$ or $\phi$, such that $\operatorname{Fr}A=\phi$.

Statement (d) implies statement (a). Suppose $X=U\cup V$, where $U$ and $V$ are disjoint, open, nonempty subsets of $X$. Then $U$ and $V$ are also closed. Therefore

$$
U=U^\circ=\operatorname{Cl}U.
$$

But $\operatorname{Fr}U=\operatorname{Cl}U-U^\circ$ (Proposition 12, Chapter 3); hence

$$
\operatorname{Fr}U=U-U=\phi,
$$

a contradiction of (d).

<span id="printed-page-186"></span>

<!-- Source: PDF175, printed186. -->

Statement (a) implies statement (e). Suppose there is a continuous function from $X$ onto $Y=\{0,1\}$. Then since $X$ is connected, $Y$ must be also, which is not the case.

Statement (e) implies statement (a). Suppose $X$ is disconnected. Then $X=U\cup V$, where $U$ and $V$ are nonempty, disjoint, open subsets of $X$. Define $g:X\to Y$ by $g(x)=0$, if $x\in U$, and by $g(x)=1$, if $x\in V$. Then $g^{-1}(\{0\})=U$ and $g^{-1}(\{1\})=V$; hence $g$ is continuous, a contradiction of (e).

We now see why the extension $F$ in Example 15 of Chapter 5 cannot be continuous.

**Example 3.** There are a number of ways that we can show that the space $N$ in Example 3 of Chapter 7 is connected. For example, let $A$ be any subset of $N$. Suppose $A$ is infinite, but not all of $N$. Since a subset of $N$ is closed if and only if it is finite, the only closed set which contains $A$ is $N$; hence $\operatorname{Cl}A=N$. Then

$$
\operatorname{Fr}A=N-A\ne\phi
$$

since $A\ne N$. Suppose $A$ is finite but nonempty. Then $A$ is closed and thus $\operatorname{Cl}A=A$. But since $A$ excludes infinitely many elements of $N$, $A^\circ=\phi$; hence $\operatorname{Fr}A=A\ne\phi$. By (d) of Proposition 3, $N$ is connected.

**Proposition 4.** Suppose $X,\tau$ is a space such that $X=U\cup V$, where $U$ and $V$ are disjoint, open, nonempty subsets of $X$. Let $A$ be any connected subspace of $X$. Then either $A\subset U$ or $A\subset V$.

_Proof._ If $A\cap U\ne\phi$ and $A\cap V\ne\phi$, then $A\cap U$ and $A\cap V$ are nonempty, disjoint subsets of $A$ which are open in $A$. But

$$
A=(A\cap U)\cup(A\cap V);
$$

thus $A$ is not connected. Therefore either $A\cap U=\phi$ and hence $A\subset V$, or $A\cap V=\phi$ and hence $A\subset U$.

**Example 4.** Let $R$ be the space of real numbers with the absolute value topology. Then the removal of any point $y$ from $R$ disconnects $R$ into two “rays,” open half-lines

$$
H^+=\{x\in R\mid y<x\}\qquad\text{and}\qquad H^-=\{x\in R\mid x<y\}.
$$

The removal of $y$ also disconnects any interval which contains $y$ as anything but an endpoint. For if, say, $[a,b]$ contains $y$ in its interior and $[a,b]-\{y\}$ is connected, then $[a,b]-\{y\}$ must lie entirely in either $H^+$ or $H^-$, an impossibility.

<span id="printed-page-187"></span>

<!-- Source: PDF176, printed187, Section9.1 fragment. -->

## Exercises

1. Find another proof that the space $N$ in Example 3 is connected.
2. Prove that each of the following subspaces of the space of real numbers with the absolute value topology is disconnected.

   a) any finite subset

   b) $(0,1)\cup(6,7)\cup(9,18)$

   c) $\{x\mid x\text{ is irrational}\}$

   d) $\{x\mid x=1/n,\text{ where }n\text{ is a positive integer or }x=0\}$

3. Modify the proof of Proposition 1 to show that $(0,1)$, and hence $R$, is connected.
4. A space $X,\tau$ is said to be _totally disconnected_ if $X$ is not connected and the only connected subspaces of $X$ are $\phi$ and subspaces which consist of only one point. Prove that each of the following spaces are totally disconnected.

   a) the subspace of rational numbers in the usual space of real numbers

   b) any discrete space of more than one point

   c) the set of real numbers with the topology described in Chapter 3, Example 11

5. Suppose $A$ and $B$ are connected subspaces of a connected space $X,\tau$. Show by producing an example that the following need not be connected.

   a) $A\cap B$  b) $A\cup B$  c) $\operatorname{Fr}A$  d) $A^\circ$

6. Let $X$ be a space with the property that given any $x\in X$ and any neighborhood $U$ of $x$, there is a neighborhood $V$ of $x$ such that $\operatorname{Cl}V$ is a proper subset of $U$. Prove that $X$ is connected. Would $X$ necessarily be connected if $\operatorname{Cl}V$ is replaced by $V$ in the first sentence?

::: info Source wording and formulas
Example 3’s closed-set characterization and displayed $\operatorname{Fr}A=N-A$ are retained. Exercise 2(a) prints “any finite subset,” and Exercise 6’s assertion remains as stated in the source.
:::

[Chapter 9 contents](./index.md) · [Next: 9.2 Further Tests for Connectedness](./further-tests-for-connectedness.md)

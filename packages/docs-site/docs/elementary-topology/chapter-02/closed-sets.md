---
title: Closed Sets — Elementary Topology
description: Section 2.4, printed pages 24–26, of Elementary Topology, second edition.
---

# 2.4 Closed Sets

::: info Transcription note
Source: printed pages 24–26 (PDF pages 31–33). Printed page 26 continues with §2.5 after Exercise 6. The citation “Proposition 2(c)” in the proof below is retained as printed.
:::

<span id="printed-page-24"></span>

<!-- source: PDF 31, printed 24 -->

**Definition 4.** Let $X,D$ be a metric space. A subset $F$ of $X$ is said to be _closed_ if $F$ is the complement in $X$ of an open set; that is, $F=X-U$, where $U$ is open.

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-2.9.svg" alt="Figure 2.9: an interval about x in the complement of the closed interval from zero to one." /><figcaption>Figure 2.9</figcaption></figure>

**Example 10.** The closed interval $[0,1]$ is a closed subset of the real line $R$ with the absolute value metric. For suppose $x\in R-[0,1]$. Set $\rho=\min(|1-x|,|x|)$. Then $N(x,\rho)\subset R-[0,1]$. Therefore $R-[0,1]$ is open, and hence $[0,1]$ is closed (Fig. 2.9).

**Example 11.** If $X$ is a set with metric $D$, if $x\in X$, and if $\rho$ is any positive number, then the _closed $\rho$-neighborhood_ of $x$, denoted by $\operatorname{ClN}(x,\rho)$, is defined to be the set of all $y\in X$ such that $D(x,y)\leq\rho$, that is,

$$
\operatorname{ClN}(x,\rho)=\{y\in X\mid D(x,y)\leq\rho\}.
$$

It is left as an exercise to show that $\operatorname{ClN}(x,\rho)$ is a closed subset of $X$. Note in Example 10 that $[0,1]=\operatorname{ClN}(\frac12,\frac12)$; therefore the fact that $[0,1]$ is closed follows from the more general considerations of this example.

The following proposition gives the basic properties of closed sets in a metric space.

**Proposition 4.** Let $X,D$ be a metric space. Then

a) $X$ and $\phi$ are closed subsets of $X$,

b) the union of any two closed sets is closed, and

c) the intersection of any family of closed sets is again a closed set.

_Proof_

a) $X=X-\phi$. Since $\phi$ is an open set, $X$ is the complement of an open set and hence is closed. Now, $\phi=X-X$, hence $\phi$ is also the complement of an open set, and is therefore closed. (Note that $X$ and $\phi$ are _both_ open and closed. It is quite possible for a set to be both open and closed; the complement of such a set would also have the property of being both open and closed.)

b) Let $F$ and $F'$ be any closed subsets of $X$. Then $F=X-U$ and $F'=X-U'$, where $U$ and $U'$ are open subsets of $X$. Then

$$
F\cup F'=(X-U)\cup(X-U')=X-(U\cap U').
$$

But $U\cap U'$ is open by Proposition 2(c); therefore $X-(U\cap U')=F\cup F'$ is closed.

<span id="printed-page-25"></span>

<!-- source: PDF 32, printed 25 -->

c) Let $\{F_i\}$, $i\in I$, be any family of closed subsets of $X$. Then $F_i=X-U_i$, where $U_i$ is an open subset of $X$ for each $i\in I$. It follows that

$$
\bigcap_I F_i=\bigcap_I(X-U_i)=X-\bigcup_I U_i.
$$

Since $\bigcup_I U_i$ is open, $\bigcap_I F_i$ is closed.

**Proposition 5.** If $X$ is a set with metric $D$ and $x\in X$, then $\{x\}$ is a closed subset of $X$.

_Proof._ Since $\{x\}=X-(X-\{x\})$, if we show that $X-\{x\}$ is open, we will have shown that $\{x\}$ is closed. Suppose $y\in X-\{x\}$. Set $\rho=D(x,y)$. Then $N(y,\rho)\subset X-\{x\}$; hence $X-\{x\}$ is open.

## Exercises

1. Show that the union of an arbitrary family of closed subsets of a metric space need not be closed. Find an example to show that the intersection of any family of open sets need not be open. [_Hint:_ Use Proposition 5.]
2. Let $X,D$ be a metric space and $Y,D\mid Y$ be a metric subspace of $X$ (see Example 4). Prove each of the following.

   a) A subset $W$ of $Y$ is open in $Y$ (that is, is $D\mid Y$-open) if and only if $W=Y\cap U$, where $U$ is an open subset of $X$.

   b) A subset $C$ of $Y$ is closed in $Y$ if and only if $C=Y\cap F$, where $F$ is a closed subset of $X$.

   c) If $Y$ is an open subset of $X$, then a subset of $Y$ is open in $Y$ if and only if it is open (in $X$).

   d) If $Y$ is a closed subset of $X$, then a subset of $Y$ is closed in $Y$ if and only if it is closed (in $X$).

   e) A subset of $Y$ may be open or closed in $Y$ without being open or closed in $X$.

3. Prove that a subset $F$ of a metric space $X,D$ is closed if and only if $X-F$ is open.
4. Decide which of the following subsets of $R^2$ with the Pythagorean metric are closed.

   a) $\{(x,y)\mid x=0,y\leq5\}$

   b) $\{(x,y)\mid x=2\text{ or }x=3,y\text{ is an integer}\}$

   c) $\{(x,y)\mid x^2+y^2<1,\text{ or }(x,y)=(1,0)\}$

   d) $\{(x,y)\mid y=x^2\}$

5. Suppose that $\{F_i\}$, $i\in I$, is a family of closed subsets of a metric space $X,D$ with the property that given any $x\in X$, there is $\rho>0$ such that $N(x,\rho)$ intersects finitely many of the $F_i$. Prove that $\bigcup_I F_i$ is closed. Try to find and prove an analogous statement for a family of open subsets of $X$.

<span id="printed-page-26"></span>

<!-- source: PDF 33, printed 26; section 2.4 fragment -->

6. Prove that a straight line in $R^2$ is closed with respect to all of the metrics on $R^2$ introduced thus far. Prove a circle in $R^2$ (circle in the usual geometric sense) is closed with respect to all of these metrics. List some other standard geometric objects which are always closed.

[Continue to 2.5 Convergence of Sequences](./convergence-of-sequences)

<style>
.topology-chapter-figure { max-width: 28rem; margin: 1.75rem auto; text-align: center; }
.topology-chapter-figure img { width: 100%; background: white; }
.topology-chapter-figure figcaption { font-family: Georgia, serif; font-size: 0.875rem; margin-top: 0.4rem; }
.topology-chapter-figure-pair { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 1.5rem; margin: 1.75rem 0; }
.topology-chapter-figure-pair .topology-chapter-figure { margin: 0; }
@media (max-width: 480px) { .topology-chapter-figure-pair { grid-template-columns: 1fr; } }
</style>

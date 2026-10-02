---
title: Convergence of Sequences — Elementary Topology
description: The available text of section 2.5, printed pages 26–28, of Elementary Topology, second edition.
---

# 2.5 Convergence of Sequences

::: info Transcription note
Source: printed pages 26–28 (PDF pages 33–35). Printed page 29 is missing; the available exercises stop at 1(c). No unavailable exercise continuation has been supplied.
:::

<span id="printed-page-26"></span>

<!-- source: PDF 33, printed 26; section 2.5 fragment -->

The reader should already have been introduced in previous courses to the notions of convergence of sequences, limits, and continuity, at least as far as the real numbers with the absolute value metric is concerned. We now extend these ideas to general metric spaces.

**Definition 5.** Let $X,D$ be a metric space and $S=\{s_n\}$, $n\in N$, be a sequence in $X$. (The capital $N$ will be used almost exclusively in this text to denote the set of positive integers.) Then $S$ is said to _converge_ to a point $y$ of $X$ if given any positive number $\rho$, there is a positive integer $M$ such that if $n>M$, then $s_n\in N(y,\rho)$ (Fig. 2.10). If $S$ converges to $y$, then we may write $s_n\to y$; $y$ is said to be the _limit_ of $S$.

Definition 5 could be restated as follows: $s_n\to y$ if all but a finite number of the $s_n$ are in $N(y,\rho)$ for any positive number $\rho$. Or again, $s_n\to y$ if all but finitely many of the $s_n$ are closer to $y$ than any given distance.

**Example 12.** Consider the sequence defined by $s_n=(1,1/n)$ in the coordinate plane (Fig. 2.11). If any of the metrics $D,D_1$, or $D_3$ are used, then this sequence converges to $(1,0)$. For in these cases,

$$
D(s_n,(1,0))=D_1(s_n,(1,0))=D_3(s_n,(1,0))=1/n
$$

for any $n\in N$. Given any positive number $\rho$, if we let $M$ be any integer greater than $1/\rho$, if $n>M$, then $D(s_n,(1,0))<\rho$. On the other hand, this sequence does not converge to $(1,0)$ with respect to the metric $D_2$.

<div class="topology-chapter-figure-pair">
<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-2.10.svg" alt="Figure 2.10: all but finitely many terms of a sequence lie in a neighborhood of its limit y." /><figcaption>Figure 2.10</figcaption></figure>
<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-2.11.svg" alt="Figure 2.11: the sequence (1,1/n) and neighborhoods of (1,0) for three metrics." /><figcaption>Figure 2.11</figcaption></figure>
</div>

<span id="printed-page-27"></span>

<!-- source: PDF 34, printed 27 -->

<div class="topology-chapter-figure-pair">
<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-2.12.svg" alt="Figure 2.12: graphs of the functions y=x, y=x², and y=x³ on the unit square." /><figcaption>Figure 2.12</figcaption></figure>
<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-2.13.svg" alt="Figure 2.13: the graph of y=xⁿ against a one-third collar of the pointwise limiting function." /><figcaption>Figure 2.13</figcaption></figure>
</div>

For if $\rho<1$, then the $D_2$-$\rho$-neighborhood of $(1,0)$ contains only $(1,0)$, and hence excludes all of the points of the sequence.

**Example 13.** Let $X,D$ be the metric space of functions described in Example 5. Let $S$ be the sequence in $X$ defined by $s_n(x)=x^n$. If we were to plot the graphs of $s_n$ for successively greater $n$ (Fig. 2.12), it would appear that the sequence $S$ converges to the function $f\in X$ defined by

$$
f(x)=\begin{cases}0&\text{if }x\neq1,\\1&\text{if }x=1.\end{cases}
$$

Such is not the case, however. For if we draw a $\rho$-collar about the graph of $f$ for $\rho=\frac13$ (Fig. 2.13), we see that no $s_n$ has its graph wholly within the collar; therefore $S$ cannot converge to $f$. Note, however, that $S$ converges “pointwise” to $f$; that is, for each $x\in[0,1]$, $s_n(x)\to f(x)$, where $\{s_n(x)\}$, $n\in N$, is considered as a sequence in the plane with the Pythagorean metric.

**Proposition 6.** If $S=\{s_n\}$, $n\in N$, is a sequence in a metric space $X,D$ such that $s_n\to y$ and $s_n\to y'$, then $y=y'$. That is, a sequence in a metric space can converge to at most one limit.

_Proof._ We will suppose $y\neq y'$, and prove a contradiction. Set

$$
\rho=\tfrac12D(y,y')
$$

(Fig. 2.14). Then

$$
N(y,\rho)\cap N(y',\rho)=\phi.
$$

For if $w\in N(y,\rho)\cap N(y',\rho)$, then we have

$$
D(y,y')\leq D(y,w)+D(w,y')<\rho+\rho=D(y,y'),
$$

a contradiction. But since $s_n\to y$ and $s_n\to y'$, both $N(y,\rho)$ and $N(y',\rho)$

<span id="printed-page-28"></span>

<!-- source: PDF 35, printed 28 -->

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-2.14.svg" alt="Disjoint neighborhoods of y and y′, both alleged limits of the same sequence." /><figcaption>Figure 2.14</figcaption></figure>

each contain all but finitely many of the $s_n$. Therefore some $s_n$ must be in both $N(y,\rho)$ and $N(y',\rho)$, contradicting the fact that $N(y,\rho)$ and $N(y',\rho)$ have no points in common. Therefore $y=y'$.

Open sets were defined in terms of $\rho$-neighborhoods. Since convergence is also defined using $\rho$-neighborhoods, we might suspect that convergence can also be characterized solely in terms of open sets (rather than $\rho$-neighborhoods). The following proposition gives such a characterization.

**Proposition 7.** A sequence $S=\{s_n\}$, $n\in N$, in a metric space $X,D$ converges to $y$ if and only if any open set which contains $y$ contains all but finitely many of the $s_n$.

_Proof._ Suppose $S$ converges to $y$ and $U$ is any open set which contains $y$. Since $U$ is open, there is $\rho>0$ such that $N(y,\rho)\subset U$. But since $s_n\to y$, all but finitely many of the $s_n$ are elements of $N(y,\rho)$; therefore all but finitely many of the $s_n$ are elements of $U$.

Conversely, suppose that given any open set $U$ which contains $y$, all but finitely many of the $s_n$ are elements of $U$. Let $\rho$ be any positive number. Then $N(y,\rho)$ is an open set which contains $y$; hence all but finitely many of the $s_n$ are elements of $N(y,\rho)$. Therefore $s_n\to y$.

## Exercises

1. Discuss the convergence of each of the following sequences in the spaces indicated.

   a) $s_n=1+1/n$, in the space of real numbers with the absolute value metric

   b) $s_n=(2,2)$, in the plane $R^2$ with the metric $D_2$ (Example 3)

   c) $s_n=(2,n)$, in the plane $R^2$ with metric $D_3$

::: warning Missing source page 29
The next printed page is absent. The remaining exercise text, if any, and the opening of §2.6 are unavailable. The next supplied page, printed page 30, begins with Figure 2.15 and Example 14.
:::

[Continue to the available text of 2.6 Continuity](./continuity)

<style>
.topology-chapter-figure { max-width: 28rem; margin: 1.75rem auto; text-align: center; }
.topology-chapter-figure img { width: 100%; background: white; }
.topology-chapter-figure figcaption { font-family: Georgia, serif; font-size: 0.875rem; margin-top: 0.4rem; }
.topology-chapter-figure-pair { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 1.5rem; margin: 1.75rem 0; }
.topology-chapter-figure-pair .topology-chapter-figure { margin: 0; }
@media (max-width: 480px) { .topology-chapter-figure-pair { grid-template-columns: 1fr; } }
</style>

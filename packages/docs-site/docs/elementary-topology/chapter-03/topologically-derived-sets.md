---
title: More About Topologically Derived Sets — Elementary Topology
description: The available text of section 3.6, printed pages 59–62, of Elementary Topology, second edition.
---

# 3.6 More About Topologically Derived Sets

::: info Transcription note
Source: printed pages 59–62 (PDF pages 62–65). Printed page 63 is missing. A handwritten strike crosses the sentence “Thus $x\in\operatorname{Fr}(X-A)$” in the proof of Proposition 12(c) on printed page 60; the underlying printed sentence is retained here. No unavailable proposition, exercise, or other continuation is reconstructed.
:::

<span id="printed-page-59"></span>

<!-- source: PDF 62, printed 59; section 3.6 fragment -->

In this section we continue the discussion begun in Section 3.5. Throughout this section $X,\tau$ will be assumed to be a topological space.

**Proposition 12.** If $A\subset X$, then

a) $\operatorname{Cl}A=A\cup A'$;

b) $\operatorname{Cl}A=A^\circ\cup\operatorname{Fr}A$;

c) $\operatorname{Fr}A=\operatorname{Fr}(X-A)$;

d) $\operatorname{Cl}A-\operatorname{Fr}A=A^\circ$.

_Proof_

a) Assume that $x\in\operatorname{Cl}A$, but that $x\notin A$. We prove first that for each open set $U$ which contains $x$, $U\cap A\neq\phi$. If $A\cap U=\phi$, then $X-U$ is a closed set which contains $A$; hence $\operatorname{Cl}A\subset X-U$. But since $x\in U$, $x$ could not be in $\operatorname{Cl}A$, a contradiction. We have then that for each open set $U$ which contains $x$, $A\cap(U-\{x\})\neq\phi$. But then $x\in A'$. Therefore

$$
\operatorname{Cl}A\subset A\cup A'.
$$

Now suppose that $y\in A\cup A'$. If $y\in A$, then $y\in\operatorname{Cl}A$, since $A\subset\operatorname{Cl}A$. Suppose further that $y\in A'$, and that $F$ is a closed subset of $X$ which contains $A$, but not $y$. Then $X-F$ is open; hence $X-F$ is an open subset which contains $y$ such that $((X-F)-\{y\})\cap A=\phi$. Therefore $y$ could not be in $A'$, a contradiction. Thus $y$ is contained in any closed set which contains $A$, and hence $y\in\operatorname{Cl}A$. This gives $A\cup A'\subset\operatorname{Cl}A$; it follows that

$$
\operatorname{Cl}A=A\cup A'.
$$

<span id="printed-page-60"></span>

<!-- source: PDF 63, printed 60 -->

b) Suppose $x\in\operatorname{Cl}A$, but $x\notin\operatorname{Fr}A$. Since $x\notin\operatorname{Fr}A$, there is some open set $U$ which contains $x$ such that either $U\subset A$, or $U\subset X-A$. If $U\subset X-A$, then $X-U$ is a closed set which contains $A$; therefore $\operatorname{Cl}A\subset X-U$, contradicting the assumption that $x\in\operatorname{Cl}A$. It must be then that $U\subset A$, and hence $x\in A^\circ$. Therefore

$$
\operatorname{Cl}A\subset A^\circ\cup\operatorname{Fr}A.
$$

Assume that $y\in A^\circ\cup\operatorname{Fr}A$, but that $y\notin\operatorname{Cl}A$. Since $A^\circ\subset A\subset\operatorname{Cl}A$, $y$ must be in $\operatorname{Fr}A$. Since $y\notin\operatorname{Cl}A$, there is a closed set $F$ which contains $A$, but not $y$. Then $X-F$ is an open set which contains $y$, but does not intersect $A$; hence $y$ could not be in $\operatorname{Fr}A$, a contradiction. Therefore $y$ must be in $\operatorname{Cl}A$. It follows that $A^\circ\cup\operatorname{Fr}A\subset\operatorname{Cl}A$; hence

$$
\operatorname{Cl}A=A^\circ\cup\operatorname{Fr}A.
$$

c) Suppose $x\in\operatorname{Fr}A$. Then any open set $U$ which contains $x$ meets both $A$ and $X-A$. Then $U$ meets $X-A$ and $X-(X-A)=A$, and hence $x\in\operatorname{Fr}(X-A)$. Thus $x\in\operatorname{Fr}(X-A)$. Similarly, if $x\in\operatorname{Fr}(X-A)$, $x\in\operatorname{Fr}A$.

d) Proposition 12(d) will follow from (b) if we show that

$$
A^\circ\cap\operatorname{Fr}A=\phi.
$$

Suppose $A^\circ\cap\operatorname{Fr}A\neq\phi$, and choose $x$ in this intersection. Then since $x\in\operatorname{Fr}A$, every open set which contains $x$ meets $X-A$. Since $x\in A^\circ$, however, there is an open set $U$ which contains $x$ such that $U\subset A$. Clearly these two possibilities mutually exclude one another; thus it is impossible to have $x$ in both $A^\circ$ and $\operatorname{Fr}A$.

The following terminology is introduced as an aid in making certain statements about topological spaces.

**Definition 8.** Let $X,\tau$ be a topological space. If $x\in X$, then any open set which contains $x$ is said to be a _neighborhood_ of $x$. (Some texts define a neighborhood of $x$ to be any set which contains $x$ in its interior, and refer to what we have defined to be a neighborhood as an _open neighborhood_. Such variations in terminology should be expected, however, in topology, since topology is still a rather young branch of mathematics and much terminology still has not become universally accepted.)

The following proposition relates neighborhoods and the topologically derived sets.

**Proposition 13.** Suppose that $X,\tau$ is any topological space and $A\subset X$.

<span id="printed-page-61"></span>

<!-- source: PDF 64, printed 61 -->

Then

a) $x\in\operatorname{Cl}A$ if and only if every neighborhood of $x$ meets $A$;

b) $x\in A^\circ$ if and only if some neighborhood of $x$ is contained in $A$;

c) $x\in\operatorname{Ext}A$ if and only if $x$ has some neighborhood disjoint from $A$;

d) $x\in\operatorname{Fr}A$ if and only if every neighborhood of $x$ meets both $A$ and $X-A$;

e) $x\in A'$ if and only if every neighborhood of $x$ meets $A$ in some point other than $x$.

_Proof_

a) By Proposition 12(a), $\operatorname{Cl}A=A\cup A'$. If $x\in\operatorname{Cl}A$ and $x\in A$, then every neighborhood of $x$ meets $A$ (in $x$). If $x\in A'$, then every neighborhood of $x$ meets $A$ also. On the other hand, if every neighborhood of $x$ meets $A$, then either $x\in A$, or $x\in A'$; hence

$$
x\in A\cup A'=\operatorname{Cl}A.
$$

b) If $x\in A^\circ$, then there is a neighborhood (open set) $U$ of $x$ such that $U\subset A$ by definition of $A^\circ$. Conversely, if there is a neighborhood $U$ of $x$ with $U\subset A$, then $x\in A^\circ$.

c) If $x\in\operatorname{Ext}A$, then since $\operatorname{Ext}A=X-\operatorname{Cl}A$ is open, $\operatorname{Ext}A$ is a neighborhood of $x$ disjoint from $A$. On the other hand, if there is a neighborhood of $x$ disjoint from $A$, then $x\in X-\operatorname{Cl}A=\operatorname{Ext}A$.

d) and e) are merely the definitions of $\operatorname{Fr}A$ and $A'$ stated in terms of neighborhoods.

**Example 20.** Let $R$ be the set of real numbers with the topology induced by the absolute value metric $D$. Let $A=(0,1]$. Given any $a\in A$, $a\neq1$, it is possible to find a neighborhood of $a$ which lies entirely in $A$ [since $A-\{1\}=(0,1)$ is open]. On the other hand, no neighborhood of 1 lies entirely in $A$; hence $A^\circ=(0,1)$. There are only two points, 0 and 1, with the property that every neighborhood of each of these points meets both $A$ and $R-A$. Therefore $\operatorname{Fr}A=\{0,1\}$. Since $\operatorname{Cl}A=A^\circ\cup\operatorname{Fr}A$, $\operatorname{Cl}A=[0,1]$. If $U$ is a neighborhood of any point $x$ in $[0,1]$, then $U$ intersects $A$ in some point other than $x$; hence $A'=[0,1]$. Note that although $A^\circ\cap\operatorname{Fr}A=\phi$, it is not true in general that $A\cap A'=\phi$. We also have (see Fig. 3.5).

$$
\operatorname{Ext}A=R-\operatorname{Cl}A=\{x\in R\mid x>1,\text{ or }x<0\}
$$

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-3.5.svg" alt="Figure 3.5: An interval and its closure, interior, frontier, exterior, and derived set." /><figcaption>Figure 3.5</figcaption></figure>

<span id="printed-page-62"></span>

<!-- source: PDF 65, printed 62 -->

**Example 21.** Let $N$ be the set of positive integers, and define a subset $U$ of $N$ to be open if $U$ contains all but finitely many positive integers. The set $\tau$ of open subsets of $N$ forms a topology for $N$ (Section 3.1, Exercise 6). Let $A$ be the set of even positive integers. Then $\operatorname{Cl}A=N$, since any open set which contains any integer must contain at least one (in fact an infinite number) of even integers. For if the open set excluded all even integers, it would exclude infinitely many positive integers and hence would not be open. It is true that $A^\circ=\phi$, since any subset of $A$ excludes infinitely many positive integers and hence could not be open. Also,

$$
\operatorname{Fr}A=\operatorname{Cl}A-A^\circ=N-\phi=N.
$$

Note that $\operatorname{Fr}A$ can be larger than $A$. Then $\operatorname{Ext}A=N-\operatorname{Cl}A=\phi$. Finally, $A'=N$, since any neighborhood of any integer contains both even and odd integers.

The notion of _denseness_ is important in topology. Although we will not develop the concept in this chapter, this is an appropriate place to define it.

**Definition 9.** Let $X,\tau$ be a topological space. A subset $A$ of $X$ is said to be _somewhere dense_ if

$$
(\operatorname{Cl}A)^\circ\neq\phi,
$$

that is, if the closure of $A$ contains some open set. $A$ is said to be _nowhere dense_ if $A$ is not somewhere dense. $A$ is said to be _dense_ if

$$
\operatorname{Cl}A=X.
$$

If $A$ is any subset of a topological space such that $A^\circ\neq\phi$, then $A$ is somewhere dense, since $A^\circ\subset A\subset\operatorname{Cl}A$.

**Example 22.** Let $R$ be the set of real numbers with the topology induced by the absolute value metric. The set $A=[0,1)$ is somewhere dense, since $A^\circ=(0,1)\neq\phi$. The set of integers $Z$ is nowhere dense, since $Z$ is closed [$R-Z$ is the union of open sets of the form $(n-1,n)$, $n$ an integer, and hence is open] and no neighborhood of any integer contains only integers; that is,

$$
\operatorname{Cl}Z=Z\quad\text{and}\quad Z^\circ=\phi.
$$

Since any neighborhood of any number contains a rational number, the closure of $Q$, the set of rationals, is all of $R$ (Proposition 13a). Therefore $Q$ is dense in $R$.

The following proposition gives a simple criterion for determining if any given set is dense.

::: warning Missing source page 63
Printed page 63 is absent. The proposition introduced by the final sentence above and any subsequent Chapter 3 text are unavailable. The next supplied page, printed page 64 (PDF page 66), begins Chapter 4, _Derived Topological Spaces. Continuity_. No missing statement or continuation is supplied here.
:::

[Return to Chapter 3](./index)

<style>
.topology-chapter-figure { max-width: 28rem; margin: 1.75rem auto; text-align: center; }
.topology-chapter-figure img { width: 100%; background: white; }
.topology-chapter-figure figcaption { font-family: Georgia, serif; font-size: 0.875rem; margin-top: 0.4rem; }
</style>

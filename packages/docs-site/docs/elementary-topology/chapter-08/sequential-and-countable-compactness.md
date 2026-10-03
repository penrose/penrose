---
title: Sequential and Countable Compactness — Elementary Topology
---

# 8.4 Sequential and Countable Compactness

<span id="printed-page-179"></span>

<!-- Source: PDF169, printed179, Section8.4 fragment. -->

There are certain properties which a space can have which are related to, but do not have the full force of, compactness. We have already seen one such property, local compactness. The purpose of this section is to introduce two other such properties, _sequential compactness_ and _countable compactness_.

**Definition 5.** A space $X,\tau$ is said to be _countably compact_ if any countable open cover of $X$ has a finite subcover. $X$ is said to be _sequentially compact_ if every sequence in $X$ has a convergent subsequence.

Any space which is compact is clearly countably compact. Also, since any open cover of a Lindelöf space has a countable subcover, a countably compact Lindelöf space is compact.

Proposition 13 will give another criterion for countable compactness.

**Proposition 13.** Let $X,\tau$ be any space. Suppose $B\subset X$; then a point $x\in X$ is said to be an _accumulation point_ of $B$ if every neighborhood of $x$ contains infinitely many points of $B$. Then $X,\tau$ is countably compact if and only if every countably infinite subset of $X$ has at least one accumulation point.

_Proof._ Suppose that $B$ is a countably infinite subset of $X$, but that $B$ does not have an accumulation point, and assume $X$ countably compact. Select $x_1,x_2,\ldots,x_n,\ldots$, a sequence of distinct points of $B$. Set

$$
A_n=\{x_n,x_{n+1},\ldots\}\qquad\text{and}\qquad C_n=X-A_n.
$$

Given any point $y\in X$, $y$ is not an accumulation point of $B$; hence there is a neighborhood $U_y$ of $y$ such that $U_y\cap B$ contains at most finitely many elements. Therefore $x_n\in U_y$ for at most finitely many $n$. If $n$ is sufficiently large, then $U_y\cap A_n=\phi$, and therefore $U_y\subset C_n$. Set $V_n=C_n^\circ$. Then $\{V_n\}$, $n\in N$, is a countable open cover of $X$. But since $X$ is countably compact, there is a finite subcover, say $\{V_1,\ldots,V_m\}$, of $\{V_n\}$, $n\in N$. Now $x_{m+1}$ is not an element of $C_1\cup\cdots\cup C_m$; hence $x_{m+1}$ cannot be an element of $V_1\cup\cdots\cup V_m$, a contradiction since $V_1\cup\cdots\cup V_m=X$. Consequently, if $X$ is countably compact, $B$ must have an accumulation point.

<span id="printed-page-180"></span>

<!-- Source: PDF170, printed180. -->

Suppose now that every countably infinite subset of $X$ has an accumulation point, but suppose that we can find a countable open cover $\{U_n\}$, $n\in N$, for which there is no finite subcover. Then for any $n\in N$, $X-\bigcup_{j=1}^n U_j\ne\phi$. Pick $x_1\in X-U_1$; suppose $x\in U_{n_1}$. Then pick

$$
x_2\in X-(U_1\cup\cdots\cup U_{n_1}).
$$

Suppose $x_k$ has been chosen and $x_k\in U_{n_k}$. Choose

$$
x_{k+1}\in X-(U_1\cup\cdots\cup U_{n_k}).
$$

By the manner in which they were chosen, these points must all be distinct; thus the set

$$
B=\{x_k\mid k=1,2,\ldots\}
$$

is a countably infinite subset of $X$. Then $B$ has an accumulation point $y$, and $y\in U_n$ for some $n$. But if $k'$ is large enough, $n<n_{k'}$; hence $x_k\notin U_n$ for $k>k'$. Hence $U_n$ is a neighborhood of $y$ which meets $B$ in only finitely many elements, contradicting the fact that $y$ is an accumulation point of $B$. There must therefore be a finite subcover of $\{U_n\}$, $n\in N$, and hence $X$ is countably compact.

**Proposition 14.** If a space $X,\tau$ is sequentially compact, it is countably compact.

_Proof._ Suppose a space $X,\tau$ is sequentially compact and $B$ is any countable infinite subset of $X$. Then we can find a sequence $\{s_n\}$, $n\in N$, where $s_n\in B$ for each $n$, and no two $s_n$ are equal. Then $\{s_n\}$, $n\in N$, has a convergent subsequence; consequently, $\{s_n\}$, $n\in N$, has a limit point $y$ (Proposition 9, Chapter 6). But then $y$ is accumulation point of $B$. Hence $X$ is countably compact by Proposition 13.

The reader may have already noted that the famous Bolzano-Weierstrass theorem from real analysis is really a statement that any compact subset of $R$ or $R^2$ (that is, a closed, bounded subset) is sequentially compact.

The reader should also note that the distinction between countably compact and sequentially compact is hairline thin. For in any countably compact space, any sequence either takes some value infinitely often, or else the set of points in the sequence is an infinite set and hence has an accumulation point. This does not imply, however, that a sequence has a subsequence which converges, but only that it has a subnet which converges, and the distinction in this case is fine indeed (although we are justified in saying that any first countable space is sequentially compact if and only if it is countably compact). In “nice” topological spaces, sequential and countable compactness are equivalent; examples showing that countable compactness does not imply sequential compactness are rather esoteric.

<span id="printed-page-181"></span>

<!-- Source: PDF171, printed181. -->

One of the nicest types of topological space is the metric space. We have already seen that in a metric space the properties of being second countable, Lindelöf, or separable are all equivalent. We now will show that in a metric space the properties of being countably compact, sequentially compact, or compact are equivalent. We do this in two propositions.

**Proposition 15.** Any countably compact metric space $X,D$ is separable.

_Proof._ For any positive number $p$, there is a maximal subset $E_p$ of $X$ such that for any $a,b\in E_p$, $D(a,b)\ge p$. The proof will closely follow the lines of the proof that (a) implies (b) in Proposition 5, Chapter 7. If $E_p$ were infinite for any $p>0$, then it would have an accumulation point $y$. But then $N(y,p/2)$ would contain infinitely many points of $E_p$; hence any two of these points would be closer together than $p$, a contradiction. Therefore $E_p$ is finite for each $p$. However, given any $x\in X$,

$$
N(x,p)\cap E_p\ne\phi,
$$

or we would have a contradiction to the maximality of $E_p$.

Take $E_{1/n}$ for each positive integer $n$. Then $\bigcup_N E_{1/n}$ is a countable dense subset of $X$, and hence $X$ is separable.

**Corollary 1.** Any countably compact metric space is compact.

_Proof._ By Proposition 5, Chapter 7, any separable metric space is Lindelöf. But any countably compact Lindelöf space is compact.

Since any sequentially compact metric space is countably compact (Proposition 14), we have the following.

**Corollary 2.** Any sequentially compact metric space is compact.

**Proposition 16.** If $X,D$ is any metric space, then the following statements are equivalent:

a) $X$ is compact.

b) $X$ is countably compact.

c) $X$ is sequentially compact.

_Proof._ It remains to be shown that if $X$ is countably compact, then $X$ is sequentially compact. Suppose that $X$ is countably compact and $\{s_n\}$, $n\in N$, is a sequence in $X$. If $s_n=y$ for infinitely many $n$, then a subsequence of $\{s_n\}$, $n\in N$, converges to $y$ (Exercise 1). Suppose then that no value is assumed by the $s_n$ more than a finite number of times; in fact, we lose no generality in assuming that the $s_n$ are all distinct. Since $\{s_n\mid n\in N\}$ is an infinite set, it has an accumulation point $y$. Set $U_n=N(y,1/n)$ for each $n\in N$. Then $U_n\cap\{s_n\}$ is infinite for each $n\in N$. Choose

$$
s_{n_1}\in\{s_n\}\cap U_1.
$$

<span id="printed-page-182"></span>

<!-- Source: PDF172, printed182. -->

Choose $s_{n_2}\in\{s_n\}\cap U_2$, $n_1<n_2$. In general, choose

$$
s_{n_k}\in\{s_n\}\cap U_k,\ n_{k-1}<n_k.
$$

Then $\{s_{n_1},\ldots,s_{n_k},\ldots\}$ is a subsequence of $\{s_n\}$, $n\in N$, which converges to $y$; hence $X$ is sequentially compact.

There are a number of other properties which are in some way related to compactness, for example, _metacompactness_, _pseudocompactness_, and the very important property of _paracompactness_. We shall examine this latter concept briefly in Section 10.5.

::: info Source indices and wording
Proposition 13’s second proof direction prints “suppose $x\in U_{n_1}$” following the selection of $x_1$. Proposition 15’s separation condition includes all $a,b\in E_p$ as printed. The original printed wording of Exercise 8 below remains visible despite handwritten marks in the scan and is retained.
:::

## Exercises

1. Prove that if a sequence assumes some value infinitely many times, then a subsequence of the sequence converges to that value.
2. Prove that in first countable spaces, countable compactness and sequential compactness are equivalent.
3. Suppose $f$ is a continuous function from a space $X,\tau$ onto a space $Y,\tau'$. Prove that if $X$ is countably compact, then $Y$ is also. Prove that if $X$ is sequentially compact, then $Y$ is also.
4. Prove that a closed subset of a countably compact or sequentially compact space is also countably or sequentially compact.
5. Suppose $f$ is a continuous function from a countably compact space $X,\tau$ into the space $R$ of real numbers with the absolute value topology. Prove that there are numbers $m$ and $M$ such that $m\le f(x)\le M$ for all $x\in X$, that is, prove that $f$ is bounded.
6. Let $X$ be any set and suppose $\tau$ and $\tau'$ are two possible topologies on $X$. Suppose $X,\tau$ is compact and $\tau'$ is coarser than $\tau$. Is $X,\tau'$ compact? Suppose $X,\tau$ is countably compact or sequentially compact and $\tau'$ is coarser than $\tau$. Is $X,\tau'$ necessarily countably compact or sequentially compact?
7. Find an example of a separable metric space which is not countably compact. Is it possible to have a nonseparable metric space which is countably compact?
8. Need the set of accumulation points of any set be closed? Need a set together with its accumulation points be closed? Prove that any set together with its accumulation points is closed provided the space is $T_2$.

[Chapter 8 contents](./index.md)

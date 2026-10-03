---
title: Paracompactness. Complete Regularity — Elementary Topology
---

# 10.5 Paracompactness. Complete Regularity

<span id="printed-page-228"></span>

<!-- Source: PDF213, printed228. -->

_Paracompactness_ and _complete regularity_ are topological notions of relatively recent origin, but because they express properties of substantial significance in important areas of modern mathematics, they are widely used in advanced mathematical literature today. It is beyond the scope of this text to discuss these concepts in depth or give much insight into why they are important, but since the reader is likely to encounter them in more advanced work in analysis or topology, we are including their definitions and some of their elementary properties in this section. We first treat paracompactness.

**Definition 4.** An open covering $\{U_i\}$, $i\in I$, of a topological space $X$, is said to be _locally finite_ if each point of $X$ has a neighborhood which meets only finitely many of the $U_i$.

The space $X,\tau$ is _paracompact_ if $X$ is $T_2$ and if each open cover of $X$ has a locally finite open refinement. (For the definitions of _open cover_ and _refinement_, see Definition 1 of Chapter 7.)

Since a finite cover is necessarily locally finite, it follows that any open cover of a compact space has a locally finite open refinement (any finite subcover of the given open cover will do). Since $T_2$ is also necessary for paracompactness, we have:

**Proposition 13.** Any compact $T_2$-space is paracompact.

It is also true that any metrizable space is paracompact though we will not offer any proof of this fact. In fact, paracompactness and metrizability are very closely related as we will see shortly.

It is not necessarily true that the product of even two paracompact spaces is paracompact, nor is it true that every subspace of a paracompact space need be paracompact. We do, however, have the following.

**Proposition 14.** Every closed subspace of a paracompact space is paracompact.

_Proof._ Suppose that $A$ is a closed subspace of the paracompact space $X$ and $\{U_i\}$, $i\in I$, is an open cover of $A$. Then since each $U_i$ is open in $A$, we have $U_i=A\cap V_i$, where $V_i$ is an open subset of $X$, for each $i\in I$. Also

$$
\{V_i\mid i\in I\}\cup\{X-A\}
$$

forms an open cover of $X$, and, hence has a locally finite open refinement $\{W_k\}$, $k\in K$. It follows now that $\{A\cap W_k\}$, $k\in K$, is a locally finite open refinement of $\{U_i\}$, $i\in I$. Therefore $A$ is paracompact.

::: warning Missing printed page 229
The supplied scan omits printed page 229. No missing propositions or proof beginnings are reconstructed. Printed page 230 resumes with the following proof fragment; its preceding definitions of $U$, $G_y$, $F'$, and $V_{x_i}$ are unavailable.
:::

<span id="printed-page-230"></span>

<!-- Source: PDF214, printed230. -->

(where $V_{x_i}$ is the set corresponding to $U_{x_i}$). Then $G_y$ is an open set which contains $y$ but does not meet $U$. Let

$$
V=\bigcup_{y\in F'}G_y.
$$

Then $V$ is an open set which contains $F'$ but does not meet $U$. Therefore $X$ is normal.

Local finiteness and paracompactness are both strongly related to metrizability. We will not review the various metrizability theorems, but will content ourselves with presenting without proof a theorem due to the Russian mathematician Smirnov, who is renowned for his work on metrizability. Two eminent American topologists, John Hocking and Gail Young, cite this theorem as “the most natural metrization theorem” they have seen. We first introduce a definition.

**Definition 5.** A space $X$ is _locally metrizable_ if each point $x\in X$ has a neighborhood which is metrizable (as a subspace).

**Proposition 17.** A locally metrizable $T_2$-space is metrizable if and only if it is paracompact.

The topological concept of _completely regular_ has particular importance in that branch of mathematics known as functional analysis. As with paracompactness, we will content ourselves with presenting the definition and certain basic facts pertinent to this concept.

**Definition 6.** A space $X$ is said to be _completely regular_ (sometimes $T_{3\frac12}$) if given any $x\in X$ and any closed subset $F$ of $X$, $x\notin F$, there is a continuous function $f:X\to[0,1]$ such that $f(x)=0$ and $f(y)=1$ for all $y\in F$.

The space $X$ is said to be _Tychonoff_ if $X$ is $T_1$ and completely regular.

Tychonoff spaces lie between regular and normal spaces in that normal implies Tychonoff, and Tychonoff implies regular. Regular does not necessarily imply completely regular, nor does Tychonoff necessarily imply normal. We will soon see, though, that Tychonoff and normal spaces are rather closely related.

**Proposition 17.** A completely regular space $X$ is $T_3$.

::: info Source numbering
Printed page 230 labels both the locally metrizable theorem and the completely regular theorem “Proposition 17.” Both labels are retained.
:::

_Proof._ Suppose $F$ is a closed subset of $X$ and $x\in X-F$. Then there is a continuous function $f:X\to[0,1]$ such that $f(x)=0$ and $f(y)=1$ for all $y\in F$. Now $[0,\tfrac12)$ and $(\tfrac12,1]$ are disjoint open subsets of $[0,1]$ which contain $0$ and $1$, respectively; hence by the continuity of $f$, $f^{-1}([0,\tfrac12))$ and $f^{-1}((\tfrac12,1])$ are disjoint open subsets of $X$ which contain $x$ and $F$, respectively. Therefore $X$ is $T_3$.

<span id="printed-page-231"></span>

<!-- Source: PDF215, printed231. -->

We have already seen that every locally compact $T_2$-space is regular (Proposition 6 of Chapter 8). We now prove the following stronger result.

**Proposition 18.** Every locally compact $T_2$-space $X$ is Tychonoff.

_Proof._ Since $X$ is $T_2$, $X$ is $T_1$. We now show that $X$ is completely regular. Let $Y$ be the one-point compactification of $X$. Then $Y$ is a compact $T_2$-space (Proposition 12 of Chapter 8) and hence is normal. Suppose $F$ is a closed subset of $X$ and $x\in X-F$. Then $\{x\}$ and $F$ are both disjoint closed subsets of the normal space $Y$; hence there is a continuous function $f:Y\to[0,1]$ such that $f(x)=0$ and $f(y)=1$ for all $y\in F$ (by Urysohn's lemma). By taking $f\mid X:X\to[0,1]$ we obtain a function which proves the complete regularity of $X$.

We leave the proof of the following proposition to the reader.

**Proposition 19.** Any normal space is a Tychonoff space.

Since a compact $T_2$-space is normal, we also have

**Corollary.** Any compact $T_2$-space is Tychonoff.

In fact, compact $T_2$-spaces can be shown to characterize Tychonoff spaces in the following sense.

**Proposition 20.** A space $X$ is Tychonoff if and only if it is homeomorphic to a subspace of a compact $T_2$-space.

We make no attempt to prove Proposition 20. We note, however, that if we found a Tychonoff space which is not normal, then Proposition 20 tells us that we have found a nonnormal subspace of a normal space.

**Proposition 21.**

a) Every subspace of a completely regular space is completely regular; hence every subspace of a Tychonoff space is Tychonoff.

b) If $\{X_n\}$, $n\in N$, is a countable family of completely regular spaces, then the product space $\mathop{\Large\times}_N X_n$ is also completely regular; hence the product of a countable family of Tychonoff spaces is Tychonoff.

_Proof._ We prove (b) and leave (a) as an exercise. Let $F$ be a closed subset of $\mathop{\Large\times}_N X_n$ and $x\in\mathop{\Large\times}_N X_n-F$. Since $\mathop{\Large\times}_N X_n-F$ is open and contains $x$, there is a basic neighborhood $\mathop{\Large\times}_N W_n$ which contains $x$ and fails to meet $F$. All but finitely many $W_n$ are equal to $X_n$. Suppose $W_{n_1},\ldots,W_{n_t}$ are those $W_n\ne X_n$. For $i=1,\ldots,t$, $X_{n_i}-W_{n_i}$ is a closed subset of $X_{n_i}$ which does not contain $x_{n_i}$, then $n_i$th coordinate of $x$. For each $i=1,\ldots,t$, we therefore have a function

$$
g_i:X_{n_i}\to[0,1]
$$

<span id="printed-page-232"></span>

<!-- Source: PDF216, printed232. -->

such that $g_i(x_{n_i})=0$ and $g_i(y)=1$ for each $y\in X_{n_i}-W_{n_i}$. Define $f:\mathop{\Large\times}_N X_n\to[0,1]$ by setting

$$
f(w)=\max\{g_i\circ p_{n_i}(w)\mid i=1,\ldots,t\},
$$

where $p_{n_i}$ is the projection into the $n_i$th component. We leave it to the reader to demonstrate that $f$ is continuous, $f(x)=0$ and $f(y)=1$ for all $y\in F$ (in fact, for all $y\in\mathop{\Large\times}_N X_n-\mathop{\Large\times}_N W_n$). This establishes that $\mathop{\Large\times}_N X_n$ is completely regular.

The importance of completely regular spaces rests partly on the following property.

**Definition 7.** Let $X$ be a topological space and let $C(X,R)$ denote the set of continuous functions from $X$ into $R$, the usual space of real numbers. We say that $C(X,R)$ _separates points_ if given any distinct real numbers $r$ and $s$ and two distinct points $x$ and $y$ of $X$, there is an element $f$ of $C(X,R)$ such that $f(x)=r$ and $f(y)=s$.

**Proposition 22.** If $X$ is Tychonoff, then $C(X,R)$ as described in Definition 7 above separates points.

Complete regularity is also related to metrizability, but we will not develop this aspect of the concept.

## Exercises

1. Prove Proposition 19.

2. Prove Proposition 22.

3. Prove (a) of Proposition 21.

4. Prove that both paracompactness and complete regularity are topological properties, that is, they are preserved by homeomorphisms.

5. Prove that if $X$ is completely regular and $F$ and $F'$ are disjoint subsets of $X$ such that $F$ is closed and $F'$ is compact, then there is a continuous function $f:X\to[0,1]$ such that $f(F)=0$ and $f(F')=1$.

6. Prove that any locally compact $T_2$-space that is the union of a countable number of compact sets is paracompact.

7. As a corollary of Exercise 6 show that any locally compact $T_2$-space is paracompact if it is second countable.

8. Prove that the product of a compact $T_2$-space and a paracompact space is paracompact.

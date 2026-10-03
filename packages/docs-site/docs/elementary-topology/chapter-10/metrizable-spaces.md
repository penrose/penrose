---
title: Metrizable Spaces — Elementary Topology
---

# 10.1 Metrizable Spaces

<span id="printed-page-208"></span>

<!-- Source: PDF195, printed208. -->

This chapter is concerned with two topics pertaining to metric spaces. The first question to be considered is, When is a space a metric space? This of course is a very poor statement of the real issue, since we have already defined what we mean by a metric space, that is, a set with a metric $D$. We have seen, however, that metric spaces have a certain topology induced by their metric. Suppose that instead of starting with a set with a metric, we begin with a topological space $X,\tau$. We might then ask, Is there a metric $D$ which can be defined on $X$ such that the topology induced by $D$ is $\tau$? This is in fact a very profound question, and satisfactory answers to it were not provided until comparatively recently. We shall only partially answer the question in this text.

**Definition 1.** A space $X,\tau$ is said to be _metrizable_ if a metric $D$ can be defined on $X$ such that the topology induced by $D$ is $\tau$. Otherwise, $X$ is said to be _nonmetrizable_.

The question now is, When is a space metrizable?

**Example 1.** As we have seen, a topology can be defined on the space $R$ of real numbers using the open intervals as a basis. This topology is the same topology as is induced by the absolute value metric; hence the open interval topology on $R$ is metrizable.

**Example 2.** If $X,D$ is any metric space, then the product space $X\times X$ can be defined without direct reference to metric. The product topology on $X\times X$ turns out to be the same topology, however, as is induced by the metric $D'$, defined by

$$
D'((x_1,x_2),(y_1,y_2))=D(x_1,y_1)+D(x_2,y_2).
$$

Therefore if $X$ is metrizable, then $X\times X$ is also metrizable. (See Proposition 1, Chapter 8.)

**Example 3.** If $N$ is the set of positive integers with the topology defined by calling a set $U$ open if $U=N-F$, where $F$ is finite, then $N$ is not metrizable. For any metrizable space must be normal, but $N$ is not even $T_2$.

<span id="printed-page-209"></span>

<!-- Source: PDF196, printed209. -->

A theorem which tells us when a space is metrizable is called a metrization theorem. One of the most important metrization theorems is _Urysohn's metrization theorem_.

**Proposition 1.** Let $X,\tau$ be a $T_1$-space. Then the following statements are equivalent:

a) $X$ is regular and second countable.

b) $X$ is separable and metrizable.

c) $X$ is homeomorphic to a subspace of the product space $\mathop{\Large\times}_N[0,1]$, that is, the product of $[0,1]$ with itself countably infinitely many times, where $[0,1]$ has the absolute value topology. The product space $\mathop{\Large\times}_N[0,1]$ is known as the _Hilbert cube_.

As a first step toward proving Proposition 1, we prove the following.

**Proposition 2.** Let $\{X_n,D_n\}$, $n\in N$, be a countable family of second countable metric spaces. Then the product space $\mathop{\Large\times}_N X_n$ is second countable and metrizable.

_Proof._ Since each $X_n$ is second countable, $\mathop{\Large\times}_N X_n$ is second countable by Proposition 4, Chapter 7. We define a metric on $\mathop{\Large\times}_N X_n$ as follows: Let $x$ and $y$ be any points of $\mathop{\Large\times}_N X_n$; denote the $n$th coordinates of $x$ and $y$ by $x_n$ and $y_n$, respectively. Define

$$
D'_n(x_n,y_n)=\min(D_n(x_n,y_n),1).
$$

It is easily proved that the metric $D'_n$ thus defined on $X_n$ is equivalent to the metric $D_n$ (cf. Section 2.3, Exercise 6). Define

$$
D(x,y)=\sum_N\frac{D'_n(x_n,y_n)}{2^n}.
$$

By comparison with the series $\sum_N(\tfrac12)^n$, we see that $D(x,y)$ is defined for each $x$ and $y$ in $\mathop{\Large\times}_N X_n$. It can be verified in a straightforward fashion that $D$ is in fact a metric for $\mathop{\Large\times}_N X_n$. What must now be shown is that the topology induced on $\mathop{\Large\times}_N X_n$ by $D$ is the same as the product topology. We will use Corollary 1, Proposition 9, Chapter 3.

Let $y\in\mathop{\Large\times}_N X_n$, and let $U$ be a basic neighborhood of $y$ in the product topology. Then $U=\mathop{\Large\times}_N W_n$, where $W_n$ is open in $X_n$ and $W_n=X_n$ for all but at most finitely many $n$, say $n_1,\ldots,n_t$. Since $W_n$ is open in $X_n$, we can find positive numbers $p_1,\ldots,p_t$ such that

$$
N(y_{n_i},p_i)\subset W_{n_i},\qquad i=1,\ldots,t.
$$

Choose $p=\min(p_1,\ldots,p_t)$. Then if $z\in N(y,p)$, $z_n\in W_n$ for each $n$; hence $z\in U$, and therefore $N(y,p)\subset U$.

::: info Source formula
The displayed choice $p=\min(p_1,\ldots,p_t)$ is retained as printed. No weighting factor is inserted into this proof.
:::

<span id="printed-page-210"></span>

<!-- Source: PDF197, printed210. -->

On the other hand, suppose $N(y,p)$ is a $D$-$p$-neighborhood of $y$. Choose $q\in N$ such that

$$
\sum_{n=q}^{\infty}(1/2)^n<p/2
$$

and choose positive numbers $p_1,\ldots,p_{q-1}$ such that

$$
p_1+\cdots+p_{q-1}<p/2.
$$

Let $V=\mathop{\Large\times}_N H_n$, where $H_n=N(y_n,p_n)$, $n=1,\ldots,q-1$, and $H_n=X_n$ otherwise. Then $V$ is a basic neighborhood of $y$ in the product topology, and $V\subset N(y,p)$. Therefore the product and metric topologies on $\mathop{\Large\times}_N X_n$ are equivalent; hence $\mathop{\Large\times}_N X_n$ is metrizable.

**Corollary.** The Hilbert cube $\mathop{\Large\times}_N[0,1]$ is second countable and metrizable.

It should be clear to the reader than any subspace of a metrizable space is metrizable (using the same metric which makes the original space into a metric space).

We now proceed to the proof of Proposition 1.

_Proof (Proposition 1)_

Statement (c) implies statement (b). This is true since any subspace of a second countable metric space is both second countable (and hence separable) and itself a metric space. Any metric space is regular and, in metric spaces, separability and second countability are equivalent notions (Proposition 5, Chapter 7); hence (b) implies (a). The difficult part of this proof then is to show that (a) implies (c).

Statement (a) implies statement (c). Since $X$ is second countable, $X$ is Lindelöf (Chapter 7, Proposition 3). Since $X$ is a regular Lindelöf space, $X$ is normal (Proposition 6, Chapter 7). Let

$$
\mathfrak B=\{B_n\mid n=1,2,3,\ldots\}
$$

be a countable basis for $\tau$. There are a countable number of ordered pairs of the form $(B_m,B_n)$ such that $\operatorname{Cl}B_m\subset B_n$ (since $X$ is regular, there is at least one such pair). Since there are countably many such pairs, we will enumerate them

$$
\{(U_k,V_k)\mid k=1,2,3,\ldots,\text{ where }U_k=B_{m_k}\text{ and }V_k=B_{n_k}\}.
$$

Then $\operatorname{Cl}U_k\subset V_k$. Therefore $\operatorname{Cl}U_k$ and $X-V_k$ are disjoint closed subsets of $X$. Applying Urysohn's lemma (Proposition 10, Chapter 5) to $\operatorname{Cl}U_k$ and $X-V_k$, we can find a continuous function $f_k$ from $X$ into $[0,1]$ such that $f_k(x)=0$ for $x\in\operatorname{Cl}U_k$ and $f_k(x)=1$ if $x\in X-V_k$.

::: warning Missing printed page 211
The supplied scan omits printed page 211. The proof continues on that missing page. Printed page 212 resumes with the following final fragment; no missing definition of $F$, $G$, $Z$, $z$, or $z'$ is reconstructed.
:::

<span id="printed-page-212"></span>

<!-- Source: PDF198, printed212. -->

a contradiction to $z'\in N(z,p)$. Therefore $x'\in G$; hence $F(G)$ is an open subset of $Z$, which completes the proof.

Proposition 1 is one of the most important metrization theorems in topology, although there are many others. It does not, however, completely characterize metrizable spaces, since there are nonseparable metric spaces (Example 6, Chapter 7); hence there are metrizable spaces which cannot be homeomorphic to a subspace of the Hilbert cube.

**Corollary.** Any separable metric space $X,D$ has a metrizable compactification.

_Proof._ Since $X$ is a separable metric space, $X$ is homeomorphic to a subspace $Z$ of the Hilbert cube. The Hilbert cube is, however, compact (since it is the product of compact spaces). Therefore $\operatorname{Cl}Z$ is a closed subset of a compact metric space, and is therefore itself a compact metric space. But $\operatorname{Cl}Z$ is a compactification of $X$; hence the desired result.

**Example 4.** Note that Euclidean $n$-space $R^n$ is a separable metric space for each $n$, and that $R^n$ is hence embeddable as a subspace of the Hilbert cube. Since $R$ is homeomorphic to the open interval $(0,1)$ by some homeomorphism $h$, a specific homeomorphism $f$ of $R^n$ in the Hilbert cube might be defined as follows: Let $x=(x_1,\ldots,x_n)$ be any point of $R^n$. Set

$$
f_k(x)=h(x_k),\qquad k=1,\ldots,n,
$$

and

$$
f_k(x)=0\quad\text{for }k>n.
$$

Define

$$
f(x)=(f_1(x),f_2(x),\ldots,f_k(x),\ldots).
$$

It is easily verified that $f$ is a homeomorphism from $R^n$ onto a subspace $Z$ of the Hilbert cube. It can also be confirmed that $\operatorname{Cl}Z$ is homeomorphic to $([0,1])^n$.

We can see from Example 7 of Chapter 8 that there can be more than one metrizable compactification of $R$, and that hence there may be a number of distinct embeddings of $R$ (or $R^n$) as a subspace of the Hilbert cube.

**Example 5.** We stated after Proposition 22 of the last chapter that we would prove that any path in a $T_2$-space is a metric space. We do so now. Any path in a $T_2$-space is a compact subspace of a $T_2$-space and hence is regular (in fact, it is normal). We also saw in the last chapter that any path was second countable. By Proposition 1, then, any path is homeomorphic to a subspace of the Hilbert cube, which is a metric space; hence any path is a metric space.

<span id="printed-page-213"></span>

<!-- Source: PDF199, printed213, section10.1 fragment. -->

## Exercises

1. Which of the following spaces are not homeomorphic to a subspace of the Hilbert cube? If a given space is homeomorphic to a subspace of the Hilbert cube, produce a homeomorphism.

   a) the open interval $(8,11)$ with the absolute value topology

   b) $N$, the space of positive integers with the discrete topology

   c) $\{0,1\}$ with the trivial topology

   d) $\{(x,y)\mid x^2+y^2=4\}\subset R^2$ with the Pythagorean topology

2. The following refer to Proposition 2.

   a) Prove that $D'_n$ is a metric on $X_n$ which is equivalent to the metric $D_n$.

   b) Prove that $D$ is a metric for $\mathop{\Large\times}_N X_n$.

3. Find necessary and sufficient conditions on a space $X,\tau$ which make the one-point compactification of $X$ a separable metric space.

4. Prove or disprove each of the following statements.

   a) If $X$ and $Y$ are each homeomorphic to a subspace of the Hilbert cube, then the product space $X\times Y$ is also.

   b) If $Z$ is a subspace of $X,\tau$ and $X$ is separable and metrizable, then $Z$ is regular and second countable.

   c) A locally compact $T_2$-space is metrizable if and only if it is second countable.

   d) The continuous image of a separable metric space is a separable metric space.

   e) The product space of a countable family of compact separable spaces is homeomorphic to a subspace of the Hilbert cube.

5. Show that there is no finite $n$ such that $([0,1])^n$ could replace the Hilbert cube in the statement of Proposition 1.

6. Define a sequence $\{s_n\}$, $n\in N$, in the Hilbert cube by letting $s_n^n=1$, and $s_n^k=0$ if $k\ne n$, that is, by letting the $n$th coordinate of $s_n$ be $1$ and every other coordinate of $s_n$ be $0$. Since the Hilbert cube is compact, this sequence has a limit point. Find a limit point of this sequence. Does the sequence converge?

7. Suppose a space $X$ has a dense metrizable subspace $Y$. Is $X$ necessarily metrizable? For example, suppose $D$ is a metric on $Y$. For each $x,y\in X$, let $a_n\to x$ and $b_n\to y$, $a_n,b_n\in Y$. Define

   $$
   D(x,y)=\lim D(a_n,b_n).
   $$

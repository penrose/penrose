---
title: T₂-Spaces — Elementary Topology
description: Section 5.2 of the supplied second-edition scan.
---

# 5.2 $T_2$-Spaces

::: info Transcription note
Source: printed pages 95–97 (PDF pages 94–96). Printed pages 95 and 97 are split at section boundaries. The strict total-order relation $<$ in Example 6 and the partial-order relation $\leq$ in Exercise 7 are retained as printed.
:::

<span id="printed-page-95"></span>

<!-- Source: PDF page 94, printed page 95; section 5.2 fragment. -->

A still stronger separation property than being either $T_0$ or $T_1$ is the following.

**Definition 3.** A space $X,\tau$ is said to be $T_2$ if given any two distinct points $x$ and $y$ of $X$, there are open sets $U$ and $V$ such that $x\in U$, $y\in V$, and $U\cap V=\phi$. A $T_2$-space is often called a _Hausdorff space_.

**Example 5.** Every metric space is a $T_2$-space (Section 2.3, Exercise 1).

**Example 6.** Let $X$ be any set which is totally ordered by a relation $<$. Let $\mathfrak{S}$ be the family of all subsets of $X$ of the form $\{x\mid x<a\}$ or $\{x\mid a<x\}$, for all $a\in X$. Then $\mathfrak{S}$ is a subbasis for a topology on $X$ called the _order topology induced by_ $<$. The set $X$ with the order topology is always $T_2$. For suppose $a$ and $b$ are distinct points of $X$. Since $X$ is totally ordered, we may assume $a<b$. If there is $c\in X$ such that $a<c<b$, then

$$
\{x\mid x<c\}\qquad\text{and}\qquad\{y\mid c<y\}
$$

are disjoint neighborhoods of $a$ and $b$, respectively. If there is no $c\in X$ such that $a<c<b$, then

$$
\{x\mid x<b\}\qquad\text{and}\qquad\{y\mid a<y\}
$$

are disjoint neighborhoods of $a$ and $b$, respectively.

Note that for the set $R$ of real numbers, the order topology (for which the family of open intervals forms a basis) and the absolute value topology are the same.

**Proposition 3**

a) Any subspace of a $T_2$-space is $T_2$.

b) Let $Y=\mathop{\Large\times}_I X_i,\tau$ be the product space of the countable family of nonempty spaces $\{X_i,\tau_i\}$, $i\in I$. Then $Y$ is $T_2$ if and only if each $X_i$ is $T_2$.

_Proof_

a) Suppose $W$ is a subspace of the $T_2$-space $X$, and let $x$ and $y$ be distinct points of $W$. Then there are open sets $U$ and $V$ in $X$ such <span id="printed-page-96"></span><!-- Source: PDF page 95, printed page 96. --> that $x\in U$, $y\in V$, and $U\cap V=\phi$. But $x\in U\cap W$, $y\in V\cap W$, and

$$
(U\cap W)\cap(V\cap W)=(U\cap V)\cap W=\phi\cap W=\phi.
$$

Therefore $U\cap W$ and $V\cap W$ are disjoint neighborhoods in $W$ of $x$ and $y$, respectively. Hence $W$ is $T_2$.

b) Assume that each $X_i,\tau_i$ is $T_2$, and let $x$ and $y$ be distinct points of $Y$. We will use $x_i$ and $y_i$ to denote the $i$th coordinate of $x$ and $y$, respectively. Since $x\ne y$, $x_i\ne y_i$ for at least one $i\in I$, say for $i'$. Therefore there are open sets $U_{i'}$ and $V_{i'}$ in $X_{i'}$ such that

$$
x_{i'}\in U_{i'},\qquad y_{i'}\in V_{i'},\qquad\text{and}\qquad U_{i'}\cap V_{i'}=\phi.
$$

Set $U=\mathop{\Large\times}_I H_i$, where $H_i=X_i$, $i\ne i'$, and $H_{i'}=U_{i'}$; and set $V=\mathop{\Large\times}_I G_i$, where $G_i=X_i$, $i\ne i'$, and $G_{i'}=V_{i'}$. Then $U$ and $V$ are neighborhoods of $x$ and $y$, respectively. Since any point of $U$ differs from any point of $V$ at least in the $i'$th coordinate, $U\cap V=\phi$. Therefore $Y$ is $T_2$.

By Proposition 20, Chapter 4, each $X_i,\tau_i$ is homeomorphic to a subspace of $Y$ (regardless of whether $Y$ is $T_2$). If $Y$ is $T_2$, then every subspace of $Y$ is $T_2$, by (a). Therefore if $Y$ is $T_2$, each $X_i,\tau_i$ is homeomorphic to a $T_2$-space, and hence is $T_2$ (Exercise 1).

**Example 7.** The reader might conjecture from Proposition 3 that if $X,\tau$ is a $T_2$-space and if $R$ is an equivalence relation on $X$, then the identification space $X/R$ is also $T_2$. This is not true. For example, let $R^2$ be the plane with the Pythagorean topology. Let $E$ be the equivalence relation defined by the partition

$$
\bigl\{\{(x,y)\mid y<0\},\ \{(x,y)\mid y\geq 0\}\bigr\}.
$$

Here $\{(x,y)\mid y<0\}$ is open, whereas $\{(x,y)\mid y\geq 0\}$ is not open. Therefore $R^2/E$ is homeomorphic to the set $X=\{0,1\}$ with the topology $\{X,\phi,\{0\}\}$, which is not $T_2$. Since the identification mapping from $R^2$ onto $R^2/E$ is continuous, we have also shown that the property of being $T_2$ is not preserved by continuous functions (although, of course, it is preserved by homeomorphisms).

## Exercises

1. Suppose that the space $X,\tau$ is homeomorphic to the space $Y,\tau'$ and that $X$ is $T_2$. Prove that $Y$ is $T_2$.

2. Prove that a space $X,\tau$ is $T_2$ if and only if, given any two distinct points $x$ and $y$ of $X$, there is a neighborhood $U$ of $x$ such that $y\notin\operatorname{Cl}U$.

<span id="printed-page-97"></span>

<!-- Source: PDF page 96, printed page 97; section 5.2 fragment. -->

3. Prove that a space $X,\tau$ is $T_2$ if and only if the diagonal $\Delta=\{(x,x)\mid x\in X\}$ is a closed subset of the product space $X\times X$.

4. Suppose that $X,\tau$ is a $T_2$-space and that $A\subset X$. Prove that $x\in\operatorname{Cl}A$ if and only if $x\in A$, or each neighborhood of $x$ contains infinitely many points of $A$.

5. Assume that $f$ is a function from a set $X$ onto a $T_2$-space $Y,\tau'$. Assume further that $X$ is given the topology $\tau$, defined by taking a subset $U$ of $X$ to be open if $U=f^{-1}(V)$, where $V$ is an open subset of $Y$. Is $X$ with this topology necessarily $T_2$?

6. Suppose $f$ is a function from a $T_2$-space $X,\tau$ onto a space $Y,\tau'$ such that $f$ is one-one and $f^{-1}$ is continuous. Prove that $Y$ is a $T_2$-space.

7. Let $X$ be a set partially ordered by $\leq$. Define $\mathfrak{S}$ to be the collection of subsets of $X$ of the form $\{x\mid x<a\}$ or $\{y\mid a<y\}$, for all $a\in X$. Is $\mathfrak{S}$ necessarily the subbasis for a topology on $X$? If it is a subbasis for a topology $\tau$ on $X$, is $X,\tau$ a $T_2$-space?

8. Suppose $X$ is a $T_2$-space. Is it necessarily true that given any subbasis $\mathfrak{S}$ for the topology on $X$ and any two distinct points $x$ and $y$ of $X$, there are disjoint members $U$ and $V$ of $\mathfrak{S}$ with $x\in U$ and $y\in V$? Answer this question with _subbasis_ replaced by _basis_.

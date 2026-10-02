---
title: Local Compactness — Elementary Topology
---

# 8.2 Local Compactness

<span id="printed-page-168"></span>

<!-- Source: PDF159, printed168, Section8.2 fragment. -->

There are times when a topological space possesses some property “locally” which it does not have taken as a whole. For example, a second countable space has a countable basis for its topology. A space $X,\tau$ may not be second countable, but could still have the property that there is an open neighborhood system for $\tau$ such that for any $x\in X$, $\mathfrak{N}_x$ is countable; we called such a space _first countable_. In a sense, a first countable space is a space which is locally second countable. Similarly, a space may not be compact, but still have the property that each point is contained in each member of an “appropriate” family of compact sets.

::: info Figure 8.3 — Penrose reproduction pending
[View Figure 8.3 on its source page](../reader?page=168).
:::

**Example 3.** Let $R^2$ be the coordinate plane with the Pythagorean metric topology. Then $R^2$ is not compact, since it is not bounded (Proposition 2). If $x\in R^2$ and $U$ is any neighborhood of $x$, then there is $p>0$ such that $N(x,p)\subset U$. Then

$$
N(x,p/2)\subset\operatorname{Cl}N(x,p/2)\subset N(x,p)\subset U
$$

(Fig. 8.3). But $\operatorname{Cl}N(x,p/2)$ is a closed, bounded subset of $R^2$, and hence is compact. We have therefore proved that if $x\in R^2$ and $U$ is any neighborhood of $x$, then there is a compact set $A$ [here $\operatorname{Cl}N(x,p/2)$] such that $x\in A^\circ$ [here $N(x,p/2)$] $\subset A\subset U$. A similar property could be proved for $R^n$, $n$ finite.

This example inspires the following definition of _local compactness_.

<span id="printed-page-169"></span>

<!-- Source: PDF160, printed169. -->

**Definition 2.** A space $X,\tau$ is said to be _locally compact_ if given any $x\in X$ and any neighborhood $U$ of $x$, there is a compact set $A$ such that

$$
x\in A^\circ\subset A\subset U.
$$

Thus $R^n$ is locally compact. The criterion for local compactness is much simpler for $T_2$-spaces, as we see from the next proposition.

**Proposition 5.** Let $X,\tau$ be a $T_2$-space. Then $X$ is locally compact if and only if given any $x\in X$, there is a compact set $A$ such that $x\in A^\circ$. (In other words, the existence of one compact subset $A$ of $X$ such that $x\in A^\circ$ assures us that given any neighborhood $U$ of $x$, there is a compact set $A'$ such that $x\in A'^\circ\subset A'\subset U$.)

_Proof._ Suppose $X$ is locally compact and $x\in X$. Since $X$ is a neighborhood of $x$, there is a compact set $A$ such that

$$
x\in A^\circ\subset A\subset X.
$$

Suppose instead that given any $x\in X$, there is at least one compact set $A$ with $x\in A^\circ$. Let $U$ be any neighborhood of $x$. Then $A^\circ\cap U$ is a neighborhood of $x$ and is a subset of $U$; hence we lose no generality in assuming that $U$ is already a subset of $A^\circ$. Now $A$ is compact and $T_2$, and hence the subspace $A$ is $T_3$ (Corollary 2, Proposition 11, Chapter 7); moreover, $U\cap A=U$ (since $U\subset A^\circ\subset A$) is a nonempty subset of $A$ which is open in $A$. Therefore there is $V$, open in both $A$ and in $X$, such that

$$
x\in V\subset\operatorname{Cl}V\text{ (in }A\text{)}\subset U\subset A^\circ
$$

(Proposition 4, Chapter 5). Since $A$ is a compact subset of a $T_2$-space, $A$ is closed; thus

$$
\operatorname{Cl}V\text{ (in }X\text{)}=\operatorname{Cl}V\text{ (in }A\text{)}.
$$

Then as a closed subset of a compact $T_2$-space, $\operatorname{Cl}V$ is compact. Therefore

$$
x\in V=V^\circ\subset\operatorname{Cl}V\subset U,
$$

and $\operatorname{Cl}V$ is compact; hence $X$ is locally compact.

**Corollary.** Any compact $T_2$-space $X,\tau$ is locally compact.

_Proof._ $X$ is a compact neighborhood of any $x\in X$.

**Example 4.** The space $X$ given in Example 10, Chapter 7 is only $T_1$, but is still locally compact. For if $x\in X$, then any neighborhood $U$ of $X$ is compact. Therefore $x\in U=U^\circ\subset U$ and $U$ is compact; hence $X$ is locally compact.

<span id="printed-page-170"></span>

<!-- Source: PDF161, printed170. -->

::: info Figure 8.4 — Penrose reproduction pending
[View Figure 8.4 on its source page](../reader?page=170).
:::

**Example 5.** Let $Q$ be the subspace of rational numbers in the space $R$ of real numbers with the absolute value topology. Then $Q$ is not locally compact. Let $x\in Q$ and suppose $A$ is a compact subset of $Q$ such that $x\in A^\circ$ (Fig. 8.4). Then $A$ contains infinitely many elements of $Q$. There is $(a,b)\subset R$ such that $x\in(a,b)\cap Q\subset A^\circ$. Choose an irrational number $t\in(a,b)$. We will now construct an open cover of $A$ which has no finite subcover. For each $q\in A$, set

$$
U(q)=\begin{cases}\{w\in R\mid q<w\},&\text{if }t<q,\\\{z\in R\mid z<q\},&\text{if }q<t.\end{cases}
$$

Then $\{U(q)\cap A)$, $q\in A$, is an open cover of $A$ which has no finite subcover. The proof of this fact is left as an exercise.

We see then that no element of $Q$ can be contained in the interior of any compact subset of $Q$. Therefore $Q$ is an example of a metric space in which not every closed bounded subset is compact. For example, $[0,1]\cap Q$ is a closed, bounded subset of $Q$, but could not be compact; for if it were compact, then $\frac12$ would be contained in the interior, $(0,1)\cap Q$, of a compact subset of $Q$.

We saw in Chapter 7 that any compact $T_2$-space was $T_4$. Since local compactness is a weaker property than compactness, we should expect weaker results from local compactness than from compactness, as is the case with the following.

**Proposition 6.** Any locally compact $T_2$-space $X,\tau$ is $T_3$.

_Proof._ We apply Proposition 4 of Chapter 5. If $x\in X$ and $U$ is any neighborhood of $x$, then there is a compact set $A$ such that $x\in A^\circ\subset A\subset U$. Since $A$ is compact, $A$ is closed, and hence $\operatorname{Cl}(A^\circ)\subset A$. Setting $V=A^\circ$, we have $x\in V\subset\operatorname{Cl}V\subset U$, where $V$ is a neighborhood of $x$; therefore $X$ is $T_3$.

We see from Example 5 that a subspace of a locally compact space need not be locally compact. We do, however, have the following proposition regarding subspaces of locally compact spaces.

**Proposition 7.** If a space $X,\tau$ is $T_2$ and locally compact, then so is every open or closed subspace.

_Proof._ Suppose $U$ is an open subspace of $X$ and $x\in U$. Then any neighborhood $V$ of $x$ in $U$ is also a neighborhood of $x$ in $X$. Therefore there is a compact set $A$ such that $x\in A^\circ\subset A\subset V\subset U$. Hence $U$ is locally <span id="printed-page-171"></span><!-- Source: PDF162, printed171. -->compact. Note that this part of the proof did not depend on the fact that $X$ was $T_2$; thus we have shown that any open subspace of any locally compact space is locally compact.

Suppose $F$ is a closed subspace of $X$ and $x\in F$. Let $A$ be any compact set such that $x\in A^\circ\subset A$. Since $X$ is $T_2$, $A$ is closed. Then $F\cap A$ is a closed subset of the compact set $A$ and hence is compact. But $F\cap A\subset F$; hence we also have $x\in(A\cap F)^\circ\text{ in }F\subset A\cap F\subset F$. Since $F$ is $T_2$, $F$ is locally compact by Proposition 5.

We now prove an even stronger result.

**Proposition 8.** A subspace $Y$ of a locally compact $T_2$-space $X,\tau$ is locally compact if and only if it is the intersection of an open set and a closed set.

_Proof._ Suppose $Y$ is a locally compact subspace of $X$ (Fig. 8.5). We will prove that $Y$ is open in $\operatorname{Cl}Y$; hence $Y=U\cap\operatorname{Cl}Y$, where $U$ is an open subset of $X$. Suppose $y\in Y$; we must find a neighborhood of $y$ (in $\operatorname{Cl}Y$) which is a subset of $Y$. Since $Y$ is locally compact, there is a set $U'$ open in $Y$ such that $y\in U'$ and $\operatorname{Cl}U'$ in $Y$ is compact. Then $U'=Y\cap V$, where $V$ is open in $X$. Furthermore, $\operatorname{Cl}U'$ in $Y=Y\cap\operatorname{Cl}(Y\cap V)$ is compact, and hence is closed. Now

$$
Y\cap V\subset Y\cap\operatorname{Cl}(Y\cap V);
$$

hence $\operatorname{Cl}(Y\cap V)\subset Y$. But

$$
\operatorname{Cl}Y\cap V\subset\operatorname{Cl}(Y\cap V).
$$

For if $z\in\operatorname{Cl}Y\cap V$ and $W$ is any neighborhood of $z$, $V\cap W$ is a neighborhood of $z$. Since $z\in\operatorname{Cl}Y$, every neighborhood of $z$ meets $Y$, and thus

$$
(V\cap W)\cap Y=W\cap(Y\cap V)\ne\phi.
$$

::: info Figure 8.5 — Penrose reproduction pending
[View Figure 8.5 on its source page](../reader?page=171).
:::

But then every neighborhood of $z$ meets $Y\cap V$ as well; hence

$$
z\in\operatorname{Cl}(Y\cap V).
$$

Therefore $\operatorname{Cl}Y\cap V\subset Y$. Thus $\operatorname{Cl}Y\cap V$ is a neighborhood of $y$ in $\operatorname{Cl}Y$ such that $\operatorname{Cl}Y\cap V\subset Y$.

It is left as an exercise to show that the intersection of a closed subset and an open subset of $X$ is locally compact.

We now investigate the behavior of locally compact spaces with regard to continuous functions. The following example shows that local compactness, unlike compactness, is not preserved by continuous functions.

<span id="printed-page-172"></span>

<!-- Source: PDF163, printed172. -->

::: info Figure 8.6 — Penrose reproduction pending
[View Figure 8.6 on its source page](../reader?page=172).
:::

**Example 6.** Let $A=\{-1\}$ and $B=\{x\mid0<x\}$ (Fig. 8.6). Let $X=A\cup B$ be given the absolute value topology. Then $X$ is the intersection of a closed subset of $R$, the usual space of real numbers, with an open subset of $R$ [for example, $X=(\{-1\}\cup\{x\mid0<x\})\cap(R-\{0\})$]; hence $X$ is locally compact. Define a function $f$ from $X$ into $R^2$ (with the Pythagorean topology) by

$$
f(x)=\begin{cases}(0,0),&\text{if }x\in A,\\(x,\sin1/x),&\text{if }x\in B.\end{cases}
$$

Then $f\mid A$ and $f\mid B$ are both continuous, and $A$ and $B$ are both closed subsets of $X$; thus $f$ is continuous (Proposition 11, Chapter 4). Let $Y$ be the image of $f$ considered as a subspace of $R^2$. The function $f$ is a continuous, one-one function from $X$ onto $Y$. But $Y$ is not locally compact. This can be seen from the fact that $(0,0)$ is not contained in the interior of any compact subset of $Y$. This in turn follows from the fact that each neighborhood $U$ in $Y$ of $(0,0)$ contains a sequence which does not have a limit point in $\operatorname{Cl}U$.

A function $f:X,\tau\to Y,\tau'$ is said to be _open_ if whenever $U$ is an open subset of $X$, $f(U)$ is open in $Y$ (cf. Section 4.6, Exercise 2). With the added assumption of openness, a continuous function will preserve local compactness.

**Proposition 9.** If $f$ is a continuous, open function from a space $X,\tau$ onto a space $Y,\tau'$, then if $X$ is locally compact, $Y$ is also.

_Proof._ Suppose $y\in Y$ and $U$ is a neighborhood of $y$. We must find a compact subset $A$ of $Y$ such that $y\in A^\circ\subset A\subset U$. Let $y=f(x)$ for some $x\in X$. By the continuity of $f$, there is a neighborhood $V$ of $x$ such that $f(V)\subset U$. Since $X$ is locally compact, there is a compact set $B$ such that $x\in B^\circ\subset B\subset V$. Then

$$
f(x)=y\in f(B^\circ)\subset f(B)\subset U.
$$

But $f(B^\circ)$ is open, since $f$ is open; and $f(B)$ is compact, since $B$ is compact <span id="printed-page-173"></span><!-- Source: PDF164, printed173. -->and $f$ is continuous. Therefore

$$
y\in f(B^\circ)\subset f(B^\circ)\subset f(B)\subset U,
$$

and hence $Y$ is locally compact.

We now use Proposition 9 to study the relation between local compactness and product spaces.

**Proposition 10.** Suppose $\mathop{\Large\times}_I X_i$ is the product space of the countable family of nonempty spaces $\{X_i,\tau_i\}$, $i\in I$. Then $\mathop{\Large\times}_I X_i$ is locally compact if and only if each component space is locally compact and all of the component spaces except at most finitely many are compact.

_Proof._ Suppose $\mathop{\Large\times}_I X_i$ is locally compact. Then the projection

$$
p_i:\mathop{\Large\times}_I X_i\to X_i
$$

is continuous, onto, and open (Section 4.6, Exercise 2) for each $i\in I$. Therefore by Proposition 9, each $X_i$ is locally compact. We must also show that all but at most finitely many of the $X_i$ are compact. Let $A$ be any compact subset of $\mathop{\Large\times}_I X_i$ such that some point $y$ of $\mathop{\Large\times}_I X_i$ is in $A^\circ$. Then there is a basic neighborhood $\mathop{\Large\times}_I V_i$ of $y$ such that $V_i=X_i$ for all but at most finitely many $i$ and

$$
\mathop{\Large\times}_I V_i\subset A^\circ\subset A.
$$

We therefore see that $p_i(A)=X_i$ for all but at most finitely many $i$. Since $p_i$ is continuous and $A$ is compact, $X_i$ is compact for all but at most finitely many $i$.

Suppose each $X_i$ is locally compact, and all but finitely many of the $X_i$ are compact. Let $y\in\mathop{\Large\times}_I X_i$, and let $y_i$ be the $i$th coordinate of $y$. If $U$ is any neighborhood of $y$, then $U$ contains a basic neighborhood of $y$ of the form $\mathop{\Large\times}_I V_i$, where $V_i$ is open in $X_i$ for each $i\in I$ and $V_i=X_i$ for all $i\in I$, except for at most finitely many, say $i_1,\ldots,i_n$. Since each $X_i$ is locally compact, for each $i\in I$ there is a compact subset $A_i$ of $X_i$ such that $y_i\in A_i^\circ\subset A_i\subset V_i$. There are at most finitely many more $i\in I$, other than $i_1,\ldots,i_n$, say $i_{n+1},\ldots,i_m$, such that $X_{i_{n+1}},\ldots,X_{i_m}$ are not compact. For any $i$ not in $\{i_1,\ldots,i_n,i_{n+1},\ldots,i_m\}$, we may let $A_i=X_i$. Then

$$
y\in\mathop{\Large\times}_I A_i^\circ\subset\left(\mathop{\Large\times}_I A_i\right)^\circ\subset\mathop{\Large\times}_I A_i\subset\mathop{\Large\times}_I V_i.
$$

But $\mathop{\Large\times}_I A_i$ is the product of compact sets and is therefore compact (Proposition 13, Chapter 7). Hence $\mathop{\Large\times}_I X_i$ is locally compact.

::: info Source wording and formulas
Example 4 prints “neighborhood $U$ of $X$.” Example 5’s cover uses a closing parenthesis after $A$ in its first occurrence; Exercise 3 below uses a brace. Example 6’s displayed decomposition and Proposition 9’s repeated $f(B^\circ)$ are retained as printed.
:::

<span id="printed-page-174"></span>

<!-- Source: PDF165, printed174, Section8.2 fragment. -->

## Exercises

1. In Example 6 show that each neighborhood $U$ of $(0,0)$ in $Y$ contains a sequence which does not converge to any point of $\operatorname{Cl}U$ (in $Y$). Why does this prove that $(0,0)$ is not contained in the interior (in $Y$) of any compact subset of $Y$?
2. Prove that any subspace which is the intersection of a closed subset and an open subset of a locally compact $T_2$-space is locally compact, thus completing the proof of Proposition 8. [*Hint:* Use Proposition 7.]
3. Provide the details for Example 5. In particular, show that $\{U(q)\cap A\}$, $q\in A$, is an open cover of $A$ which has no finite subcover.
4. Which of the following subspaces of the plane $R^2$ with the Pythagorean topology are locally compact?

   a) $R^2-\{(0,0)\}$

   b) $\{(x,y)\mid x\text{ and }y\text{ are both rational}\}$

   c) $\{(x,y)\mid x\text{ and }y\text{ are both integers}\}$

   d) $R^2-\bigcup_N\{C\mid C\text{ is a circle of radius }1/n\text{ with center }(0,0)\}$, where $N$ is the set of positive integers

   e) $R^2-\{(x,y)\mid x^2+y^2<1,\text{ or }x=0\text{ or }1,\text{ and }y=0\text{ or }1\}$

5. If $X,\tau$ is a space and $R$ is an equivalence relation on $X$, is the identification mapping from $X$ onto $X/R$, the identification space, necessarily open? If $X$ is locally compact, must $X/R$ be locally compact? Give examples to prove your points.
6. Is the union of finitely many locally compact subspaces of any space always a locally compact subspace? Is the intersection of two locally compact subspaces locally compact?
7. Suppose $X,\tau$ is a locally compact $T_2$-space which is second countable. Prove that $X$ is the union of countably many compact subsets $A_1,A_2,\ldots,A_n,\ldots$ such that $A_n\subset A_{n+1}^\circ$. Example: $R^2=\bigcup_N D_n$, where $D_n=\operatorname{Cl}N((0,0),n)$.
8. Find an example of a compact space which is not locally compact.

[Chapter 8 contents](./index.md) · [Next: 8.3 Compactifications](./compactifications.md)

---
title: Continuity and Convergence — Elementary Topology
description: The available text of section 6.6 of the supplied second-edition scan.
---

# 6.6 Continuity and Convergence

::: info Transcription note
Source: printed pages 130–132 (PDF pages 125–127). Printed page 130 begins with the remaining §6.5 exercises, transcribed separately. The source's coordinate superscripts, quotient-class bars, and example interval are retained.
:::

<span id="printed-page-130"></span>

<!-- Source: PDF page 125, printed page 130; section 6.6 fragment. -->

The following proposition relates the convergence of nets and the continuity of functions. It is a generalization of Proposition 10 of Chapter 2, thus strengthening the assertion that nets are a good generalization of sequences.

**Proposition 10.** Let $f$ be a function from a space $X,\tau$ to a space $Y,\tau'$. Then $f$ is continuous if and only if for every net $\{s_i\}$, $i\in I$, in $X$ such that $s_i\longrightarrow x$, the net $\{f(s_i)\}$, $i\in I$, in $Y$ converges to $f(x)$.

_Proof._ Suppose $f$ is continuous, but also suppose there is a net $\{s_i\}$, $i\in I$, in $X$ such that $s_i\longrightarrow x$, but $\{f(s_i)\}$, $i\in I$, does not converge to $f(x)$. Then there is a subnet of $\{f(s_i)\}$, $i\in I$, no subnet of which converges to $f(x)$ (Proposition 6). Let

$$
\{f(s_{k_j})\},\qquad j\in J,
$$

be such a subnet. Then there is a neighborhood $V$ of $f(x)$ for which $\{f(s_{k_j})\}$, $j\in J$, is residually not in $V$. But since $f$ is continuous, $f^{-1}(V)$ is a neighborhood of $x$. Since $s_i\longrightarrow x$, $\{s_i\}$, $i\in I$, is residually in $f^{-1}(V)$. But $\{s_{k_j}\}$, $j\in J$, is a subnet of $\{s_i\}$, $i\in I$, which is not residually in $f^{-1}(V)$. Therefore $\{s_i\}$, $i\in I$, has a subnet which does not converge to $x$, a contradiction to Proposition 5.

<span id="printed-page-131"></span>

<!-- Source: PDF page 126, printed page 131. -->

Suppose for each net $\{s_i\}$, $i\in I$, in $X$ such that $s_i\longrightarrow x$, $f(s_i)\longrightarrow f(x)$, but $f$ is not continuous. Then there is a neighborhood $V$ of $f(x)$ such that for no neighborhood $U$ of $x$ do we have $f(U)\subset V$. Let $T(X,x)$ be the directed set of all neighborhoods of $x$. Let $s$ be a selection function from $T(X,x)$ into $X$ such that $f(s(U))\notin V$ for all $U\in T(X,x)$. Then $s_U\longrightarrow x$, but $f(s_U)\not\longrightarrow f(x)$, a contradiction.

We now use Proposition 10 to prove several results about nets in derived topological spaces.

**Proposition 11.** Let $X,\tau$ be any space and $R$ be an equivalence relation on $X$. For each $x\in X$, let $\bar{x}$ denote the equivalence class of $x$. Then if $s_i\longrightarrow x$ in $X$, $\bar{s}_i\longrightarrow\bar{x}$ in the identification space $X/R$.

_Proof._ The function defined by $x\longrightarrow\bar{x}$ is continuous. We then apply Proposition 10.

**Proposition 12.** Suppose $\{s_i\}$, $i\in I$, is a net in the product space $\mathop{\Large\times}_J X_j$. We will denote the $j$th coordinate of $s_i$ by $s_i^j$; thus $\{s_i^j\}$, $i\in I$, will be a net in $X_j$. Then

$$
s_i\longrightarrow y=(y_1,y_2,\ldots,y_j,\ldots)
$$

(remember we have restricted our attention in this text to the product of countably many spaces) if and only if

$$
s_i^j\longrightarrow y_j.
$$

_Proof._ Suppose $s_i\longrightarrow y$. Then the projection mapping from $\mathop{\Large\times}_J X_j$ into $X_j,\tau_j$ is continuous. Therefore, by Proposition 10,

$$
p_j(s_i)=s_i^j\longrightarrow p_j(y)=y_j.
$$

Suppose $s_i^j\longrightarrow y_j$ for each $j\in J$. Let $U$ be a typical basic neighborhood for $y$ in the product topology. Then $U=\mathop{\Large\times}_J W_j$, where each $W_j$ is an open subset of $X_j$; in particular, each $W_j$ is a neighborhood of $y_j$. Also $W_j=X_j$ for each $j\in J$, except finitely many, say $j_1,\ldots,j_n$. For each $j\in J$, except $j_1,\ldots,j_n$, $s_i^j\in W_j$. For $j_1,\ldots,j_n$ we can find $i_1,\ldots,i_n$ in $I$ such that if $i_q\leq i$, $s_i^{j_q}\in W_{j_q}$, $q=1,\ldots,n$. Since $I$ is directed, we can find, using induction if necessary, $i_0\in I$ such that if $i_0\leq i$, $s_i^{j_q}\in W_{j_q}$, $q=1,\ldots,n$. Thus if $i_0\leq i$, $s_i^j\in W_j$ for each $j\in J$, and hence if $i_0\leq i$,

$$
s_i\in\mathop{\Large\times}_J W_j.
$$

Therefore $\{s_i\}$, $i\in I$, is residually in $\mathop{\Large\times}_J W_j$. Since $\mathop{\Large\times}_J W_j$ was a typical basic neighborhood and every neighborhood of $y$ contains such a basic <span id="printed-page-132"></span><!-- Source: PDF page 127, printed page 132. --> neighborhood, $\{s_i\}$, $i\in I$, is residually in every neighborhood of $y$; therefore $s_i\longrightarrow y$.

It is not true that if $Y$ is a subspace of $X$, $\{s_i\}$, $i\in I$, converges in $X$, and $s_i\in Y$ for each $i\in I$, then $\{s_i\}$, $i\in I$, considered as a net in $Y$ also converges. This is not even true for sequences, as we see from the following example.

**Example 14.** Let $R$ be the set of real numbers with the absolute value topology. Then the sequence defined by $s_n=1/n$ converges to 0 in $R$, but does not converge at all in the subspace $(0,1)$, since $(0,1)$ does not contain the limit 0. The following is true, however.

**Proposition 13**

a) If $s_i\longrightarrow y$ in $X$ and $Y$ is a subspace of $X$ such that $s_i\in Y$ for each $i$ and $y\in Y$, then $s_i\longrightarrow y$ in $Y$ also.

b) If $s_i\longrightarrow y$ in $X$ and $Y$ is a closed subspace of $X$, then if each $s_i\in Y$, then $y\in Y$ as well, and $s_i\longrightarrow y$ in $Y$.

_Proof._ The proof of (a) is left as an exercise. Statement (b) follows immediately from (a) and Proposition 7.

## Exercises

1. Prove Proposition 13.

2. Let $\{X_j,\tau_j\}$, $j\in J$, be a countable family of nonempty spaces, and consider the product $\mathop{\Large\times}_J X_j$ of the sets $\{X_j\}$, $j\in J$. How much of Proposition 12 is true if $\mathop{\Large\times}_J X_j$ is given a topology coarser than the product topology? How much of Proposition 12 is true if $\mathop{\Large\times}_J X_j$ is given a topology finer than the product topology?

3. Prove that a function $f$ from a space $X,\tau$ onto a space $Y,\tau'$ is a homeomorphism if and only if a net $\{s_i\}$, $i\in I$, converges to $x\in X$ if and only if $\{f(s_i)\}$, $i\in I$, converges to $f(x)$ in $Y$.

4. Using the results of this chapter, prove that if $f$ is a continuous function from $X,\tau$ into $Y,\tau'$, then $f(\operatorname{Cl}A)\subset\operatorname{Cl}f(A)$ for any $A\subset X$.

5. Also using methods from this chapter, prove Proposition 21, Chapter 4.

6. Suppose $f$ is a continuous function from $X,\tau$ into $Y,\tau'$. Let $\{s_i\}$, $i\in I$, be a net in $X$, and suppose $A$ is the set of limit points of this net. Prove that $f(A)$ is a set of limit points for $\{f(s_i)\}$, $i\in I$. Is it necessarily a complete set of limit points for $\{f(s_i)\}$, $i\in I$, or might there be others as well? [Hint: Consider the sequence defined by $s_n=1/n$ in $(0,1)$, and the identity function from $(0,1)$ into the space of real numbers.]

7. Let $X/R$ be an identification space of a space $X$. Discuss the conditions $R$ must satisfy for the following to hold: For any net $\{s_i\}$, $i\in I$, in $X$ which converges to a point $x$, $\{\bar{s}_i\}$, $i\in I$, the identification net in $X/R$ converges only to $\bar{x}$.

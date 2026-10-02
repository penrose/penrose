---
title: The Need for a Generalized Notion of Convergence — Elementary Topology
description: The available text of section 6.1 of the supplied second-edition scan.
---

# 6.1 The Need for a Generalized Notion of Convergence

::: info Transcription note
Source: printed pages 113–116 (PDF pages 110–113). Printed page 116 also begins §6.2, transcribed separately. The original distinction between sequences and general convergence is retained.
:::

<span id="printed-page-113"></span>

<!-- Source: PDF page 110, printed page 113. -->

The reader will recall that we have already discussed convergence of sequences in metric spaces in Chapter 2. He may therefore suspect that extending the theory of convergence to general topological spaces will merely consist of rewording the definition of a convergent sequence in terms of a general space. For example, we might say: A sequence $\{s_n\}$, $n\in N$, in a space $X,\tau$ converges to a limit $y$ in $X$ if every neighborhood of $y$ contains all but finitely many of the $s_n$.

Actually, however, not only will we find it necessary to generalize the notion of convergence of a sequence, but we will also have to generalize the very notion of a sequence as well. The purpose of this section is to illustrate this point. We begin by proving a proposition concerning sequences in metric spaces.

**Proposition 1.** Let $A$ be a subset of a metric space $X,D$. Then $y\in\operatorname{Cl}A$ if and only if there is a sequence $\{s_n\}$, $n\in N$, such that

$$
s_n\longrightarrow y\qquad\text{and}\qquad s_n\in A
$$

for each $n\in N$.

_Proof._ Suppose $y\in\operatorname{Cl}A$. Then if $y\in A$, let $\{s_n\}$, $n\in N$, be the sequence defined by $s_n=y$ for all $n\in N$. Then

$$
s_n\longrightarrow y\qquad\text{and}\qquad s_n\in A
$$

for all $n\in N$. Suppose $y\in\operatorname{Cl}A-A$. By Proposition 12, Chapter 3, $\operatorname{Cl}A=A\cup A'$; therefore $y\in A'$. Then each neighborhood of $y$ contains at least one element of $A$. Let $U_n$ be the $D$-$1/n$-neighborhood of $y$ for each positive integer $n$. For each $n$, select $s_n\in U_n\cap A$. The sequence $\{s_n\}$, $n\in N$, thus obtained converges to $y$, and each $s_n$ is in $A$.

On the other hand, suppose $y$ is such that there is a sequence $\{s_n\}$, $n\in N$, such that $s_n\longrightarrow y$, and $s_n\in A$ for each $n\in N$. Since $s_n\longrightarrow y$, each neighborhood of $y$ contains all but finitely many of the $s_n$. Therefore each neighborhood of $y$ contains some point of $A$. By Proposition 13, Chapter 3, then $y\in\operatorname{Cl}A$.

<span id="printed-page-114"></span>

<!-- Source: PDF page 111, printed page 114. -->

If the notion of a sequence were sufficient for the study of general topological spaces, we would expect that this proposition relating closures and sequences generalizes to arbitrary topological spaces. That is, if $A\subset X$, where $X,\tau$ is a topological space, then $y\in\operatorname{Cl}A$ if and only if, etc. There are a number of reasons why we would like this proposition to generalize. The fact is, though, that it does not generalize using sequences. This is shown by the following example.

**Example 1.** Let $X$ be the set of all functions from the set $R$ of real numbers into $R$. We make no assumption about the continuity of these functions. We will define a topology on $X$ by specifying an open neighborhood system. Suppose $f$ is any element of $X$. Let $F$ be any finite subset of $R$ and $p$ be any positive real number. Define

$$
U(f,F,p)=\{g\in X\mid |g(x)-f(x)|<p\text{ for all }x\in F\}.
$$

Let $\mathfrak{N}_f$ be the set of all $U(f,F,p)$ for all finite subsets $F$ of $R$ and all positive numbers $p$. Note that for a given $f$, $U(f,F,p)$ depends on both $F$ and $p$; hence $U(f,F,p)$ is not a $p$-neighborhood in the metric sense.

We will now show that this definition of $\mathfrak{N}_f$ for each $f\in X$ gives us an open neighborhood system for a topology on $X$. In accordance with Proposition 7 of Chapter 3, we must show that (i) through (iv) of Definition 5, Chapter 3 are satisfied.

Statements (i) and (ii) are clearly satisfied.

iii) Suppose $U(f,F_1,p_1)$ and $U(f,F_2,p_2)$ are any two members of $\mathfrak{N}_f$. Then

$$
U(f,F_1\cup F_2,\min(p_1,p_2))
$$

is an element of $\mathfrak{N}_f$ which is contained in $U(f,F_1,p_1)\cap U(f,F_2,p_2)$. For suppose

$$
g\in U(f,F_1\cup F_2,\min(p_1,p_2));
$$

we may assume $p_1\leq p_2$. It follows that

$$
|g(x)-f(x)|<p_1\leq p_2
$$

for each $x\in F_1$ and each $x\in F_2$. Therefore

$$
g\in U(f,F_1,p_1)\cap U(f,F_2,p_2).
$$

iv) Suppose $U(f,F,p)\in\mathfrak{N}_f$ and $g\in U(f,F,p)$. Let

$$
F=\{x_1,\ldots,x_n\}
$$

and

$$
q_i=p-|f(x_i)-g(x_i)|,\qquad i=1,\ldots,n.
$$

<span id="printed-page-115"></span>

<!-- Source: PDF page 112, printed page 115. -->

Set $p'=\min(q_1,\ldots,q_n)$. Then

$$
U(g,F,p')\subset U(f,F,p)
$$

(Exercise 1). Therefore the $\mathfrak{N}_f$ do form an open neighborhood system for a topology on $X$.

Let $A$ be the set of all elements $f$ in $X$ such that $f(x)=0$ or 1 for any $x\in R$, and $f(x)=0$ for at most countably many $x\in R$. Suppose $g$ is the function defined by $g(x)=0$ for all $x\in R$. We will show that $g\in\operatorname{Cl}A$. For let $U(g,F,p)$ be any basic neighborhood of $g$. Let $h$ be the function defined by $h(x)=0$ for $x\in F$ and $h(x)=1$ if $x\notin F$. Then

$$
h\in A\cap U(g,F,p).
$$

Therefore any neighborhood of $g$ meets $A$; hence $g\in\operatorname{Cl}A$.

Suppose there is a sequence $\{f_n\}$, $n\in N$, such that $f_n\in A$ for each $n\in N$, and $f_n\longrightarrow g$. Let $B_n=\{x\mid f_n(x)=0\}$. Each $B_n$ is a countable subset of $R$; hence $\bigcup_N B_n$ is also a countable subset of $R$. Since $R$ is uncountable, we can find $z\in R-\bigcup_N B_n$. Let $F=\{z\}$ and $p=1/2$. Consider $U(g,F,p)$. Then no matter what $n$ is, $z\notin B_n$, and hence $f_n(z)=1$. Therefore

$$
|f_n(z)-g(z)|=1.
$$

There is no positive integer $n$, then, for which $f_n\in U(g,F,p)$. But $U(g,F,p)$ is a neighborhood of $g$; thus if $f_n\longrightarrow g$, $U(g,F,p)$ would have to contain all but finitely many of the $f_n$. It is impossible then that $f_n\longrightarrow g$.

What we have here, then, is a topological space for which Proposition 1 of this chapter does not hold. What are we to do? There are two alternatives, either (1) we can restrict ourselves merely to sequences and say that Proposition 1 has no generalization, or (2) we can try to generalize the notion of a sequence in such a way that Proposition 1 can be generalized to any topological space. It is this latter alternative that we choose.

## Exercises

1. In Example 1, prove (iv) in the proof that the $\mathfrak{N}_f$ form an open neighborhood system.

2. A topological space $X,\tau$ is said to be _first countable_ if there is an open neighborhood system for $\tau$ such that $\mathfrak{N}_x$ is a countable collection for each $x\in X$.

   a) Prove that every metric space is first countable.

   b) Prove that Proposition 1 holds for any topological space which is first countable. Thus the space in Example 1 is not first countable.

   c) Prove that any subspace of a first countable space is first countable.

   <span id="printed-page-116"></span>
   <!-- Source: PDF page 113, printed page 116; section 6.1 fragment. -->

   d) Prove that the product space of a countable family of nonempty spaces is first countable if and only if each component space is first countable.

   e) Which of the spaces mentioned in Section 5.3, Exercise 6 are first countable?

3. Do you think that the following might be an appropriate generalization of sequences? Let $I$ be any set. Then an $I$-sequence in a space $X,\tau$ will be a function $s$ from $I$ into $X$. We will denote the $I$-sequence by $\{s_i\}$, $i\in I$, where $s_i$ denotes $s(i)$. We will say that $\{s_i\}$, $i\in I$, converges to $y$ if any neighborhood of $y$ contains all but finitely many of the $s_i$. For example, suppose $I=(0,1)$ and $s$ is the identity function on $I$ considered as a function from $I$ into the space of real numbers. Does $\{s_i\}$, $i\in I$, converge to 1 according to our definition of convergence of a $I$-sequence? Does it seem as though it should if this is a suitable generalization of the notion of a sequence?

4. Let $X$ be any uncountable set. For each $x\in X$, define

   $$
   \mathfrak{N}_x=\{N\subset X\mid x\in N\text{ and }N\text{ excludes at most countably many points of }X\}.
   $$

   Prove that the collection of $\mathfrak{N}_x$ forms an open neighborhood system for a topology on $X$. Does Proposition 1 apply to $X$ with this topology?

5. Find two distinct topologies on the set $R$ of real numbers such that the only sequences which converge relative to each of the topologies are those sequences which are constant from some term on, and these sequences converge only to their constant value. (Use Exercise 4 and the notion of convergence introduced in the first paragraph of this chapter.) Since the two topologies have the same convergent sequences converging to the same limits, sequences alone are inadequate to characterize either topology.

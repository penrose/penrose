---
title: Compactness in Euclidean Space — Elementary Topology
---

# 8.1 Compactness in $R^n$

<span id="printed-page-163"></span>

<!-- Source: PDF155, printed163. -->

The product space $R^n$ of the space $R$ of real numbers with the absolute value topology with itself $n$ times, better known as _Euclidean n-space_, is perhaps the most important topological space of all (or, more accurately, family of spaces, since there is a space for each positive integer $n$). Compact subsets of $R^n$ therefore hold a special place among compact sets and warrant a special section to study them.

We have already seen that $[0,1]\subset R$ is compact. Thus any subspace of $R^n$ homeomorphic to $[0,1]$ is compact. More generally, any continuous image of $[0,1]$ in $R^n$ is compact. Using the various propositions already proved, we can find many compact subsets of $R^n$. However, $R^n$ has many properties not shared by all topological spaces. We would therefore expect there to be certain criteria for compactness which are more peculiar to $R^n$. Proposition 2 gives such a criterion. Preparatory to Proposition 2, we first prove the following.

**Proposition 1.** Suppose $x=(x_1,\ldots,x_n)$ and $y=(y_1,\ldots,y_n)$ are any two points of $R^n$. Define $D(x,y)=\max(|x_i-y_i|,\ i=1,\ldots,n)$ (cf. Examples 3 and 6 of Chapter 2). Then $D$ is a metric on $R^n$. Moreover, the topology induced on $R^n$ by $D$ is the same as the product topology on $R^n$.

_Proof._ The proof that $D$ is actually a metric is straightforward and is left as an exercise. Let $\tau$ be the product topology on $R^n$ and $\tau'$ be the topology induced by $D$. In order to prove $\tau=\tau'$, we will use Corollary 1, Proposition 9, Chapter 3. Suppose $x=(x_1,\ldots,x_n)\in R^n$. Set

$$
\mathfrak{N}_x=\left\{\mathop{\Large\times}_{i=1}^n N(x_i,p_i)\ \middle|\ \begin{array}{l}\text{where }p_i>0,\\N(x_i,p_i)=(x_i-p_i,x_i+p_i)\subset R,\ i=1,\ldots,n\end{array}\right\},
$$

and

$$
\mathfrak{N}'_x=\{N'(x,p)\mid p>0,\ \text{where }N'(x,p)\text{ is the }D\text{-}p\text{-neighborhood of }x\text{ in }R^n\}.
$$

<span id="printed-page-164"></span>

<!-- Source: PDF156, printed164. -->

Then taking the collection of all $\mathfrak{N}_x$ and the collection of $\mathfrak{N}'_x$ for all $x\in R^n$, we get open neighborhood systems for $\tau$ and $\tau'$, respectively.

Suppose $N'\in\mathfrak{N}'_x$. Then

$$
N'=\mathop{\Large\times}_{i=1}^n N(x_i,p)
$$

and hence is a member of $\mathfrak{N}_x$. (For a picture of a typical $N'$ in $R^2$, the reader should see Fig. 4, Chapter 2.) Suppose $N\in\mathfrak{N}_x$. Then

$$
N=\mathop{\Large\times}_{i=1}^n N(x_i,p_i),
$$

where $p_i>0$, $i=1,\ldots,n$. Set $p=\min(p_1,\ldots,p_n)$. Then $N'(x,p)\in\mathfrak{N}'_x$ and $N'(x,p)\subset N$. Therefore by Corollary 1, Proposition 9, Chapter 3, $\tau=\tau'$.

**Proposition 2.** A subset $A$ of $R^n$ is said to be _bounded_ if there is a positive number $p$ such that $A\subset N'(\bar0,p)$, where $\bar0$ is the origin in $R^n$ and $N'(\bar0,p)$ is the $D$-$p$-neighborhood of $\bar0$ described in Proposition 1. A subset $A$ of $R^n$ is compact if and only if $A$ is closed and bounded.

_Proof._ Proposition 1 has shown that $R^n$ with the product topology is a metric space (with metric $D$ as in Proposition 1). In Section 7.4, Exercise 7, it was shown that any compact subset of any metric space is closed and bounded.

::: info Figure 8.1 — Penrose reproduction pending
[View Figure 8.1 on its source page](../reader?page=164).
:::

Suppose $A$ is a closed, bounded subset of $R^n$ (Fig. 8.1). Then, since $A$ is bounded, $A\subset N'(\bar0,p)$ for some positive number $p$; hence

$$
A\subset\operatorname{Cl}N'(\bar0,p).
$$

Now

$$
A\subset\operatorname{Cl}N'(\bar0,p)\subset\mathop{\Large\times}_{i=1}^n\operatorname{Cl}N(0,p)=\mathop{\Large\times}_{i=1}^n[-p,p].
$$

But $[-p,p]$ is compact since it is homeomorphic to $[0,1]$; hence

$$
\mathop{\Large\times}_{i=1}^n\operatorname{Cl}N(0,p)
$$

<span id="printed-page-165"></span>

<!-- Source: PDF157, printed165. -->

is compact since it is the product of a family of compact spaces (Proposition 13, Chapter 7). Therefore $A$ is a closed subset of a compact $T_2$-space (any metric space is $T_2$), and hence $A$ is compact (Corollary 1, Proposition 11, Chapter 7).

**Corollary.** The closure of any bounded subset of $R^n$ is compact.

_Proof._ Suppose $A$ is bounded. Then

$$
A\subset N'(\bar0,p)\subset\operatorname{Cl}N'(\bar0,p+1).
$$

Therefore $\operatorname{Cl}A\subset N'(\bar0,p+1)$. $\operatorname{Cl}A$ is closed, and thus $\operatorname{Cl}A$ is closed and bounded, and is therefore compact.

The next proposition is true in any compact metric space, but has its application primarily in the study of real functions.

**Proposition 3.** Let $X,D$ be any compact metric space and suppose $\{U_i\}$, $i\in I$, is an open cover of $X$. Then there is a positive number $p$ such that $N(x,p)\subset U_i$ for some $i$, for any $x\in X$. That is, there is $p>0$ such that the $p$-neighborhood of any point in $X$ is a subset of at least one of the $U_i$. Such a number $p$ is called a _Lebesgue number_ of the cover, and is dependent on the cover for its value.

_Proof._ Each element $x$ of $X$ is contained in at least one $U_i$, since $\{U_i\}$, $i\in I$, is a cover of $X$. Since each $U_i$ is also open, for each $x\in X$, we may select $p_x>0$ such that $N(x,p_x)\subset U_i$ for at least one of the $U_i$ which contain $x$. Since a selection has been made for each $x$, $\{N(x,p_x/2)\}$, $x\in X$, is itself an open cover of $X$ (in fact, it is a refinement of the original cover). Since $X$ is compact, we can find a finite number of the elements of $X$, say $x_1,\ldots,x_n$, such that

$$
\{N(x_1,p_{x_1}/2),\ldots,N(x_n,p_{x_n}/2)\}
$$

is an open cover of $X$. Let

$$
p=\min(p_{x_1}/2,\ldots,p_{x_n}/2).
$$

We now show that $p$ is a Lebesgue number for $\{U_i\}$, $i\in I$. If $x\in X$, then $x\in N(x_j,p_{x_j}/2)$ for some $1\le j\le n$. If $z\in N(x,p)$, then

$$
D(z,x_j)\le D(z,x)+D(x,x_j)<p+p_{x_j}/2\le p_{x_j}.
$$

Therefore

$$
N(x,p)\subset N(x_j,p_{x_j})\subset U_i
$$

for some $i$.

We recall that the definition of continuity of a function from one metric space to another can be expressed: A function $f:X,D\to Y,D'$ is

::: warning Missing printed page 166
The preceding sentence ends at the end of printed page 165. Printed page 166 is absent. Its continuation, any intervening definitions, examples, and Proposition 4’s statement are unavailable. Printed page 167 begins with the following proof fragment.
:::

<span id="printed-page-167"></span>

<!-- Source: PDF158, printed167. -->

_Proof._ Choose any $p>0$. Then $\{N(y,p/2)\}$, $y\in Y$, is an open cover of $Y$. Since $f$ is continuous, $\{f^{-1}(N(y,p/2))\}$, $y\in Y$, is an open cover of $X$. Let $q$ be the Lebesgue number of this cover in accordance with Proposition 3. It is left as an exercise to prove that this $q$ has the desired property that

$$
f(N(x,q))\subset N(f(x),p)
$$

for each $x\in X$.

**Corollary.** Any function from a closed, bounded subset of $R^n$ into any metric space is uniformly continuous. In particular, any function from a closed interval $[a,b]\subset R$ is uniformly continuous.

::: info Source wording
The corollary prints “Any function” without adding a continuity assumption. That source wording is retained. Exercise 5(a) also retains the printed composition order $f\circ g$.
:::

## Exercises

1. The following refer to the proof of Proposition 1.

   a) Prove that $D$ is a metric.

   b) Prove that the collection of all $\mathfrak{N}_x$ and the collection of all $\mathfrak{N}'_x$, are open neighborhood systems for $\tau$ and $\tau'$, respectively.

2. Complete the proof of Proposition 4.
3. A function $f$ from a space $X,\tau$ into the space of real numbers is said to be _bounded above_ if $f(x)\le M$ for some number $M$ and each $x\in X$. What would we mean if we said that $f$ was _bounded below_? Prove that if $X$ is compact and $f$ is continuous, then $f$ is bounded above and below. Prove that if $M=\text{least upper bound }\{f(x)\mid x\in X\}$ and $m=\text{greatest lower bound }\{f(x)\mid x\in X\}$, $f$ is continuous, then there are $w$ and $y$ in $X$ such that $f(w)=M$ and $f(y)=m$.
4. Which of the functions defined below from $R$ into $R$ are uniformly continuous?

   a) $f(x)=x+2$, for all $x\in R$

   b) $f(x)=4x+7$, for all $x\in R$

   c) $f(x)=\begin{cases}x\sin(1/x),&x\ne0\\0,&\text{if }x=0\end{cases}$

5. Let $X,D$; $Y,D'$ and $Z,D''$ be metric spaces. Decide which of the following statements are true. If a statement is true, prove it; if false, find a counterexample.

   a) If the function $f$ from $X$ to $Y$ and the function $g$ from $Y$ to $Z$ are both uniformly continuous, then $f\circ g:X\to Z$ is also uniformly continuous.

   b) Suppose $X$ and $Y$ are both the set of real numbers and $D$ and $D'$ are the absolute value metric. Then if $f$ and $g$ are uniformly continuous functions from $X$ to $Y$, then $f+g$ defined by $(f+g)(x)=f(x)+g(x)$ is also uniformly continuous.

   c) If $f$ is a homeomorphism from $X$ onto $Y$ and $f$ is uniformly continuous, then $f^{-1}$ is also uniformly continuous.

<span id="printed-page-168"></span>

<!-- Source: PDF159, printed168, Section8.1 fragment. -->

6. A metric space $X,D$ is said to be _totally bounded_ if given any $p>0$, the open cover $\{N(x,p)\}$, $x\in X$, has a finite subcover. Prove that any bounded subset of $R^m$ (with the metric described earlier) is totally bounded. Prove that a compact subset of $X,D$ is closed and totally bounded. Show that a subset of $X,D$ may be closed and totally bounded yet not be compact.

[Chapter 8 contents](./index.md) · [Next: 8.2 Local Compactness](./local-compactness.md)

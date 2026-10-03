---
title: Normality and the Extension of Functions — Elementary Topology
description: The available text of section 5.5 of the supplied second-edition scan.
---

# 5.5 Normality and the Extension of Functions

::: info Transcription note
Source: printed pages 106–109 and 111–112 (PDF pages 104–109). Printed page 110 is missing; the end of Proposition 10's proof, Proposition 11, and its corollary are unavailable. The printed formula $f(0)=0$ on page 109 and the wording of Exercise 4 are retained. Figure 5.13 is referenced in the prose but has not been located in the supplied scan. The editorial notices identify these limitations without supplying missing text.
:::

<span id="printed-page-106"></span>

<!-- Source: PDF page 104, printed page 106; section 5.5 fragment. -->

One of the central and most difficult questions in all of topology is that of function extensions. Specifically, suppose that $Y$ is a subspace of a space $X,\tau$ and that $f$ is a continuous function from $Y$ into some space $Z,\tau'$. Does there exist a function $F$ from $X$ into $Z$ such that $F$ is continuous and $F(y)=f(y)$ for each $y\in Y$? That is, is there a continuous function

$$
F:X,\tau\longrightarrow Z,\tau'
$$

such that $F\mid Y=f$? The answer is sometimes yes and sometimes no. For most instances, the answer is not known.

**Example 14.** If $Y$ is a subspace of $X,\tau$ and $i$ is the identity function on $Y$, then $i$ is a continuous function from $Y$ into $X$. Of course $i$ can be extended to the identity function $I$ for all of $X$. In this case, $I$ is an extension of $i$ since $I\mid Y=i$. This is a rather trivial and therefore uninteresting type of extension.

A function may have several extensions. For if we let $X=\{1,2\}$ with the discrete topology and $Y=\{1\}$, then $I'$ defined by $I'(1)=1$, $I'(2)=1$, is an extension of $i$ which is different from $I$.

**Example 15.** Let $X$ be the closed interval $[0,1]$ with the usual absolute value topology, and let $Y=Z=\{0,1\}$ with the subspace topology (which is the discrete topology in this case). Let $i$ be the identity function on $Y$. Then $i$ is continuous as a function from $Y$ to $Z$. Although we cannot prove it at this time (we will be able to do so later in the book), $i$ cannot be extended to a continuous function from $[0,1]$ into $Z$. We may see this informally as follows: If there were a continuous function $F$ from $X$ onto $Z$ (as there would have to be if $i$ could be extended), then, since $\{0\}$ and $\{1\}$ are both open subsets of $Z$, $F^{-1}(\{0\})$ and $F^{-1}(\{1\})$ would both <span id="printed-page-107"></span><!-- Source: PDF page 105, printed page 107. --> be open subsets of $X$; moreover,

$$
X=F^{-1}(\{0\})\cup F^{-1}(\{1\})\qquad\text{and}\qquad F^{-1}(\{0\})\cap F^{-1}(\{1\})=\phi.
$$

Thus $X=[0,1]$ would be expressible as the union of two disjoint, nonempty, open subsets. The reader should try to express $[0,1]$ as the union of two such subsets in order to see the intuitive difficulties of such a decomposition.

Topological spaces which are $T_4$ are, however, bound up essentially with some very important extension properties. In fact, $T_4$-spaces can be characterized by certain of their extension properties. This is proved in the following proposition, one of the most important propositions in topology.

**Proposition 10 (Urysohn's lemma).** A topological space $X,\tau$ is $T_4$ if and only if given any disjoint nonempty closed subsets $A$ and $B$ of $X$, there is a continuous function $f$ from $X$ into $Z=[0,1]$ (with the absolute value topology) such that

$$
f(a)=0\text{ for any }a\in A\qquad\text{and}\qquad f(b)=1\text{ for any }b\in B.
$$

Before proving this proposition, we note that it is indeed a proposition dealing with function extensions. Explicitly, if $X,\tau$ is a $T_4$-space and $Y$ is a subspace of $X$ which can be expressed as the union of two disjoint nonempty closed subsets of $X$, then the function $g:Y\longrightarrow[0,1]$ such that

$$
g(a)=0\text{ for all }a\in A\qquad\text{and}\qquad g(b)=1\text{ for all }b\in B
$$

can be extended to a continuous function $f:X\longrightarrow[0,1]$. Although this may appear as a rather modest result about function extensions because of the restrictions that have been placed upon $Y$ and $g$, very general and important results flow from Urysohn's lemma. We shall mention a few of these after the proof.

_Proof (Proposition 10)._ Suppose $X$ has the property described and $A$ and $B$ are any two disjoint nonempty subsets of $X$. Then there is a continuous function $f$ from $X$ into $Z=[0,1]$ such that

$$
f(a)=0\text{ for all }a\in A\qquad\text{and}\qquad f(b)=1\text{ for all }b\in B.
$$

Now $U'=\{x\mid0\leq x<1/2\}$ and $V'=\{x\mid1/2<x\leq1\}$ are disjoint open subsets of $Z$; therefore, since $f$ is continuous, $U=f^{-1}(U')$ and $V=f^{-1}(V')$ are disjoint open subsets of $X$. But $A\subset U$ and $B\subset V$, and hence $X$ is $T_4$.

Suppose now that $X$ is $T_4$. Recall that a space $X,\tau$ is $T_4$ if and only if given any closed subset $F$ of $X$ and any open set $U$ which contains $F$, <span id="printed-page-108"></span><!-- Source: PDF page 106, printed page 108. --> there is an open set $V$ which contains $F$ such that

$$
F\subset V\subset\operatorname{Cl}V\subset U
$$

(Proposition 7). Suppose $A$ and $B$ are disjoint nonempty closed subsets of $X$ (Fig. 5.11). Consider the set of rational numbers $q$ such that $0\leq q\leq1$ and $q$ is of the form $q=n/2^k$, where $n$ and $k$ are positive integers. For example, $1/2$, $3/2^2=3/4$, and $5/2^3=5/8$ are such rational numbers. With each such rational $q$ we will associate an open subset $U(q)$ of $X$ such that

1. $A\subset U(q)$;
2. $B\cap U(q)=\phi$;
3. if $q<q'$, then $\operatorname{Cl}U(q)\subset U(q')$.

<figure>
  <img src="/elementary-topology/figures/figure-5.11.svg" alt="A inside U(q), its closure inside U(q′), with B outside both." />
  <figcaption>Figure 5.11</figcaption>
</figure>

Since $X$ is $T_4$ and $A$ and $B$ are disjoint closed subsets of $X$, there are disjoint open sets $U$ and $V$ such that $A\subset U$ and $B\subset V$. We let $U=U(0)$ and $X-B=U(1)$. Using Proposition 7, we can find an open set, which we let be $U(\frac12)$, such that

$$
\operatorname{Cl}U(0)\subset U(\tfrac12)\subset\operatorname{Cl}U(\tfrac12)\subset U(1).
$$

Similarly, we can find $U(\frac14)$ and $U(\frac34)$ such that

$$
\operatorname{Cl}U(0)\subset U(\tfrac14)\subset\operatorname{Cl}U(\tfrac14)\subset U(\tfrac12)
$$

and

$$
\operatorname{Cl}U(\tfrac12)\subset U(\tfrac34)\subset\operatorname{Cl}U(\tfrac34)\subset U(1).
$$

We will continue finding the $U(q)$ by induction on $k$, the exponent of 2 in $q=n/2^k$. Note that we have already defined $U(q)$ for $k=1$ and $k=2$.

Assume we have defined $U(q)$ for $k$. We now define $U(q)$ for $k+1$ (and thus for $n=1,3,\ldots,2^{k+1}-1$). Note that the definition of $U(q)$ needs to be given only for odd $n$; for if $n$ were even, the numerator and denominator of $q$ could be divided by 2. Because the $U(q)$ have already been constructed for $q=n/2^k$, $n$ odd, we have

$$
\operatorname{Cl}U\left(\frac{n-1}{2^{k+1}}\right)\subset U\left(\frac{n+1}{2^{k+1}}\right)
$$

[since $n$ is odd, $\operatorname{Cl}U((n-1)/(2^{k+1}))=\operatorname{Cl}U(((n-1)/2)/2^k)$, which has already been defined]. We therefore can find an open set, which we let be <span id="printed-page-109"></span><!-- Source: PDF page 107, printed page 109. --> $U(n/2^{k+1})$ such that

$$
\operatorname{Cl}U((n-1)/2^{k+1})\subset U(n/2^{k+1})\subset\operatorname{Cl}U(n/2^{k+1})\subset U((n+1)/2^{k+1})
$$

(Fig. 5.12). We thus have an inductive definition of $U(q)$ for each $q$ as described. By construction, the collection of $U(q)$ have properties (1) through (3) given above.

<figure>
  <img src="/elementary-topology/figures/figure-5.12.svg" alt="Three nested dyadic neighborhoods around A, retaining the source's printed indices." />
  <figcaption>Figure 5.12</figcaption>
</figure>

We now define a function $f$ from $X$ into $Z=[0,1]$ such that $f(a)=0$ for all $a\in A$ and $f(b)=1$ for all $b\in B$. If $x\in X$, define $f(x)=1$, if $x\in B$. If $x$ is not in $B$, then $x\in U(1)$. For each $x$ not in $B$, define

$$
f(x)=\text{greatest lower bound }\{q\mid q=n/2^k\text{ and }x\in U(q)\}
$$

(this set of real numbers has a lower bound, 0, and hence has a greatest lower bound). Certainly $0\leq f(x)\leq1$. If $x\in A$, then $x\in U(0)$; therefore $f(0)=0$. We now prove that $f$ is continuous.

Suppose $f(x_0)=y_0$. First, assume that $y_0$ is neither 0 nor 1. Then, given any positive number $p$, there are rationals $q$ and $q'$ of the form $n/2^k$ such that

$$
y_0\in(q,q')\subset(y_0-p,y_0+p)
$$

(that is, the set of “binary” rationals is dense in $[0,1]$. Alternately, any real number can be approximated to an arbitrary degree of accuracy by a rational of the form $n/2^k$). Then (Fig. 5.13) $V=U(q')-\operatorname{Cl}U(q)$ is a neighborhood of $x_0$, and

$$
f(V)\subset(y_0-p,y_0+p).
$$

::: info Figure 5.13 — unlocated in the supplied scan
The printed reference above is retained. No captioned Figure 5.13 has been located on the available source pages. [View the reference on printed page 109](../reader?page=109).
:::

If $y_0$ is either 0 or 1, then the corresponding neighborhoods of 0 and 1, respectively, are $[0,q')$ and $(q,1]$; but the argument is the same. What we have shown is that, given any neighborhood $H$ of $y_0$ (any neighborhood

::: warning Missing source page 110
The preceding sentence ends at the end of printed page 109. Printed page 110 is absent. The completion of this proof, Proposition 11, and its corollary are unavailable. Printed page 111 begins with the following proof fragment; its missing statement has not been reconstructed.
:::

<span id="printed-page-111"></span>

<!-- Source: PDF page 108, printed page 111. -->

<figure>
  <img src="/elementary-topology/figures/figure-5.14.svg" alt="A map on the two horizontal edges of a square extends to a continuous map of the full square into the plane." />
  <figcaption>Figure 5.14</figcaption>
</figure>

_Proof._ Let $p_i$ be the projection into the $i$th coordinate from $R^n$ into $R$ [defined by $p_i(x_1,\ldots,x_i,\ldots,x_n)=x_i$]. Then, setting $f_i=p_i\circ f$, $f_i$ is a continuous function from $A$ into $R$. Each $f_i$ therefore has a continuous extension $F_i$ to all of $X$. Define

$$
F(x)=(F_1(x),\ldots,F_n(x))
$$

for each $x\in X$. Then $F$ is an extension of $f$; moreover, $F$ is continuous, by Proposition 21 of Chapter 4.

**Example 16.** Let $R$ be the space of real numbers with the absolute value topology and $R^2$ be the plane with the product topology. Both of these spaces are normal. Let $Z\subset R$ be the set of integers, and let $Z^2\subset R^2$ be the set of points of $R^2$ of the form $(m,n)$, where $m$ and $n$ are integers. The subspace topology on both $Z$ and $Z^2$ is the discrete topology, and hence any function $f$ from $Z$ into $Z^2$ is continuous. Both $Z$ and $Z^2$ are of the same cardinality; thus we have a one-one function $f$ from $Z$ onto $Z^2$, and $f$ is continuous. By the corollary to Proposition 11, then $f$ has a continuous extension $F$ from $R$ into $R^2$. The reader might find from a little experimentation that this is a case where the proof that $F$ exists is much simpler than trying to construct a specific $F$.

**Example 17.** A continuous function from the interval $[0,1]$ into any space $X,\tau$ is called a _path_ in $X$. The space $[0,1]\times[0,1]$ (with the product topology) is a normal space, and

$$
A=\{(x,y)\mid y=1\}\cup\{(x,y)\mid y=0\}
$$

is a closed subset of $[0,1]\times[0,1]$. If $f$ is any continuous function from $A$ into $R^2$, then $f$ has a continuous extension $F$ (Fig. 5.14). Technically, this means that any two paths in $R^2$ are _homotopic_ (Chapter 11).

## Exercises

1. Using only Proposition 10, prove the following: Suppose $A$ and $B$ are disjoint closed subsets of a normal space $X,\tau$ and $f$ is a continuous function from $A\cup B$ <span id="printed-page-112"></span><!-- Source: PDF page 109, printed page 112. --> into $R^n$ such that $f\mid A$ and $f\mid B$ are each constant functions. Then $f$ has a continuous extension $F$ to all of $X$.

2. Any space which can be substituted for $R$ in Proposition 11 is called an _absolute retract_. Which of the following are definitely absolute retracts? Which could not possibly be absolute retracts?

   a) $R^n$, with the product topology from the space $R$ of real numbers with the absolute value topology

   b) $I^n$, with the product topology, where $I=[0,1]$

   c) any finite set with the discrete topology

   d) $(0,1)$ with the usual topology

   e) $R^2-\{(0,0)\}$ with the Pythagorean topology

3. Which of the following statements about absolute retracts are true?

   a) If $X,\tau$ is an absolute retract and $x$ and $y$ are any points of $X$, then there is a continuous function $f$ from $[0,1]$ into $X$ such that $\{x,y\}\subset f([0,1])$.

   b) The product space of a countable family of nonempty spaces is an absolute retract if and only if each component space is an absolute retract.

4. Prove that the set of binary rationals as described in the proof of Proposition 10 is dense in the space of real numbers.

5. A space $X$ is said to be _completely normal_ (sometimes called $T_5$) if every subspace of $X$ is normal. Prove that $X$ is completely normal if and only if $X$ is $T_1$ and given any two subsets $A$ and $B$ of $X$ such that $\operatorname{Cl}A\cap B=A\cap\operatorname{Cl}B=\phi$, there exist disjoint open sets $U$ and $V$ such that $A\subset U$ and $B\subset V$.

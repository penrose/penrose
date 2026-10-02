---
title: Cauchy Sequences — Elementary Topology
---

# 10.2 Cauchy Sequences

<span id="printed-page-213"></span>

<!-- Source: PDF199, printed213, section10.2 fragment. -->

Preparatory to a discussion of complete metric spaces, we will investigate a notion which the reader may have encountered as early as freshman calculus, that of a _Cauchy sequence_ (although, as was the case with _metric_, the terminology might have been different).

<span id="printed-page-214"></span>

<!-- Source: PDF200, printed214. -->

Suppose $X,D$ is any metric space and $\{s_n\}$, $n\in N$, is any sequence in $X$ which converges to some point $y$ of $X$. Then the following proposition is true.

**Proposition 3.** Given any number $p>0$, there is a positive integer $M$ such that if $k$ and $m$ are any two integers greater than $M$, then $D(s_k,s_m)<p$.

_Proof._ Since $s_n\to y$, we may find a positive integer $M$ such that if $n>M$, $D(s_n,y)<p/2$. Then if $k$ and $m$ are both integers greater than $M$, we have

$$
D(s_k,s_m)\le D(s_k,y)+D(y,s_m)<p/2+p/2=p.
$$

This result inspires the following definition.

**Definition 1.** Let $X,D$ be a metric space. Then a sequence $\{s_n\}$, $n\in N$, in $X$ is said to be a _Cauchy sequence_ if given any positive number $p$, there is a positive integer $M$ such that if $m$ and $k$ are integers greater than $M$, then $D(s_k,s_m)<p$.

::: info Source numbering
The source labels this definition “Definition 1,” as it also does the metrizable-space definition in Section 10.1.
:::

Proposition 3 states that if a sequence in a metric space converges, then that sequence is a Cauchy sequence. It is not true, however, that every Cauchy sequence in any metric space converges.

**Example 6.** Let $\{s_n\}$, $n\in N$, be the sequence in the space $R$ of real numbers (with the absolute value metric) defined by $s_n=1/n$. Then $s_n\to0$; hence $\{s_n\}$, $n\in N$, is a Cauchy sequence in $R$, or any subspace of $R$ which contains it. But $\{s_n\}$, $n\in N$, is therefore a Cauchy sequence in $R-\{0\}$; however, it does not converge in $R-\{0\}$.

When the reader first studied the structure of the space $R$ of real numbers, he may have taken as a basic axiom any one of the following:

A. Any nonempty subset $W$ of $R$ which has an upper bound has a least upper bound.

B. Any nonempty subset $W$ of $R$ which has a lower bound has a greatest lower bound.

C. Every Cauchy sequence in $R$ converges.

We will now show that all three of these statements are equivalent relative to $R$. In Exercise 2, the reader is asked to show the equivalence of A and B. Propositions 4 and 5 now prove that A and C are equivalent.

**Proposition 4.** Assume property A of the space $R$ of real numbers. Then a sequence $\{s_n\}$, $n\in N$, in $R$ converges if and only if it is a Cauchy sequence.

_Proof._ If $\{s_n\}$, $n\in N$, converges, it is a Cauchy sequence by Proposition 3.

<span id="printed-page-215"></span>

<!-- Source: PDF201, printed215. -->

Suppose that $\{s_n\}$, $n\in N$, is a Cauchy sequence. Set $p=1$. Then there is a positive integer $M$ such that if $k$ and $m$ are integers greater than $M$, $|s_k-s_m|<1$. Let

$$
T=\max(|s_1|,|s_2|,\ldots,|s_{M+1}|).
$$

Then for any positive integer $n$, $|s_n|<T+1$; hence

$$
-(T+1)<s_n<T+1.
$$

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-10.1.svg" alt="A bounded number-line interval with midpoint a1 and a collection of sequence points, used in a bisection argument." />
<figcaption>Figure 10.1. <a href="/docs/elementary-topology/reader?page=215">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-10.2.svg" alt="Number-line intervals with successive endpoints a1,a2 and b1,b2 and sequence points, illustrating nested interval bisection." />
<figcaption>Figure 10.2. <a href="/docs/elementary-topology/reader?page=215">View the interactive figure and its Substance program.</a></figcaption>
</figure>

Divide $[-(T+1),T+1]$ into two intervals $[-(T+1),0]$ and $[0,T+1]$. One of these intervals must contain infinitely many of the $s_n$. Set the left-hand endpoint of that interval equal to $a_1$ and the right-hand endpoint of that interval equal to $b_1$ (Fig. 10.1). Divide $[a_1,b_1]$ into

$$
[a_1,(a_1+b_1)/2]\quad\text{and}\quad[(a_1+b_1)/2,b_1].
$$

One of these intervals contains infinitely many of the $s_n$. Set the left-hand endpoint of that interval equal to $a_2$ and the right-hand endpoint of that interval equal to $b_2$. Continuing in like manner, suppose we have found that the interval $[a_{n-1},b_{n-1}]$ contains infinitely many $s_n$. Divide

$$
[a_{n-1},b_{n-1}]
$$

into

$$
[a_{n-1},(a_{n-1}+b_{n-1})/2]\quad\text{and}\quad[(a_{n-1}+b_{n-1})/2,b_{n-1}]
$$

(Fig. 10.2). One of these intervals contains infinitely many of the $s_n$. Set the left-hand endpoint of that interval equal to $a_n$ and the right-hand endpoint equal to $b_n$. By construction the following statements hold:

a) $a_{n-1}\le a_n$, $n=1,2,3,\ldots$;

b) $b_n\le b_{n-1}$, $n=1,2,3,\ldots$;

c) $b_n-a_n=(\tfrac12)^n((T+1)-(-(T+1)))=(\tfrac12)^{n-1}(T+1)$, $n=1,2,3,\ldots$.

Let $A=\{a_n\mid n\in N\}$ and $B=\{b_n\mid n\in N\}$. Since $A$ is nonempty and has an upper bound $T+1$, $A$ has a least upper bound, say $a$. Since $B$ is nonempty and has a lower bound, $B$ has a greatest lower bound, say $b$. Then $a_n\le a\le b\le b_n$ for each $n\in N$. If $a\ne b$, then $b-a>0$. But

$$
b-a\le b_n-a_n=(\tfrac12)^{n-1}(T+1)
$$

<span id="printed-page-216"></span>

<!-- Source: PDF202, printed216. -->

for each $n$. If then $b-a>0$, there is a positive integer $M'$ such that if $n>M'$,

$$
(\tfrac12)^{n-1}(T+1)<b-a,
$$

which is impossible; therefore $a=b$.

We now show that $s_n\to a$. Let $p>0$. Then there is an integer $M_1$ such that $n>M_1$ implies $b_n-a_n<p/2$. Consequently

$$
[a_n,b_n]\subset N(a,p/2)=(a-p/2,a+p/2).
$$

Therefore $N(a,p/2)$ contains infinitely many of the $s_n$. Since $\{s_n\}$, $n\in N$, is a Cauchy sequence, there is a positive integer $M$ such that if $k$ and $m$ are greater than $M$, $|s_k-s_m|<p/2$. Since $N(a,p/2)$ contains infinitely many $s_n$, there is at least one integer $m'>M$ such that $s_{m'}\in N(a,p/2)$. Therefore if $n>M$, then

$$
|s_n-a|\le|s_n-s_{m'}|+|s_{m'}-a|<p/2+p/2=p.
$$

Hence if $n>M$, then $s_n\in N(a,p)$. Therefore $s_n\to a$.

**Proposition 5.** If property C is assumed for the space $R$ of real numbers, then every nonempty subset $W$ of $R$ which has an upper bound has a least upper bound; that is, property C implies property A.

_Proof._ Let $W$ be a nonempty subset of $R$ such that $W$ has an upper bound $T$. We can find a sequence $\{s_n\}$, $n\in N$, such that (1) $s_n\le s_{n+1}$ for each $n$; (2) $s_n\in W$ for each $n$; and (3) given any $w\in W$, there is a positive integer $M$ such that $n>M$ implies $w\le s_n$. The construction of such a sequence is left as an exercise. We now prove that this sequence must be a Cauchy sequence.

Suppose $\{s_n\}$, $n\in N$, is not a Cauchy sequence. Then for some $p>0$ there is no integer $M'$ such that if $m$ and $k$ are integers greater than $M'$, then $|s_k-s_m|<p$. By construction, if $k<m$, then $s_k\le s_m$; hence

$$
|s_k-s_m|=s_m-s_k.
$$

Therefore there is an integer $m_1$ such that $s_{m_1}-s_1>p$. There is an integer $m_2>m_1$ such that $s_{m_2}-s_{m_1}>p$; therefore $s_{m_2}-s_1>2p$. There is an integer $m_3>m_2$ such that $s_{m_3}-s_{m_2}>p$, and hence $s_{m_3}-s_1>3p$. Continuing in like fashion, we see that the set of $s_n$, and hence $W$, could not have an upper bound, contradicting the assumption that $W$ has an upper bound. Therefore $\{s_n\}$, $n\in N$, is a Cauchy sequence.

Since $\{s_n\}$, $n\in N$, is a Cauchy sequence, it converges to some limit $y$. It is left as an exercise to prove that $y$ is the least upper bound of $W$.

::: warning Missing printed page 217
The supplied scan omits printed page 217, between the end of this proof and Definition 2 in Section 10.3. No unavailable exercises or next-section introduction are reconstructed. Section 10.3's available text begins on printed page 218.
:::

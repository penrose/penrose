---
title: Complete Metric Spaces — Elementary Topology
---

<script setup>
import ReadingAdditions from '../../../src/elementary-topology/ReadingAdditions.vue';
</script>

# 10.3 Complete Metric Spaces

::: warning Missing printed page 217
The supplied scan omits printed page 217, including this section's opening. The section title is verified by the running header on printed page 219. No missing introduction or preceding exercises are reconstructed.
:::

<span id="printed-page-218"></span>

<!-- Source: PDF203, printed218. -->

**Definition 2.** A metric $D$ on a set $X$ is said to be _complete_ if every Cauchy sequence in $X,D$ converges to a point of $X$. If $D$ is a complete metric on $X$, then $X,D$ is said to be a _complete metric space_.

The absolute value metric $D$ on the space $R$ of real numbers is a complete metric on $R$; thus $R,D$ is a complete metric space. Note that $D$ is not a complete metric for the set $Q$ of rational numbers.

We said that a metric space is complete if its metric is complete. Two metric spaces may be homeomorphic as topological spaces, but one might be a complete metric space and the other not. The following example illustrates this point.

**Example 7.** Let $N$ be the set of positive integers. Let $D$ be the usual absolute value metric on $N$, i.e., $D(m,n)=|m-n|$ for all $m,n\in N$. Then $N,D$ is a complete metric space, since only Cauchy sequences in $N$ are those sequences which are constant from some point on (see Section 10.2, Exercise 6). The topology induced on $N$ by $D$ is the discrete topology. Now define a metric $D'$ on $N$ by

$$
D'(m,n)=|1/m-1/n|.
$$

It is easily verified that $D'$ is a metric on $N$ which also induces the discrete topology. Therefore, considered as topological spaces, $N,D$ and $N,D'$ are homeomorphic (the identity mapping being an explicit homeomorphism). However, $D'$ is not a complete metric space, since the sequence $\{s_n\}$, $n\in N$, in $N$ defined by $s_n=n$ for each $n\in N$ is a Cauchy sequence, but does not converge.

The following terminology proves useful in the discussion of complete metric spaces.

**Definition 3.** Let $A$ be a subset of a metric space $X,D$. The _diameter_ of $A$ is defined to be the least upper bound of

$$
\{D(x,y)\mid x,y\in A\}.
$$

We denote the diameter of $A$ by $d(A)$.

**Example 8.** If $A=\{(x,y)\mid x^2+y^2=1\}\subset R^2$ with the Pythagorean metric, then $d(A)=2$ (Fig. 10.3). If $B=\{(x,y)\mid |x|\le2,|y|\le1\}\subset R^2$, then $d(B)=\sqrt{20}$ (Fig. 10.4).

**Proposition 6.** If $A$ is a subset of the metric space $X,D$ then

$$
d(A)=d(\operatorname{Cl}A).
$$

_Proof._ Let $p$ be any positive number. We will show that

$$
d(\operatorname{Cl}A)<d(A)+p.
$$

<span id="printed-page-219"></span>

<!-- Source: PDF204, printed219. -->

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-10.3.svg" alt="A unit circle A on coordinate axes, its horizontal diameter marked d(A)=2 and endpoints (-1,0),(1,0)." />
<figcaption>Figure 10.3. <a href="/docs/elementary-topology/reader?page=219">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-10.4.svg" alt="A shaded rectangle B on coordinate axes with a diagonal marked d(B)=sqrt(20), illustrating the diameter of a planar set." />
<figcaption>Figure 10.4. <a href="/docs/elementary-topology/reader?page=219">View the interactive figure and its Substance program.</a></figcaption>
</figure>

Suppose $x$ and $y$ are in $\operatorname{Cl}A$. Then both $N(x,p/2)$ and $N(y,p/2)$ meet $A$. Choose

$$
x'\in N(x,p/2)\cap A
$$

and

$$
y'\in N(y,p/2)\cap A.
$$

Then

$$
\begin{aligned}
D(x,y)&\le D(x,x')+D(x',y)\le D(x,x')+D(x',y')+D(y,y')\\
&<p/2+d(A)+p/2=d(A)+p.
\end{aligned}
$$

Therefore $d(\operatorname{Cl}A)\le d(A)$. But since $A\subset\operatorname{Cl}A$, $d(A)\le d(\operatorname{Cl}A)$; hence $d(A)=d(\operatorname{Cl}A)$.

We now use Proposition 6 to prove an important criterion for completeness.

**Proposition 7.** A metric space $X,D$ is complete if and only if given a countable family $\{A_n\}$, $n\in N$, of closed, nonempty subsets of $X$ such that

$$
A_1\supset A_2\supset\cdots\supset A_n\supset\cdots\quad\text{and}\quad d(A_n)\to0,
$$

$\bigcap_N A_n\ne\phi$.

_Proof._ Suppose $X,D$ is a complete metric space. For each $n\in N$, choose $a_n\in A_n$. Suppose $p>0$. Then there is an integer $M$ such that $n>M$ implies $d(A_n)<p/2$. If $k$ and $m$ are both integers greater than $M$, then $a_k$ and $a_m$ are both elements of $A_{M+1}$; hence, since $d(A_{M+1})<p/2$,

$$
D(a_k,a_m)<p.
$$

We therefore see that $\{a_n\}$, $n\in N$, is a Cauchy sequence. Since $X,D$ is assumed to be a complete metric space, the sequence $\{a_n\}$, $n\in N$, converges to some point $y$. Then for any $n$, $\{a_n,a_{n+1},\ldots\}$ is also a sequence <span id="printed-page-220"></span><!-- Source: PDF205, printed220. -->which converges to $y$. But $A_n$ is closed and

$$
\{a_n,a_{n+1},\ldots\}\subset A_n,
$$

for each $n$; therefore $y\in A_n$ (since $y\in\operatorname{Cl}A_n$ and $\operatorname{Cl}A_n=A_n$). Since $n$ was arbitrary, $y\in\bigcap_N A_n$; therefore $\bigcap_N A_n\ne\phi$.

Conversely, suppose that given any decreasing sequence $A_1\supset A_2\supset\cdots$ of closed, nonempty subsets of $X$ such that

$$
d(A_n)\to0,\qquad\bigcap_N A_n\ne\phi.
$$

Let $\{s_n\}$, $n\in N$, be a Cauchy sequence in $X$. Set

$$
B_n=\{s_k\mid k\ge n\}\quad\text{and}\quad A_n=\operatorname{Cl}B_n
$$

for all $n\in N$. Then $\{A_n\}$, $n\in N$, fulfills the necessary conditions, and hence $\bigcap_N A_n\ne\phi$. Choose $y\in\bigcap_N A_n$. We now show that $s_n\to y$.

Let $p>0$. Then there is an integer $M$ such that if $n>M$, $d(B_n)<p$. By Proposition 6, $d(A_n)<p$ as well. Then $D(s_n,y)<p$ for all $n>M$; that is, if $n>M$, then $s_n\in N(y,p)$. Therefore $s_n\to y$.

As we have seen, not every metric space is complete. The next proposition enables us to say that any compact metric space is complete.

**Proposition 8.** If a metric space $X,D$ is compact, then $D$ is complete.

_Proof._ Suppose we have a decreasing sequence $A_1\supset A_2\supset\cdots$ of closed, nonempty subsets of $X$. Then, by Proposition 8, Chapter 7, $\bigcap_N A_n\ne\phi$. Therefore, by Proposition 7, $D$ is complete.

**Corollary.** Any separable metric space $X,D$ is homeomorphic to a dense subspace of a complete metric space.

_Proof._ By the corollary to Proposition 1, $X$ has a metrizable compactification $Z,D'$. But $Z,D'$ is a complete metric space by Proposition 8.

It is not true that any subspace of a complete metric space is necessarily complete (e.g., the rational numbers form an incomplete subspace of the real numbers). We do, however, have the following.

**Proposition 9.** Any closed subspace $Y$ of a complete metric space $X,D$ is a complete metric space.

_Proof._ Suppose $\{s_n\}$, $n\in N$, is a Cauchy sequence in $Y$. Then $\{s_n\}$, $n\in N$, is also a Cauchy sequence in $X$; hence $s_n\to y$, for some $y\in X$. But then $y\in\operatorname{Cl}Y=Y$, and thus $\{s_n\}$, $n\in N$, converges in $Y$.

Suppose $\{X_n,D_n\}$, $n\in N$, is a countable family of nonempty metric spaces. Then a metric $D$ can be defined for $\mathop{\Large\times}_N X_n$ as in Proposition 2. <span id="printed-page-221"></span><!-- Source: PDF206, printed221. -->The following proposition gives a necessary and sufficient condition for $\mathop{\Large\times}_N X_n,D$, to be complete.

**Proposition 10.** $\mathop{\Large\times}_N X_n,D$, is a complete metric space if and only if each component space $X_n,D_n$ is a complete metric space.

_Proof._ First suppose that $X_n,D_n$ is a complete metric space for each $n$, and that $\{s_n\}$, $n\in N$, is a Cauchy sequence in $\mathop{\Large\times}_N X_n$. If we denote the $k$th coordinate of $s_n$ by $s_n(k)$, then it is easily verified that $\{s_n(k)\}$, $n\in N$, is a Cauchy sequence in $X_k$. Therefore $\{s_n(k)\}$, $n\in N$, converges in $X_k$. The convergence of $\{s_n\}$, $n\in N$, then follows from Proposition 12, Chapter 6.

Suppose now that one of the $X_n,D_n$, say $X_1,D_1$, is not complete. Then there is a Cauchy sequence $\{s_n(1)\}$, $n\in N$, in $X_1$ which does not converge. Select a point $a_n$ from each $X_n$, $n\ge2$. Then the sequence

$$
\{s_n\},\quad n\in N,\quad\text{in}\quad\mathop{\Large\times}_N X_n,
$$

defined by setting the first coordinate of $s_n$ equal to $s_n(1)$ and the $n$th coordinate of $s_n$ equal to $a_n$, $n\ne1$, (that is, $\{s_n\}$, $n\in N$, is a constant sequence in all but the first coordinate) is a Cauchy sequence which could not converge in $\mathop{\Large\times}_N X_n$, since it does not converge in the first coordinate.

**Corollary.** The Hilbert cube is a complete metric space.

_Proof 1._ Since $[0,1]$ with the absolute value metric is compact, it is a complete metric space; hence the Hilbert cube $\mathop{\Large\times}_N[0,1]$ is a complete metric space by Proposition 10.

_Proof 2._ The Hilbert space is a compact metric space, and hence is complete by Proposition 8.

::: info Source wording
The second proof says “Hilbert space,” retained as printed.
:::

We have already seen that any separable metric space is homeomorphic to a dense subspace of a complete metric space. Actually, the following stronger result is true.

**Proposition 11.** Let $X,D$ be any metric space. Then $X$ may be embedded as a dense subspace of a complete metric space $Y,D'$ by an embedding which preserves distances [that is, if $h$ is the embedding of $X$, then $D(x,y)=D'(h(x),h(y))$ for any $x$ and $y$ in $X$].

We saw earlier that any topological space was homeomorphic to a dense subset of a compact space. Here we have a somewhat analogous theorem for metric spaces, compact spaces being the most important type of topological space, and complete metric spaces being the most important type of metric space. A space $Y,D'$ as described in Proposition 11 is called a _completion_ of $X,D$.

<span id="printed-page-222"></span>

<!-- Source: PDF207, printed222. -->

The proof of Proposition 11 in its entirety is long and cumbersome, and little is to be gained by going through all the sordid details. Therefore only an outline of the proof is presented here.

_Outline of proof (Proposition 11)._ Let $Y'$ be the set of all Cauchy sequences in $X$. Two Cauchy sequences $\{s_n\}$, $n\in N$, and $\{t_n\}$, $n\in N$, will be considered to be equivalent if $D(s_n,t_n)\to0$. It must be verified that we have thus defined a genuine equivalence relation on $Y'$. Denote the equivalence class of a sequence $\{s_n\}$, $n\in N$, by $\{s_n\}''$; denote the set of equivalence classes by $Y$. We define a metric $D'$ on $Y$ by setting

$$
D'(\{s_n\}'',\{t_n\}'')=\lim D(s_n,t_n).
$$

It must be shown that the required limit always exists and is independent of the representatives of the equivalence classes. Moreover, it must be shown that $D'$ is actually a metric. If $x\in X$, then $x$ can be identified with the equivalence class of the constant sequence $\{s_n=x\}$, $n\in N$. The mapping thus defined in distance preserving is hence a homeomorphism. By a method of diagonalization, it can be shown that each Cauchy sequence in $Y$ converges, and that each element of $Y$ is the limit of a Cauchy sequence each member of which is the class of a constant sequence. For more details, the reader might see Theorem 2–72 in Hocking and Young, _Topology_ (Addison-Wesley, 1961).

## Exercises

1. In Example 7, confirm that $D'$ is a metric on $N$ and that $D'$ induces the discrete topology on $N$. Describe a completion of $N,D$; of $N,D'$.

2. Find a necessary and sufficient condition for a completion of a space $X,D$ to be compact. [*Hint:* Let $R$ be the space of real numbers and let $D$ be the absolute value metric. Set $D'(x,y)=\min(D(x,y),1)$ for all $x,y\in R$. Then $D'$ is a metric on $R$ which is equivalent to $D$. Is $D'$ a complete metric on $R$? Describe the completion of $R,D'$.]

3. Prove Proposition 8 by means of Proposition 9, Chapter 7 and the definition of a Cauchy sequence.

4. In the proof of Proposition 10, verify that each $\{s_n(k)\}$, $n\in N$, is really a Cauchy sequence in $X_k$.

5. Let $f$ be a continuous function from a metric space $X,D$ into itself with the property that

   $$
   D(f(x),f(y))\le kD(x,y),
   $$

   where $0\le k<1$, for any $x,y\in X$. That is, $f$ “contracts” distances. Set $f^1=f$, $f^2=f\circ f$, and, in general, $f^n=f\circ f^{n-1}$. Choose $y\in X$. Consider the sequence $\{s_n\}$, $n\in N$, defined by $s_n=f^n(y)$.

   a) Prove that $\{s_n\}$, $n\in N$, is a Cauchy sequence.

   <span id="printed-page-223"></span>
   <!-- Source: PDF208, printed223, section10.3 fragment. -->

   b) Prove that if $D$ is complete, then $f$ has a unique fixed point, that is, that there is one and only one $z\in X$ such that $f(z)=z$. [*Hint:* Let $z$ be the unique limit of $\{s_n\}$, $n\in N$. The sequence $\{f(s_n)\}$, $n\in N$, must have the same limit as $\{s_n\}$, $n\in N$, but $f(z)$ is also the limit of $\{f(s_n)\}$, $n\in N$. Suppose $z$ and $z'$ are both fixed points of $f$, and show that this contradicts the assumption that $f$ is a contracting function.]

   c) Show by example that if $D$ is not complete, a contracting function may not have any fixed points. If $D$ is not complete, might a contracting function have more than one fixed point?

6. A distance-preserving function is called an _isometry_. Prove that it is not possible to isometrically embed a complete metric space $X,D$ as a dense proper subspace of another complete metric space $Y,D'$. Prove, however, that it might be possible to embed $X$ as a dense proper subspace of a complete metric space $Y,D'$ if the embedding is not required to be an isometry.

7. A subset $A$ of a metric space $X,D$ is said to be _totally bounded_ if given any positive number $p$, the open cover $\{N(x,p)\}$, $x\in A$, of $A$ has a finite subcover. Prove that a subspace of a complete metric space is compact if and only if it is closed and totally bounded. (Cf. Exercise 6 of Section 8.1.)

<ReadingAdditions :pdf-page="207" />

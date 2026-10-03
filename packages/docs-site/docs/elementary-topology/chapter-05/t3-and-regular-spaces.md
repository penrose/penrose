---
title: T₃- and Regular Spaces — Elementary Topology
description: The available text of section 5.3 of the supplied second-edition scan.
---

# 5.3 $T_3$- and Regular Spaces

::: info Transcription note
Source: printed pages 97–101 (PDF pages 96–100). Printed page 97 begins with the final exercises of §5.2, transcribed on that section's page. The author's explicit convention for $T_3$ and regular spaces is retained. The source's “neighborhod” in Example 8 is also retained.
:::

<span id="printed-page-97"></span>

<!-- Source: PDF page 96, printed page 97; section 5.3 fragment. -->

**Definition 4.** A space $X,\tau$ is said to be $T_3$ if given any closed subset $F$ of $X$ and any point $x$ of $X$ which is not in $F$, there are open sets $U$ and $V$ such that $x\in U$, $F\subset V$, and $U\cap V=\phi$ (Fig. 5.2). A space $X,\tau$ is said to be _regular_ if $X$ is both $T_3$ and $T_1$. (The author is quite aware of the lack of uniformity in the literature about what constitutes a $T_3$- or a regular space. In some places, $T_3$ and regular are synonymous. In others, $T_1$ is assumed as part of $T_3$, and in still others, $T_3$ does not imply $T_1$. The author has therefore felt justified in making the definition to suit himself.)

<div class="topology-chapter-figure-pair">
<figure class="topology-chapter-figure">
  <img src="/elementary-topology/figures/figure-5.2.svg" alt="A point x and a closed set F separated by disjoint open sets U and V." />
  <figcaption>Figure 5.2</figcaption>
</figure>
<figure class="topology-chapter-figure">
  <img src="/elementary-topology/figures/figure-5.3.svg" alt="A neighborhood V of x whose closure is contained in the neighborhood U." />
  <figcaption>Figure 5.3</figcaption>
</figure>
</div>

We first state and prove a very important criterion for being $T_3$.

<span id="printed-page-98"></span>

<!-- Source: PDF page 97, printed page 98. -->

**Proposition 4.** A space $X,\tau$ is $T_3$ if and only if given any $x\in X$ and any neighborhood $U$ of $x$, there is a neighborhood $V$ of $x$ such that $\operatorname{Cl}V\subset U$ (Fig. 5.3).

_Proof._ Suppose that $X$ is $T_3$ and that $U$ is a neighborhood of $x$. Then $X-U$ is a closed subset of $X$ which does not contain $x$. Therefore there are open sets $W$ and $V$ such that $X-U\subset W$, $x\in V$, and $W\cap V=\phi$. Since $X-U\subset W$, $X-W\subset U$. Moreover, since $W\cap V=\phi$, we have $V\subset X-W\subset U$. But then $X-W$ is a closed set which contains $V$; hence

$$
V\subset\operatorname{Cl}V\subset X-W\subset U.
$$

Suppose instead that given any $x\in X$ and any neighborhood $U$ of $x$, there is a neighborhood $V$ of $x$ such that $\operatorname{Cl}V\subset U$. Let $x\in X$, and let $F$ be any closed subset of $X$ which does not contain $x$. Then $X-F$ is a neighborhood of $x$; hence there is a neighborhood $V$ of $x$ such that $\operatorname{Cl}V\subset X-F$. Then $X-\operatorname{Cl}V$ is an open set which contains $F$, and $V$ is an open set which contains $x$. Since $V\subset\operatorname{Cl}V$,

$$
(X-\operatorname{Cl}V)\cap V=\phi.
$$

Therefore $X-\operatorname{Cl}V$ and $V$ are suitable open sets for “separating” $F$ and $x$; hence $X$ is $T_3$.

**Example 8.** Any metric space $X,D$ is regular. Although this has been proved previously, we may prove it again using Proposition 4. For if $x\in X$, and if $U$ is any neighborhood of $x$, then $U$ contains a $D$-$p$-neighborhood of $x$ for some positive number $p$. Choose a number $q$ such that $0<q<p$. Then the $D$-$q$-neighborhod of $x$ is a subset of the $D$-$p$-neighborhood of $x$, and

$$
\operatorname{Cl}N(x,q)\subset\{y\mid D(y,x)\leq q\}\subset N(x,p)=\{w\mid D(w,x)<p\}\subset U.
$$

Therefore $X,D$ is $T_3$. We have already seen that any metric space is $T_1$; hence any metric space is regular.

**Example 9.** If $X$ is any set of more than one point, then if $X$ is given the trivial topology, $X$ is $T_3$ in a vacuous sort of way. For the only closed nonempty subset of $X$ is $X$ itself, and it follows that there is no point of $X$ in $X-X$. It is to avoid such cases as this that one usually requires a space to be regular rather than merely $T_3$. We also see from this example that $T_3$ does not imply $T_2$. However, if every one-point subset of $X$ is a closed subset of $X$, then $X$ is $T_2$ if $X$ is $T_3$.

We now give an example of a space which is $T_2$ but not $T_3$, or regular. This demonstrates that regularity is a stronger separation property than merely being $T_2$.

<span id="printed-page-99"></span>

<!-- Source: PDF page 98, printed page 99. -->

**Example 10.** Let $R$ be the set of real numbers. We will define a topology on $R$ by giving an open neighborhood system. If $x$ is any real number other than 0, let $\mathfrak{N}_x$ be the family of all open intervals which contain $x$. If $x=0$, we will let $\mathfrak{N}_0$ be the family of all sets of the form

$$
(-p,p)-\{1/n\mid n\text{ is a positive integer}\},
$$

where $0<p$. The collection of $\mathfrak{N}_x$ for all $x\in R$ gives an open neighborhood system for a topology $\tau$ on $R$ (Chapter 3, Proposition 7). It is easily verified that $R,\tau$ is $T_2$ (Exercise 1).

We now show that $X$ is not $T_3$. Take $x=0$ and $F=\{1/n\mid n\text{ is a positive integer}\}$. $F$ is a closed subset of $R,\tau$, since there is no point $y$ of $R$ such that each neighborhood of $y$ contains a point of $F$, except those points in $F$ itself. (Note that in the usual topology for $R$, each neighborhood of 0 would contain points of $F$; thus 0 would be in $\operatorname{Cl}F$. We have, however, purposely excluded the points of $F$ from the neighborhoods of 0.) Suppose $V$ is a neighborhood of 0 of the form

$$
(-p,p)-\{1/n\mid n\text{ is a positive integer}\}
$$

(any neighborhood of 0 contains such a neighborhood because of the manner in which the $\mathfrak{N}_x$ define the topology $\tau$). Then $(-p,p)$ contains infinitely many of the $1/n$. Hence any open set $U$ which contains $F$ would have to overlap $V$ (Fig. 5.4); thus we could not find an open set $U$ which contains $F$ such that $U\cap V=\phi$. Therefore $R,\tau$ is not $T_3$.

<figure class="topology-chapter-figure">
  <img src="/elementary-topology/figures/figure-5.4.svg" alt="Deleted reciprocal points approaching zero; an open set containing them overlaps a neighborhood of zero." />
  <figcaption>Figure 5.4</figcaption>
</figure>

We now investigate how the property of being regular or $T_3$ behaves with respect to the derived topological spaces.

**Proposition 5**

a) Every subspace of a regular space is regular.

b) Suppose $Y=\mathop{\Large\times}_I X_i$ is the product space of the (countable) family of nonempty spaces $\{X_i,\tau_i\}$, $i\in I$. Then $Y$ is regular if and only if each $X_i,\tau_i$ is a regular space.

_Proof_

a) Suppose $W$ is a subspace of a regular space $X,\tau$. Let $F$ be closed in $W$ and $x\in W-F$. Since $F$ is closed in $W$, $F=W\cap F'$, where $F'$ is a closed subset of $X$. Then $x\in X-F'$. Since $X$ is $T_3$, <span id="printed-page-100"></span><!-- Source: PDF page 99, printed page 100. --> there are open sets $U$ and $V$ in $X$ such that

$$
x\in U,\qquad F'\subset V,\qquad\text{and}\qquad U\cap V=\phi.
$$

Then $W\cap U$ and $W\cap V$ are disjoint subsets of $W$ which are open in $W$ such that $x\in W\cap U$ and $F\subset W\cap V$. $W$ is therefore $T_3$. By Section 5.1, Exercise 6, $W$ is also $T_1$, since $X$ is $T_1$. Therefore $W$ is regular.

b) Suppose each $X_i,\tau_i$ is a regular space. By Exercise 6 of Section 5.1, $Y$ is $T_1$. Suppose that $x\in Y$ and that $U$ is a neighborhood of $x$. We lose no generality in assuming that $U$ is a basis element for the product topology, since any neighborhood of $x$ contains a neighborhood of $x$ which is a basis element. Then $U=\mathop{\Large\times}_I U_i$, where each $U_i$ is open in $X_i$. Each $U_i$ is therefore a neighborhood in $X_i$ of $x_i$, the $i$th coordinate of $x$ and $U_i=X_i$ except for $i_1,\ldots,i_m$. Since each $X_i$ is $T_3$, there is an open neighborhood $V_{i_j}$ of $x_{i_j}$, $j=1,\ldots,m$, such that

$$
x_{i_j}\in V_{i_j}\subset\operatorname{Cl}V_{i_j}\subset U_{i_j}.
$$

For each $i\in I$, set $V_i=X_i$, $i\ne i_j$ and let the $V_{i_j}$ be as above, $j=1,\ldots,m$. Then

$$
x\in\mathop{\Large\times}_I V_i\subset\operatorname{Cl}\left(\mathop{\Large\times}_I V_i\right)\subset\mathop{\Large\times}_I\operatorname{Cl}V_i\subset U.
$$

By Proposition 4, then, $Y$ is $T_3$. Hence $Y$ is regular.

If $Y$ is regular, then since each $X_i,\tau_i$ is homeomorphic to a subspace of $Y$, and each subspace of $Y$ is regular, each $X_i$ is regular.

As with $T_2$-spaces, it is not true that if $X,\tau$ is regular and $R$ is an equivalence relation on $X$, then the identification space $X/R$ is regular (Exercise 2). However, the following is true.

**Proposition 6.** If $X,\tau$ is a regular space and $F$ is a closed subset of $X$, then if $R$ is the equivalence relation defined by the partition

$$
\bigl\{\{x\mid x\in F\}\bigr\}\cup\bigl\{\{y\mid y=y\}\mid y\notin F\bigr\},
$$

the identification space $X/R$ is $T_2$. (Note that this identification has the effect of shrinking $F$ to a point. See Figs. 5.5 and 5.6.)

_Proof._ Suppose $\bar{x}$ and $\bar{y}$ are distinct points of $X/R$, where $\bar{x}$ and $\bar{y}$ denote the equivalence classes of $x$ and $y$, respectively. If $x$ and $y$ are not in $F$, then since $X$ is regular and hence also $T_2$, there are open sets $U$ and $V$ in $X-F$ such that

$$
x\in U,\qquad y\in V,\qquad\text{and}\qquad U\cap V=\phi.
$$

<span id="printed-page-101"></span>

<!-- Source: PDF page 100, printed page 101. -->

<div class="topology-chapter-figure-pair">
<figure class="topology-chapter-figure">
  <img src="/elementary-topology/figures/figure-5.5.svg" alt="A closed set F and two points in disjoint open neighborhoods before identification." />
  <figcaption>Figure 5.5</figcaption>
</figure>
<figure class="topology-chapter-figure">
  <img src="/elementary-topology/figures/figure-5.6.svg" alt="The closed set F collapsed to a single equivalence class in the quotient space." />
  <figcaption>Figure 5.6</figcaption>
</figure>
</div>

The sets $U$ and $V$ may be chosen in $X-F$, since $X-F$ is a $T_2$-subspace of $X$, and since a subset of $X-F$, an open set, is open in $X-F$ if and only if it is open in $X$. Then $\bar{U}=\{\bar{u}\mid u\in U\}$ and $\bar{V}=\{\bar{v}\mid v\in V\}$ are open disjoint subsets of $X/R$ such that $\bar{x}\in\bar{U}$ and $\bar{y}\in\bar{V}$.

If either $x\in F$, or $y\in F$, then the other point could not be in $F$. For if $x$ and $y$ are both in $F$, then $\bar{x}=\bar{y}=F$, contradicting $\bar{x}\ne\bar{y}$. Suppose $\bar{x}=F$. Then there are open subsets $U$ and $V$ of $X$ such that

$$
\bar{x}=F\subset U,\qquad y\in V,\qquad\text{and}\qquad U\cap V=\phi.
$$

It follows that $\bar{U}=\{\bar{u}\mid u\in U\}$ and $\bar{V}=\{\bar{v}\mid v\in V\}$ are disjoint open subsets of $X/R$ such that $\bar{x}\in\bar{U}$ and $\bar{y}\in\bar{V}$. Therefore $X/R$ is $T_2$.

<figure class="topology-chapter-figure">
  <img src="/elementary-topology/figures/figure-5.7.svg" alt="A torus, the product of two circles, viewed as a regular subspace of three-dimensional space." />
  <figcaption>Figure 5.7</figcaption>
</figure>

**Example 11.** In Example 14 of Chapter 4, a circle is obtained by identifying 0 and 1 in $[0,1]$. Since $\{0,1\}$ is a closed subset of $[0,1]$, we know that the circle is at least a $T_2$-space. Since the circle is a subspace of a metric space $R^2$, and any metric space is regular, we have the stronger result that a circle is a regular space. The torus $C\times C$ (Fig. 5.7), where $C$ is a circle, is regular since it is the product of regular spaces. The torus is also seen to be regular because it is a subspace of $R^3$.

## Exercises

1. In regard to Example 10,

   a) verify that the family of $\mathfrak{N}_x$ forms an open neighborhood system for a topology on $R$;

   b) show that $R$ with this topology is $T_2$.

::: warning Missing source page 102
Printed page 102 is absent. The next supplied page has running section label 5.4 and begins with Example 12. Any further §5.3 exercises and the opening of §5.4, including its definition, are unavailable and have not been reconstructed.
:::

<style scoped>
.topology-chapter-figure {
  max-width: 28rem;
  margin: 1.75rem auto;
  text-align: center;
}
.topology-chapter-figure img {
  width: 100%;
  background: white;
}
.topology-chapter-figure figcaption {
  font-family: Georgia, serif;
  font-size: 0.875rem;
  margin-top: 0.4rem;
}
.topology-chapter-figure-pair {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 1.5rem;
  margin: 1.75rem 0;
}
.topology-chapter-figure-pair .topology-chapter-figure {
  margin: 0;
}
@media (max-width: 480px) {
  .topology-chapter-figure-pair {
    grid-template-columns: 1fr;
  }
}
</style>

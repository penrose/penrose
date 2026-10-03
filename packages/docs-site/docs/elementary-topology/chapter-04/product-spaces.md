---
title: Product Spaces — Elementary Topology
description: The available text of section 4.6 of the supplied second-edition scan.
---

<script setup>
import ReadingAdditions from '../../../src/elementary-topology/ReadingAdditions.vue';
</script>

# 4.6 Product Spaces

::: info Transcription note
Source: printed pages 84 and 86–90 (PDF pages 85–90). Printed page 85 is missing. The source uses a large cross for Cartesian products, retained here as $\mathop{\Large\times}_I S_i$. Definitions and propositions on the missing page are not reconstructed. The formulas and indexing in the supplied proofs are retained as printed.
:::

<span id="printed-page-84"></span>

<!-- Source: PDF page 85, printed page 84; section 4.6 fragment. -->

The reader has undoubtedly encountered the concept of the Cartesian product of finitely many sets in previous studies. If $S_1,S_2,\ldots,S_n$ are sets, then the _Cartesian product_ of these sets $\mathop{\Large\times}_{i=1}^n S_i$ is defined by

$$
\mathop{\Large\times}\limits_{i=1}^n S_i=\{(s_1,s_2,\ldots,s_n)\mid s_i\in S_i,\ i=1,\ldots,n\}.
$$

That is, the _Cartesian product_, or simply the _product_, of the $S_i$ is the set of ordered $n$-tuples of elements of the $S_i$. The coordinate plane is nothing but the Cartesian product $R\times R$, where $R$ is the set of real numbers. The $i$th place in an ordered $n$-tuple is usually called the $i$th _coordinate_.

If, however, the reader has already adjusted to the $n$-tuple definition of the product of $n$ sets, then he may find it somewhat hard to begin the study of product topological spaces by having to learn a new and more general definition of the product of a family of sets—one which allows us to take the product of infinitely many sets as well as finitely many. In the body of this text, we will extend the definition of the product so that we can deal with the product of countably many sets. The Appendix gives the definition and some properties of more general products. Wherever possible, proofs about product spaces will be given in a form which easily adapts to the more general definition of a product. It should be kept in mind, however, that not all statements about finite or countable products are true when applied to the product of an arbitrary family of sets or topological spaces.

**Definition 6.** Let $\{S_i\}$, $i\in I$, be a family of sets, where $I$ is a countable index set (either finite or infinite). We may then choose $I$ either to be $\{1,2,\ldots,n\}$, where $n$ is an appropriate positive integer, or to be the set of positive integers. By $\mathop{\Large\times}_I S_i$ we will mean the set of ordered tuples of the form $(s_1,s_2,\ldots,s_i,\ldots)$, where $s_i\in S_i$ for each $i\in I$. If

$$
s=(s_1,\ldots,s_i,\ldots)\in\mathop{\Large\times}_I S_i,
$$

::: warning Missing source page 85
Printed page 85 is absent. The sentence above ends here in the supplied page. The next available page begins with the end of an unavailable statement, followed by its proof. No missing continuation, Definition 7, Example 16, or proposition statement is reconstructed. References to that material are retained in the available text below.
:::

<span id="printed-page-86"></span>

<!-- Source: PDF page 86, printed page 86. The opening is a source fragment. -->

is continuous. Moreover, $\tau$ is the coarsest topology on $\mathop{\Large\times}_I S_i$ for which each $p_i$ is continuous.

_Proof._ The product topology has been specifically defined so as to make each projection continuous. If any topology on $\mathop{\Large\times}_I S_i$ were strictly coarser than the product topology, then some member of $\mathfrak{S}$, the subbasis for $\tau$, would not be open; hence at least one of the projections could not be continuous.

**Example 17.** It is not true that the product space of countably many discrete spaces necessarily has the discrete topology, although the product topology will be discrete if only finitely many spaces are involved. Let $I$ be the set of positive integers. For each $i\in I$, let $S_i=\{1,2\}$ with the discrete topology. Set $U=\mathop{\Large\times}_I W_i$, where $W_i=\{1\}$ for each $i\in I$. Then $U$ is the product of open subsets of the $S_i$ (specifically, $U=\{(1,1,\ldots,1,\ldots)\}$), but $U$ is not open. The proof that $U$ is not open is left as an exercise. It is, however, a rather easy corollary of Proposition 19.

<div class="topology-chapter-figure-pair">

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-4.8.svg" alt="Figure 4.8: A vertical open strip as a product-topology subbasis element." /><figcaption>Figure 4.8</figcaption></figure>

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-4.9.svg" alt="Figure 4.9: Intersecting open strips form an open rectangle." /><figcaption>Figure 4.9</figcaption></figure>

</div>

**Example 18.** Let $R$ be the space of real numbers with the absolute value topology. We will show that the product topology on the plane $R^2=R\times R$ is the same as the topology on $R^2$ induced by the Pythagorean metric $D$. The topology induced by $D$ is the same as the topology induced by the metric $D_3$ of Chapter 2, Example 3 (Section 3.2, Exercise 2). A typical subbasis element for the product topology on $R^2$ is shown in Fig. 4.8. This means that a typical basis element for the product topology is given by Fig. 4.9 (see Proposition 19). Each element of the basis for the product topology is $D_3$-open, and each $D_3$-$\rho$-neighborhood of any point of $R^2$ is exactly a basis element of the product topology. We can easily verify that a basis for the product topology is also a basis for the topology induced by $D_3$; hence the topologies are the same.

<span id="printed-page-87"></span>

<!-- Source: PDF page 87, printed page 87. -->

Note that not every open set of the product topology is of the form $U\times V$, where $U$ and $V$ are open subsets of $R$. For example,

$$
\{(x,y)\mid x^2+y^2<1\}
$$

is open, but is not a product set.

**Proposition 19.** Let $\mathop{\Large\times}_I S_i,\tau$ be the product space of the countable family of spaces $\{S_i,\tau_i\}$, $i\in I$. Set

$$
\begin{gathered}
\mathfrak{B}=\bigl\{\mathop{\Large\times}_I V_i\mid V_i\text{ is open in }S_i,\text{ and }V_i=S_i\\
\text{for all but at most finitely many }i\bigr\}.
\end{gathered}
$$

Then $\mathfrak{B}$ is a basis for $\tau$.

_Proof._ Let $\mathfrak{S}$ be the subbasis for $\tau$ described in Definition 7. Then a basis for $\tau$ is obtained by taking all finite intersections of members of $\mathfrak{S}$ (Chapter 3, Definition 4). Suppose $U_1,\ldots,U_n$ are elements of $\mathfrak{S}$, with $U_k=\mathop{\Large\times}_I W_i$, where $W_i=S_i$ for all $i$, except possibly $i=i_k$, $k=1,\ldots,n$. Then

$$
U_1\cap U_2\cap\cdots\cap U_n=\mathop{\Large\times}_I V_i,
$$

where $V_i=S_i$, except possibly for $i_1,i_2,\ldots,i_k$. This means that each element of the basis derived from $\mathfrak{S}$ is also a member of $\mathfrak{B}$; but each member of $\mathfrak{B}$ is the intersection of finitely many members of $\mathfrak{S}$. Therefore $\mathfrak{B}$ is a basis for $\tau$.

**Corollary.** If $I$ is finite, then a basis for $\tau$ consists of all sets of the form $\mathop{\Large\times}_I V_i$, where $V_i$ is open in $S_i$.

**Proposition 20.** Suppose $\mathop{\Large\times}_I S_i,\tau$ is the product space of the nonempty spaces $\{S_i,\tau_i\}$, $i\in I$. Then $S_i,\tau_i$ is homeomorphic to a subspace of $\mathop{\Large\times}_I S_i,\tau$ for each $i\in I$.

_Proof._ We lose no generality in proving this proposition for $S_1,\tau_1$, since the same proof could be used for any $i\in I$. Let $y_2,\ldots,y_i,\ldots$ be fixed points of $S_2,\ldots,S_i,\ldots$, respectively. Define the function $q_1$ from $S_1$ into $\mathop{\Large\times}_I S_i$ by

$$
q_1(x)=(x,y_2,\ldots,y_i,\ldots)
$$

for each $x\in S_1$. Let $Y$ be the subspace of $\mathop{\Large\times}_I S_i$, defined by

$$
Y=\{(x,y_2,\ldots,y_i,\ldots)\mid x\in S_1\}.
$$

Then $q_1$ takes $S_1$ onto $Y$; moreover, $q_1$ is one-one. It remains to show that $q_1$ and $q_1^{-1}$ are continuous.

Assume $U$ an open subset of $Y$. Then $U=Y\cap U'$, where $U'$ is an open subset of $\mathop{\Large\times}_I S_i$. If $p_1$ is the projection into the first component, then <span id="printed-page-88"></span><!-- Source: PDF page 88, printed page 88. --> $p_1(U')$ is an open subset of $S_1$ (Exercise 2). But it is readily seen that $q_1^{-1}(U)=p_1(U')$. Therefore $q_1^{-1}(U)$ is an open subset of $S_1$, and thus $q_1$ is continuous. On the other hand, suppose $V$ is an open subset of $S_1$. Then

$$
q_1(V)=(q_1^{-1})^{-1}(V)=Y\cap\mathop{\Large\times}_I W_i,
$$

where $W_i=S_i$, $i\geq 2$, and $W_1=V$. It follows that $(q_1^{-1})^{-1}(V)$ is an open subset of $Y$; hence $q_1^{-1}$ is continuous. Therefore $q_1$ is a homeomorphism.

**Example 19.** Let $R$ be the set of real numbers with the absolute value topology, and let $R^2$ be the plane with the product topology. Figure 4.10 illustrates one possible embedding of $R$ as a subspace of $R^2$. One generally thinks of the $x$-axis as being the real line, whereas, strictly speaking, it is a space which is homeomorphic to the real line. Note that even when restricting oneself to the procedure of Proposition 20, there are uncountably many subspaces of $R^2$ homeomorphic to $R$.

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-4.10.svg" alt="Figure 4.10: Embedding the real line as the x-axis of the coordinate plane." /><figcaption>Figure 4.10</figcaption></figure>

**Proposition 21.** Suppose $f$ is a function from a space $X,\tau$ into the product space $\mathop{\Large\times}_I S_i,\tau'$. Define $f_i:X,\tau\to S_i,\tau_i$ by $f_i(x)=p_i\circ f(x)$ for each $x\in X$, where $p_i$ is the projection into the $i$th component. Then $f$ is continuous if and only if $f_i$ is continuous for each $i\in I$.

_Proof._ If $f$ is continuous, then $f_i=p_i\circ f$ is the composition of two continuous functions and therefore is continuous.

Assume now that $f_i$ is continuous for each $i\in I$. We will use Propositions 7 and 19. We first note that

$$
f(x)=(f_1(x),f_2(x),\ldots,f_i(x),\ldots)
$$

for each $x\in X$. Suppose $\mathop{\Large\times}_I V_i$ is any member of the basis $\mathfrak{B}$ for $\tau'$ described in Proposition 19, where $V_i=S_i$ for each $i\in I$, except $i_1,\ldots,i_m$. Now $f^{-1}(\mathop{\Large\times}_I V_i)$ is the set of all points $x$ of $X$ such that $f(x)\in\mathop{\Large\times}_I V_i$. But this is easily seen to be $\bigcap_I f_i^{-1}(V_i)$. For every $i$, except $i_1,\ldots,i_m$,

$$
f_i^{-1}(V_i)=X\qquad\text{(because }V_i=S_i\text{)}.
$$

Since $f_i$ is continuous for each $i\in I$, $f_{i_j}^{-1}(V_{i_j})$ is open in $X$ for $j=1,\ldots,m$.

<span id="printed-page-89"></span>

<!-- Source: PDF page 89, printed page 89. -->

Therefore

$$
f^{-1}\left(\mathop{\Large\times}_I V_i\right)=f_{i_1}^{-1}(V_{i_1})\cap\cdots\cap f_{i_m}^{-1}(V_{i_m}),
$$

which is open in $X$ since it is the intersection of finitely many open sets. Hence, by Proposition 7, $f$ is continuous.

Proposition 21 is extremely important in the study of product spaces. Note that its proof would not have gone through if we had defined a subbasis of the product topology to consist of sets of the form $\mathop{\Large\times}_I V_i$, where $V_i$ is open in $S_i$, since we could not have been sure then that $\bigcap_I f_i^{-1}(V_i)$ was an open subset of $X$. (Note, however, with this topology that each projection is still continuous.) Proposition 21 is, in fact, another good reason why the product topology was defined as it was.

**Example 20.** Let $R$ be the space of real numbers with the absolute value topology, and let $R^2$ be the plane with product topology from $R$. Define $f:R\to R^2$ by

$$
f(x)=(\sin x,3x+1)
$$

for each $x\in R$. Then $f_1(x)=\sin x$ and $f_2(x)=3x+1$ for each $x\in R$. Since $f_1$ and $f_2$ are both continuous functions from $R$ into $R$, $f$ is continuous.

## Exercises

1. Suppose that $\{S_i\}$, $i\in I$, is any countable family of sets, and that $S_i=\phi$, for some $i$. Prove $\mathop{\Large\times}_I S_i=\phi$.

2. A function $f$ from a space $X,\tau$ to a space $Y,\tau'$ is said to be _open_ if $f(V)$ is open in $Y$ whenever $V$ is an open subset of $X$. Prove that the projection $p_i$ from the product space $\mathop{\Large\times}_I S_i,\tau$ into $S_i,\tau_i$ is open for each $i\in I$.

3. Prove that the product space of a countable family of spaces, each with the trivial topology, has the trivial topology.

4. Each of the sets involved in the following is to be considered as a subspace of the space $R$ of real numbers with the absolute value topology. Sketch each of the following spaces.

   a) $[0,1]\times[0,1]$

   b) $\{0,1\}\times[0,1]$

   c) $\{0,1\}\times R$

   d) $\{x\mid x>0\}\times\{x\mid x\text{ is an integer greater than }1\}$

5. Let $C=\{(x,y)\mid x^2+y^2=1\}\subset R^2$ with the Pythagorean topology. Describe

   a) $C\times C$;

   b) $C\times[0,1]$;

   c) $C\times(0,1)$.

6. Prove that the set $U$ in Example 17 is not an element of the product topology.

7. Let $X,\tau$ be any space, and let $X\times X$ have the product topology. The _diagonal_ $\Delta$ of $X\times X$ is defined by $\Delta=\{(x,x)\mid x\in X\}$. Prove that $X$ is homeomorphic to $\Delta$.

8. Let $\{S_i,\tau_i\}$, $i\in I$, be a countable family of spaces, and let $P$ be a permutation of $I$. Prove that the product spaces $\mathop{\Large\times}_I S_i$ and $\mathop{\Large\times}_I S_{P(i)}$ are homeomorphic.

   <span id="printed-page-90"></span>

   <!-- Source: PDF page 90, printed page 90. The first sentence continues Exercise 8. -->

   That is, the order in which the components are used in the product does not affect the topological character of the product space.

9. Consider the space $N$ described in Exercise 6 of Section 3.1 and Exercise 5 of Section 4.4. Prove or disprove: The product space $N\times N$ is homeomorphic to $N$.

<style>
.topology-chapter-figure { max-width: 28rem; margin: 1.75rem auto; text-align: center; }
.topology-chapter-figure img { width: 100%; background: white; }
.topology-chapter-figure figcaption { font-family: Georgia, serif; font-size: 0.875rem; margin-top: 0.4rem; }
.topology-chapter-figure-pair { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 1.5rem; margin: 1.75rem 0; }
.topology-chapter-figure-pair .topology-chapter-figure { margin: 0; }
@media (max-width: 480px) { .topology-chapter-figure-pair { grid-template-columns: 1fr; } }
</style>

<ReadingAdditions :pdf-page="87" />

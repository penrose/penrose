---
title: Baire Category Theorem — Elementary Topology
---

# 10.4 Baire Category Theorem

<span id="printed-page-223"></span>

<!-- Source: PDF208, printed223, section10.4 fragment. -->

We now come to a theorem of great importance in mathematics, particularly in the construction of _existence proofs_ in analysis. An existence proof is a proof which shows that something exists, or can be found at least in theory, even if we cannot actually come up with a specific example of what exists. For example, we may wish to know that such and such an equation has a solution even if we cannot find the solution, or that such and such a function exists even if we cannot at the moment construct an example of the function. To know that something either can or cannot be done either encourages us to try to do it, or saves us the time and effort of trying. Unfortunately, there will always be intrepid unbelievers who will insist upon trying to trisect angles with ruler and compass, square circles, and solve quintic equations by radicals.

We now state and prove the famous _Baire Category Theorem_.

**Proposition 12.** Let $X,D$ be a nonempty complete metric space. Then the following hold:

a) If $X$ is expressed as the union of countably many subsets $A_1,A_2,\ldots,A_n,\ldots$, then at least one of the $A_n$ is somewhere dense. That is, for one of the $A_n$, $\operatorname{Cl}A_n$ contains an open subset of $X$.

b) If $U_1,U_2,\ldots$ are countably many dense open subsets of $X$, then $\bigcap_N U_n$ is dense in $X$, that is, $\operatorname{Cl}(\bigcap_N U_n)=X$.

<span id="printed-page-224"></span>

<!-- Source: PDF209, printed224. -->

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-10.5.svg" alt="Nested circular neighborhoods and closed balls with centers and radii labeled, lying inside a larger circular region in the Baire category proof." />
<figcaption>Figure 10.5. <a href="/docs/elementary-topology/reader?page=224">View the interactive figure and its Substance program.</a></figcaption>
</figure>

_Proof_

a) If (a) is false, then there is a countable family $\{A_n\}$, $n\in N$, of subsets of $X$ such that $X=\bigcup_N A_n$, but $(\operatorname{Cl}A_n)^\circ=\phi$ for each $n\in N$. For each $n$ then, $\operatorname{Cl}A_n\ne X$. Select $b_1\in X-\operatorname{Cl}A_1$. Since $X-\operatorname{Cl}A_1$ is open, there is a positive number $p_1<1$ such that

$$
N(b_1,p_1)\subset X-\operatorname{Cl}A_1.
$$

Set $B_1=N(b_1,p_1/2)$ (Fig. 10.5). Then $\operatorname{Cl}B_1\subset N(b_1,p_1)$; hence

$$
\operatorname{Cl}B_1\cap\operatorname{Cl}A_1=\phi.
$$

Now $B_1$ is a nonempty open subset of $X$, and therefore $B_1\not\subset\operatorname{Cl}A_2$. Choose $b_2\in B_1-\operatorname{Cl}A_2$. Since $B_1-\operatorname{Cl}A_2$ is open, there is $p_2>0$ such that $N(b_2,p_2)\subset B_1-\operatorname{Cl}A_2$. We lose no generality in further requiring that $p_2<\tfrac12$. Set $B_2=N(b_2,p_2/2)$. Then

$$
B_2\subset B_1\quad\text{and}\quad\operatorname{Cl}B_2\cap\operatorname{Cl}A_2=\phi.
$$

Proceeding in like fashion, we can obtain a decreasing sequence of open $p_n$-neighborhoods $B_1\supset B_2\supset\cdots\supset B_n\supset\cdots$ such that $\operatorname{Cl}B_n\cap\operatorname{Cl}A_n=\phi$ and $p_n<1/n$. Then

$$
\operatorname{Cl}B_1\supset\operatorname{Cl}B_2\supset\cdots\supset\operatorname{Cl}B_n\supset\cdots\quad\text{and}\quad d(B_n)\to0.
$$

Therefore by Proposition 7, $\bigcap_N\operatorname{Cl}B_n\ne\phi$. Pick $x\in\bigcap_N B_n$. Then $x\in A_n$ for some $n$, since $\bigcup_N A_n=X$. But then

$$
x\in\operatorname{Cl}A_n\cap\operatorname{Cl}B_n,
$$

which is impossible, since $\operatorname{Cl}A_n$ and $\operatorname{Cl}B_n$ are disjoint. Therefore (a) is proved.

::: info Source formula
The instruction “Pick $x\in\bigcap_N B_n$” is retained as printed, following the preceding statement about $\bigcap_N\operatorname{Cl}B_n$.
:::

<span id="printed-page-225"></span>

<!-- Source: PDF210, printed225. -->

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-10.6.svg" alt="A large circular region T, an overlapping irregular open region Ui, a small shaded neighborhood, and labeled points used in the dense-intersection argument." />
<figcaption>Figure 10.6. <a href="/docs/elementary-topology/reader?page=225">View the interactive figure and its Substance program.</a></figcaption>
</figure>

b) Suppose $\{U_n\}$, $n\in N$, is a countable family of dense open subsets of $X$. In order to prove that $\bigcap_N U_n$ is dense, it is sufficient to prove that each neighborhood of any point of $X$ meets $\bigcap_N U_n$. Choose any $x\in X$ and any $p>0$; we will show that

$$
N(x,p)\cap\left(\bigcap_N U_n\right)\ne\phi.
$$

(This suffices to prove statement (b), since the collection of $p$-neighborhoods is a basis for the topology induced by $D$.) Set

$$
T=\operatorname{Cl}N(x,p/2);
$$

then $T\subset N(x,p)$. We now show that $T\cap(\bigcap_N U_n)\ne\phi$. Since $T$ is closed, the subspace $T$ is itself a complete metric space (Proposition 9). Set $A_n=T-U_n$. Since

$$
A_n=T-U_n=T\cap(X-U_n),
$$

the intersection of two closed subsets of $X$, $A_n$ is closed in both $X$ and $T$.

Suppose $A_n$ is somewhere dense. Then there is $t\in T$ and $q>0$ such that

$$
N(t,q)\cap T\subset\operatorname{Cl}A_n\cap T=A_n.
$$

Therefore $N(t,q)\cap(T-A_n)=\phi$. Now $t\in T=\operatorname{Cl}N(x,p/2)$ (Fig. 10.6); hence $N(t,q)$ meets $N(x,p/2)$ in some point $z$. We may choose $q'>0$ such that

$$
N(z,q')\subset N(t,q)\cap N(x,p/2).
$$

<span id="printed-page-226"></span>

<!-- Source: PDF211, printed226. -->

But since $U_n$ is dense, $N(z,q')$ intersects $U_n$, say, in $z'$. Then

$$
z'\in T\cap N(t,q)\subset A_n.
$$

But $A_n=T-U_n$, and hence $z'\in T-U_n$; that is, $z'\notin U_n$, a contradiction. Therefore $A_n$ must be nowhere dense in $T$.

By (a) then, $T\ne\bigcup_N A_n$ (remember that $T$ is a complete metric space); thus there is $s\in T-\bigcup_N A_n$. Therefore, since $A_n=T-U_n$, $s\in T\cap(\bigcap_N U_n)$. Then $T\cap(\bigcap_N U_n)\ne\phi$, and hence

$$
N(x,p)\cap\left(\bigcap_N U_n\right)\ne\phi.
$$

This completes the proof of (b).

A topological space $X,\tau$ which is the union of countably many subsets each of which is nowhere dense in $X$ is said to be of the _first category_. Otherwise, $X$ is said to be of the _second category_. Proposition 12 thus states that every complete metric space is of the second category.

**Example 9.** Assign a positive integer $n$ to each real number. Set

$$
A_n=\{x\in R\mid\text{such that the positive integer }n\text{ has been assigned to }x\}.
$$

Then $R=\bigcup A_n$. Since $R$ with the absolute value metric is a complete metric space, at least one of the $A_n$ must be somewhere dense in $R$. Actually, we can show that some $A_n$ is dense in any closed interval.

Note that since the subspace $Q$ of rational numbers is countable, we could assign a different positive integer to each rational number, and thus $Q$ could be expressed as the union of countably many subsets of $Q$ each of which is nowhere dense in $Q$.

**Example 10.** The plane $R^2$ with the Pythagorean metric is a complete metric space. Any straight line $L$ in $R^2$ is a closed subset of $R^2$; moreover, $R^2-L$ is an open dense subset of $R^2$. If $L_1$ and $L_2$ are any two lines in $R^2$, then

$$
(R^2-L_1)\cap(R^2-L_2)=R^2-(L_1\cup L_2).
$$

By Proposition 11, however, $(R^2-L_1)\cap(R^2-L_2)$ is a dense subset of $R^2$. In general, we may remove countably many straight lines from the plane and still have what remains a dense subset of $R^2$ (though it may not be open).

Proposition 12 is used in existence proofs in the following ways. A suitable complete metric space is first constructed. Suppose we wish to prove that something exists which does not have a certain property $P$. We express the set of elements of $X$ which have $P$ as the union of countably <span id="printed-page-227"></span><!-- Source: PDF212, printed227. -->many nowhere dense subsets of $X$. Since this union could not be all of $X$ by Proposition 12(a), there must be some element of $X$ which does not have $P$. This approach is used to show that there is a continuous function from the space of real numbers to the space of real numbers which is nowhere differentiable. If we wish to show that some element of $X$ has a given property $Q$, we find countably many conditions such that if an element of $X$ satisfies all of the conditions, then that element has $Q$. If the countable family of conditions is such that the set of elements of $X$ which satisfy any one of the given conditions is an open dense subset of $X$, then there is an element which satisfies them all, and hence has $Q$, by Proposition 12(b). For examples and details of some existence theorems from analysis which use Baire's theorem, the reader is referred to Chapter 13, Section 4.2 of Dugundji, _Topology_ (Allyn & Bacon, 1964).

## Exercises

1. Use Proposition 12 to show that a complete metric space which is connected must contain uncountably many points if it contains more than one.

2. Suppose at each point of $R^2$ (with the Pythagorean metric) we draw a circle with integral radius. Is it necessarily true that the set of circles with radius $n$ is somewhere dense in $R^2$ for at least one positive integer $n$?

3. Prove that the union of countably many nowhere dense, closed subsets of a complete metric space can still be dense. Why is this not a contradiction to Proposition 11(b)? [*Hint:* Consider the rationals in the space of reals.]

4. Prove that if $X,D$ is a complete metric space, then the removal of countably many closed, nondense subsets of $X$ still leaves a dense subset of $X$. Does this remain true if the subsets removed are not required to be closed?

5. The results obtained in Exercise 5 of Section 10.3 are also used in some important existence proofs. Indicate how an existence proof which uses these results might be constructed.

6. Which of the following subspaces of the usual space $R$ of real numbers are of the second category?

   a) The irrational numbers

   b) $\{x\mid0<x\le1\}$

   c) $\{x\mid x=1/n,\ n\text{ a positive integer, or }x=0\}$

   d) $(0,1)\cup(3,4)$

7. Suppose $Y$ and $Z$ are subspaces of some space $X,\tau$ and $Y$ and $Z$ are both of the second category. Decide whether each of the following must be of the second category.

   a) $X\cap Y$

   b) $X\cup Y$

   c) the product space $X\times Y$

   d) $X-Y$

8. Prove that every locally compact metric space $X,D$ can be given a metric $D'$ such that $D'$ is equivalent to $D$ and $X,D'$ is complete. Hence each locally compact metric space is of the second category.

::: info Source references and variables
Example 10 and Exercise 3 reference Proposition 11, retained as printed. Exercise 7 introduces $Y$ and $Z$ and then asks about expressions involving $X$ and $Y$; those variables are also retained.
:::

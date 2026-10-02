---
title: 7.4 Derived Spaces, Separation Axioms, and Compactness
---

# 7.4 The Derived Spaces and Compactness. The Separation Axioms and Compactness

<span id="printed-page-156"></span>

<!-- Source: PDF149, printed156, Section7.4 fragment. -->

It is not necessarily true that any subspace of a compact space is compact. For example, $(0,1)$ is not compact (Example 8), whereas $[0,1]$ is compact (Proposition 7). We do though have some information about which subspaces of a compact space are compact.

**Proposition 10.** Any closed subset of a compact space is compact.

_Proof._ Let $A$ be a closed subset of a compact space $X,\tau$, and suppose $\{U_i\}$, $i\in I$, is any open cover of $A$. Then since $A$ is closed, $X-A$ is open; hence $\{X-A\}\cup\{U_i\mid i\in I\}$ is an open cover of $X$. Since $X$ is compact, $X-A$ together with finitely many of the $U_i$, say $U_{i_1},\ldots,U_{i_n}$ form a cover of $X$. Therefore $\{U_{i_1},\ldots,U_{i_n}\}$ is a finite subcover of $A$, and hence $A$ is compact.

A partial converse to Proposition 10 is given by

**Proposition 11.** Any compact subset of a $T_2$ space is closed.

<figure class="topology-chapter-figure">
  <img src="/elementary-topology/figures/figure-7.3.svg" alt="A compact set A and an exterior point x are separated using finitely many neighborhoods and their intersection." />
  <figcaption>Figure 7.3</figcaption>
</figure>

_Proof._ Let $A$ be a compact subset of a $T_2$-space $X,\tau$ (Fig. 7.3) and suppose $x\in X-A$. We must find a neighborhood of $x$ which does not meet $A$

::: warning Missing printed page 157
The supplied scan skips printed page 157. The remainder of Proposition 11’s proof and the intervening statements are unavailable. Printed page 158 begins within another proof, whose statement is missing. Later source references to Proposition 12 are preserved without reconstructing it.
:::

<span id="printed-page-158"></span>

<!-- Source: PDF150, printed158. -->

_Proof._ Let $\{U_i\}$, $i\in I$, be any open cover of $Y$. Since $f$ is continuous, $\{f^{-1}(U_i)\}$, $i\in I$, is an open cover of $X$. Since $X$ is compact, we can find finitely many $U_i$, say $U_{i_1},\ldots,U_{i_n}$, such that $\{f^{-1}(U_{i_1}),\ldots,f^{-1}(U_{i_n})\}$ is an open cover of $X$. But then $\{U_{i_1},\ldots,U_{i_n}\}$ is a finite open subcover of $\{U_i\}$, $i\in I$. Therefore $Y$ is compact.

Since any homeomorphism is continuous, we have the following.

**Corollary.** If $X,\tau$ is compact, then any space homeomorphic to $X$ is compact.

**Example 11.** Proposition 11 enables us to find many more compact spaces. For example, if $X,\tau$ is a compact space, $R$ is an equivalence relation on $X$, and $X/R$ is the identification space, then $X/R$ is compact, since the identification mapping from $X$ onto $X/R$ is continuous. Since the circle is an identification space derived from $[0,1]$ (Chapter 4, Example 14), the circle is compact. The next proposition will give us even more compact spaces.

**Proposition 13** (_Tychonoff theorem_). Let $\mathop{\Large\times}_I X_i$ be the product space of the countable family of nonempty spaces $\{X_i,\tau_i\}$, $i\in I$. Then $\mathop{\Large\times}_I X_i$ is compact if and only if each component space is compact.

_Proof._ Suppose $\mathop{\Large\times}_I X_i$ is compact. Since the projection map

$$
p_i:\mathop{\Large\times}_I X_i\to X_i
$$

is continuous and onto for each $i\in I$, $X_i$ is compact for each $i\in I$ (Proposition 12).

Suppose each $X_i$ is compact. Let $\{s_j\}$, $j\in J$, be any ultranet in $\mathop{\Large\times}_I X_i$ with the $i$th coordinate of $s_j$ being denoted by $s_j(i)$. Then

$$
\{p_i(s_j)\}=\{s_j(i)\},\quad j\in J,
$$

is an ultranet in $X_i$ by Proposition 22, Chapter 6. Therefore $\{s_j(i)\}$ converges in $X_i$ by the corollary to Proposition 9 of this chapter. But then $\{s_j\}$, $i\in I$, converges in $\mathop{\Large\times}_I X_i$ by Proposition 12 of Chapter 6. Therefore $\mathop{\Large\times}_I X_i$ is compact by the corollary to Proposition 9.

**Example 12.** We have already seen that the closed interval $[0,1]$ and the circle $C$ are compact. Using Proposition 13, we can now say that $([0,1])^n$ is compact for any $n$. Then cylinder $C\times[0,1]$, the torus $C\times C$, and the cube $([0,1])^3$ are all examples of compact spaces (Figs. 7.4, 7.5, and 7.6).

Often one of the hardest steps in proving that some function is a homeomorphism is showing that its inverse is continuous. The next proposition affords us some relief in certain special (though important) instances.

::: info Source references and indices
Example 11 cites Proposition 11 as printed. Proposition 13’s statement specifies a countable family, and its last proof paragraph prints $\{s_j\}$, $i\in I$; these source readings are retained.
:::

<span id="printed-page-159"></span>

<!-- Source: PDF151, printed159. -->

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-7.4.svg" alt="The compact cylinder with its endpoint circles and interval fiber." />
<figcaption>Figure 7.4. <a href="/docs/elementary-topology/reader?page=159">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-7.5.svg" alt="The compact torus with its two circle fibers and their shared point." />
<figcaption>Figure 7.5. <a href="/docs/elementary-topology/reader?page=159">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-7.6.svg" alt="The compact cube with its two marked boundary faces." />
<figcaption>Figure 7.6. <a href="/docs/elementary-topology/reader?page=159">View the interactive figure and its Substance program.</a></figcaption>
</figure>

**Proposition 14.** Let $f$ be a continuous one-one function from a compact space $X,\tau$ onto a $T_2$-space $Y,\tau'$. Then $f$ is a homeomorphism.

_Proof._ We must show that $f^{-1}$ is continuous. We use Proposition 8, Chapter 4. Suppose $F$ is any closed subset of $X$. Since $F$ is closed, $F$ is compact (Proposition 10); hence $f(F)$ is compact (Proposition 12). Then $f(F)$ is a compact subset of a $T_2$-space and is therefore closed (Proposition 11). But

$$
f(F)=(f^{-1})^{-1}(F).
$$

We have therefore shown that if $F$ is any closed subset of $X$, $(f^{-1})^{-1}(F)$ is a closed subset of $Y$. Therefore $f^{-1}$ is continuous; hence $f$ is a homeomorphism.

Because of the great importance of the Tychonoff theorem (Proposition 13), we now present another proof which does not depend on the material of Chapter 6. We first prove another criterion for compactness.

**Proposition 15.** A space $X,\tau$ is compact if and only if there is a subbasis $\mathfrak{S}$ such that whenever $\mathfrak{C}$ is a cover of $X$ consisting of elements of $\mathfrak{S}$, then $\mathfrak{C}$ contains a finite subcover of $X$.

_Proof._ If $X$ is compact, then $\tau$ itself serves as a subbasis for $\tau$ having the required property.

Suppose now that $X$ has a subbasis $\mathfrak{S}$ having the property stated; we now prove that $X$ is compact. Let $\mathfrak{A}$ be any collection of open sets which does not contain a finite subcover of $X$. We will show that $\mathfrak{A}$ cannot be a cover of $X$, and, hence, indirectly show that any open cover of $X$ contains a finite subcover.

Let $\mathfrak{X}$ be the collection of all $\mathfrak{B}\subset\tau$ such that $\mathfrak{A}\subset\mathfrak{B}$ but no finite subset of $\mathfrak{B}$ covers $X$. The set $\mathfrak{X}$ is nonempty since $\mathfrak{A}\in\mathfrak{X}$; moreover, $\subset$ is a partial ordering of $\mathfrak{X}$.

Assume $\mathfrak{K}$ is any chain in $\mathfrak{X},\subset$. Then the union of the members of $\mathfrak{K}$ is easily shown to be a member of $\mathfrak{X}$ and is an upper bound for $\mathfrak{K}$. <span id="printed-page-160"></span><!-- Source: PDF152, printed160. -->Therefore, by Zorn’s Lemma, $\mathfrak{X}$ contains a maximal element $\mathfrak{N}$. Since $\mathfrak{A}\subset\mathfrak{N}$, if we show that $\mathfrak{N}$ is not a cover of $X$, then $\mathfrak{A}$ itself will not be a cover of $X$.

Suppose then that $\mathfrak{N}$ is a cover of $X$. Then each $x\in X$ is in some member of $\mathfrak{N}$; assume $x\in M\in\mathfrak{N}$. Now $\mathfrak{S}$ is a subbasis for $\tau$ and $M$ is a member of $\tau$, hence we can find finitely many members $S_1,\ldots,S_n$ of $\mathfrak{S}$ such that

$$
x\in S_1\cap\cdots\cap S_n\subset M.
$$

Suppose that no $S_i$ is a member of $\mathfrak{N}$, $i=1,\ldots,n$. Then $S_i\in\tau-\mathfrak{N}$ for $i=1,\ldots,n$. Because $\mathfrak{N}$ is a maximal element of $\mathfrak{X}$, $\mathfrak{N}\cup\{S_i\}$ must contain a finite subcover of $X$ (or it would be a member of $\mathfrak{X}$ which properly contains $\mathfrak{N}$). Consequently, for $i=1,\ldots,n$, we can find a finite subset $\mathfrak{N}_i$ of $\mathfrak{N}$ such that $S_i$, together with the elements of $\mathfrak{N}_i$, forms a finite open cover of $X$. But since $S_1\cap\cdots\cap S_n\subset M$, it follows that

$$
\{M\}\cup\left(\bigcup_{i=1}^n\mathfrak{N}_i\right)
$$

forms a finite cover of $X$. This, however, is a contradiction, since $\mathfrak{N}$ contains no finite subcover of $X$. This contradiction stems from the assumption that no $S_i$ is a member of $\mathfrak{N}$; therefore assume that $x\in S_i$ for some $i=1,\ldots,n$.

The argument above shows that given any $x\in X$ for which we have some $x\in M\in\mathfrak{N}$, there is some member $S\in\mathfrak{S}$ for which $x\in S\in\mathfrak{N}$. It follows then that $\mathfrak{S}\cap\mathfrak{N}$ covers the same portion of $X$ that $\mathfrak{N}$ does. Thus, if $\mathfrak{N}$ is a cover of $X$, then $\mathfrak{S}\cap\mathfrak{N}$ is also a cover of $X$. But $\mathfrak{S}\cap\mathfrak{N}$ is a subset of $\mathfrak{S}$, and thus contains a finite subcover of $X$. Therefore $\mathfrak{S}\cap\mathfrak{N}$, and hence $\mathfrak{N}$, cannot be a cover of $X$ since $\mathfrak{N}$ contains no finite subcover of $X$.

We have shown then that any collection of open subsets of $X$ which does not contain a finite subcover of $X$ fails to cover $X$. Therefore $X$ is compact.

**Proposition 16** (_Tychonoff product theorem_, proof of which does not use nets or filters). If $\{X_i,\tau_i\}$, $i\in I$, is a nonempty family of nonempty compact spaces, then the product space $\mathop{\Large\times}_I X_i,\tau$ is also compact.

_Proof._ The set

$$
\mathfrak{S}=\{p_i^{-1}(U)\mid U\in\tau_i,\ i\in I\}
$$

forms a subbasis for the product topology $\tau$ (recall that $p_i$ is the projection into the $i$th component). Let $\mathfrak{A}$ be any collection of members of $\mathfrak{S}$ which does not contain a finite subcover of $\mathop{\Large\times}_I X_i$, and for each $i\in I$, set

$$
\mathfrak{A}_i=\{U\mid U\in\tau_i,\ p_i^{-1}(U)\in\mathfrak{A}\}.
$$

<span id="printed-page-161"></span>

<!-- Source: PDF153, printed161. -->

No finite subset of $\mathfrak{A}_i$ can cover $X_i$; for, otherwise,

$$
\{p_i^{-1}(U)\},\ U\in\mathfrak{A}_i,
$$

would be a subcollection of $\mathfrak{A}$ which covers $X$, and from which we could obtain a finite subcover. Since no finite subset of $\mathfrak{A}_i$ covers $X_i$, but $X_i$ is compact, it follows that $\mathfrak{A}_i$ fails to cover $X_i$ for each $i\in I$. Therefore for $i\in I$ we can find

$$
x_i\in X_i-\bigcup\{A\mid A\in\mathfrak{A}_i\}.
$$

Let $x$ be that point of $\mathop{\Large\times}_I X_i$ with $x_i$ as found in the previous sentence as its $i$th coordinate. Then $x$ is a point of $X$ which is not in the union of members of $\mathfrak{A}$. Consequently, $\mathfrak{A}$ does not form a cover of $X$. It follows then that any collection of members of the subbasis $\mathfrak{S}$ which covers $\mathop{\Large\times}_I X_i$ contains a finite subcover of $\mathop{\Large\times}_I X_i$; hence by Proposition 15, $\mathop{\Large\times}_I X_i$ is compact.

## Exercises

1. Decide which of the following spaces are compact. If practicable, sketch a picture of the space. The set $R$ of real numbers, the plane $R^2$, or any subspace of these spaces will be assumed to have the usual metric topology. Products will have the product topology.

   a) $(0,1)\times[0,1]$

   b) $C\times R$, where $C=\{(x,y)\mid x^2+y^2=1\}\subset R^2$

   c) $\{(x,y,z)\mid x^2+y^2+z^2=1\}\subset R^3$

   d) $\{(x,y)\mid x^2+y^2\le1\}\subset R^2$

   e) $N\times C$, where $N$ is the set of positive integers

   f) $\{1,2,3,4,5\}\times C$, with $C$ as in (b)

2. Prove that any subset of $N$ in Example 3 is compact.
3. There is a continuous function from $[0,1]$ onto $[0,1]\times[0,1]$. Prove that this function cannot be one-one.
4. It was shown that the product of normal spaces need not be normal. Prove that the product of compact normal spaces is normal.
5. Let $f$ be the function from $[0,1]$ onto $[0,1]$ (with the absolute value topology) defined by $f(x)=\sin(1/x)$ if $x\ne0$, $f(x)=0$ if $x=0$. Prove that $f$ is not continuous. [*Hint:* Suppose $f$ is a continuous function from a compact space $X$ onto a compact space $Y$. Define $G_f=\{(x,y)\mid y=f(x)\}$. Prove that if $f$ is continuous, then $G_f$ is a closed subset of the product space $X\times Y$. The easiest way to effect this proof is through the use of Proposition 10, Chapter 6. Then $G_f$ is a closed subset of a compact $T_2$-space if $X$ and $Y$ are both compact and $T_2$ (as in the case in this problem). Therefore what can be said about $G_f$?]

<span id="printed-page-162"></span>

<!-- Source: PDF154, printed162. -->

6. Prove that every compact metric space is separable.
7. Suppose $X,D$ is any metric space. A subset $Y$ of $X$ is said to be _bounded_ if $Y\subset N(x,p)$ for some $x\in X$ and $p>0$. Prove that any compact subset of a metric space is both closed and bounded.
8. Suppose $X,\tau$ is a first countable space such that $X$ is $T_1$ and every compact subset of $X$ is closed. Prove that $X$ is $T_2$. [*Hint:* Show that every convergent sequence in $X$ has a unique limit.]
9. Let $X,D$ be a compact metric space and let $\{s_n\}$, $n\in N$, be a sequence in $X$ such that given any $p>0$, there is $m\in N$ such that if $m<n$ and $m<n'$, then $D(s_n,s_{n'})<p$. Prove that $\{s_n\}$, $n\in N$, converges in $X$. Prove that this is not necessarily true if the assumption that $X$ is compact is removed.
10. Prove or disprove: Suppose $X$ and $Y$ are both compact $T_2$-spaces and $f$ is a function from $X$ into $Y$. Then $f$ is continuous if and only if $f$ considered as a subspace of $X\times Y$ is compact.
11. Suppose $X,\tau$ is a space having the property that whenever a subset $A$ of $X$ is compact, then $X-A$ is also compact. Which of the following properties must $X$ also have.

    a) $T_2$  b) $T_1$  c) Every subset of $X$ is compact.

::: info Source wording
Exercise 5’s stated codomain and the word “onto” are retained as printed. Exercise 10 describes the function itself as a subspace, and Exercise 11 ends its question with a period; neither source wording is silently corrected.
:::

[Chapter 7 contents](./index.md)

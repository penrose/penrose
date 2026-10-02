---
title: T₄- and Normal Spaces — Elementary Topology
description: The available text of section 5.4 of the supplied second-edition scan.
---

# 5.4 $T_4$- and Normal Spaces

::: info Transcription note
Source: printed pages 103–106 (PDF pages 101–104). Printed page 102 is absent, so the section's opening and definition are unavailable. The section title is supplied by the running header of printed page 103. The source's use of $X$ in Example 13 is retained. Printed page 106 also begins §5.5, transcribed separately.
:::

::: warning Missing source page 102
The supplied text resumes with Example 12. The missing opening of this section has not been reconstructed.
:::

<span id="printed-page-103"></span>

<!-- Source: PDF page 101, printed page 103. -->

**Example 12.** Any space $X,\tau$ of more than one point with the trivial topology is $T_4$ (there are no nonempty disjoint closed subsets of $X$), but is not normal, since no one-point subset of $X$ is closed. Any metric space is normal (Propositions 5 and 13 of Chapter 2).

Note that any space with the discrete topology has all of the separation properties introduced so far.

We now prove another criterion for a space to be $T_4$.

**Proposition 7.** A space $X,\tau$ is $T_4$ if and only if given any closed subset $F$ of $X$ and any open subset $U$ of $X$ with $F\subset U$, there is an open set $V$ such that

$$
F\subset V\subset\operatorname{Cl}V\subset U.
$$

(Note the similarity between this proposition and Proposition 4 regarding $T_3$-spaces. Note also the difference, namely, $F$ is a set rather than a point. This difference helps us explain the rather bad “hereditary” properties of normal spaces.)

_Proof._ Suppose $X$ is $T_4$, $F$ is a closed subset of $X$, and $U$ is an open set which contains $F$. Then $X-U$ is a closed set and $(X-U)\cap F=\phi$. Hence there are open sets $W$ and $V$ such that

$$
X-U\subset W,\qquad F\subset V,\qquad\text{and}\qquad W\cap V=\phi.
$$

Then $F\subset V\subset\operatorname{Cl}V\subset U$ (since $X-U\subset W$ and $W\cap V=\phi$). Therefore $V$ has the desired properties.

Suppose now that given any closed set $F$ and any open set $U$ which contains $F$, there is an open set $V$ such that $F\subset V\subset\operatorname{Cl}V\subset U$. Let $F$ and $F'$ be any two disjoint closed subsets of $X$. Then $X-F$ is an open set which contains $F'$. By hypothesis, then there is an open set $V$ such that

$$
F'\subset V\subset\operatorname{Cl}V\subset X-F.
$$

Hence $X-\operatorname{Cl}V$ is an open set which contains $F$ and is disjoint from $V$. Therefore $X$ is $T_4$.

It would be very nice to have an analog of Propositions 3 and 5 for normal spaces. Unfortunately, not only is it false that the product of normal spaces is normal; it is even false that every subspace of a normal space is normal. Examples of normal spaces for which some subspace is not normal are somewhat sophisticated for this text. The following example, however, gives a regular space which is not normal, but which is the product of normal spaces.

**Example 13.** Let $R$ be the set of real numbers with the topology $\tau$ as described in Example 11 of Chapter 3. Let $R^2$ be given the product topology. <span id="printed-page-104"></span><!-- Source: PDF page 102, printed page 104. --> Then a typical basic neighborhood of $(x,y)\in R^2$ is as shown in Fig. 5.9. We know that $R,\tau$ is normal (Exercise 1). Each basic neighborhood $U$ of $(x,y)$ is not only open, but also closed. For if $(x',y')$ is any point of $R^2-U$, then it is readily seen that there is a basic neighborhood of $(x',y')$ contained entirely in $R^2-U$; hence $R^2-U$ is open, and thus $U$ is closed. Therefore $\operatorname{Cl}U=U$. Hence, by Proposition 4, $R^2$ is $T_3$. $R^2$ is $T_1$ since each one-point subset of $R^2$ is closed. Alternately, $R,\tau$ is regular; thus $R^2$ with the product topology is regular by Proposition 5.

<figure>
  <img src="/elementary-topology/figures/figure-5.9.svg" alt="A clopen half-open rectangle in the lower-limit product topology." />
  <figcaption>Figure 5.9</figcaption>
</figure>
<figure>
  <img src="/elementary-topology/figures/figure-5.10.svg" alt="A basic rectangle meeting the antidiagonal Y in only its southwest corner." />
  <figcaption>Figure 5.10</figcaption>
</figure>

Let $Y=\{(x,y)\mid y+x=0\}$. Then the subspace topology of $Y$ is discrete, since if $(x,y)\in Y$, there is a basic neighborhood $U$ of $(x,y)$ such that

$$
U\cap Y=\{(x,y)\}
$$

(Fig. 5.10). Since $Y$ is a closed subset of $X$, each subset of $Y$ is a closed subset of $X$ (since a subset of $Y$ is closed in $Y$ if and only if it is closed in $X$, but every subset of $Y$ is closed in $Y$). Let

$$
F=\{(x,y)\mid x+y=0\text{ and }x\text{ is rational}\}
$$

and

$$
F'=\{(x,y)\mid x+y=0\text{ and }x\text{ is irrational}\}.
$$

Since $F$ and $F'$ are subsets of $Y$, they are closed. Also, $F\cap F'=\phi$. If $R^2$ is $T_4$, there must then be open sets $U$ and $V$ such that

$$
F\subset U,\qquad F'\subset V,\qquad\text{and}\qquad U\cap V=\phi.
$$

Although a rigorous argument of the impossibility of such sets will not be given here, it can be made fairly clear why there cannot be such sets. For if $(x,y)\in F$, then there would be a basic neighborhood of $(x,y)$ contained in $U$. There are, however, points of $F'$ “arbitrarily close” to $(x,y)$. Hence some basic neighborhood contained in $V$ of a point in $F'$ would be bound to overlap with the basic neighborhood of $(x,y)$ in $U$. Therefore $U\cap V$ could not be $\phi$.

<span id="printed-page-105"></span>

<!-- Source: PDF page 103, printed page 105. -->

Although we do not have that every subset of a normal space is normal, we do have the weaker statement which follows.

**Proposition 8.** If $Y$ is a closed subset of a normal space $X,\tau$ then the subspace $Y$ is normal.

_Proof._ Since $X$ is $T_1$, $Y$ is $T_1$ because every subspace of a $T_1$-space is $T_1$. Since $Y$ is closed, a subset $F$ of $Y$ is closed in $Y$ if and only if $F$ is closed in $X$. Therefore if $F$ and $F'$ are disjoint closed subsets of $Y$, they are also disjoint closed subsets of $X$. There are thus open sets $U$ and $V$ such that

$$
F\subset U,\qquad F'\subset V,\qquad\text{and}\qquad U\cap V=\phi.
$$

But then

$$
F\subset Y\cap U,\qquad F'\subset Y\cap V,
$$

and $Y\cap U$ and $Y\cap V$ are disjoint subsets of $Y$ which are open in $Y$. Therefore $Y$ is $T_4$; hence $Y$ is normal.

**Proposition 9.** If the product space $\mathop{\Large\times}_I X_i$ of the family of nonempty spaces $\{X_i,\tau_i\}$, $i\in I$, is normal, then $X_i,\tau_i$ is normal for each $i\in I$.

_Proof._ If $\mathop{\Large\times}_I X_i$ is normal, then $\mathop{\Large\times}_I X_i$ is $T_1$. But then each $X_i,\tau_i$ is homeomorphic to a closed subspace of $\mathop{\Large\times}_I X_i$ (Exercise 6). Such a subspace then is normal by Proposition 8. Hence $X_i,\tau_i$ is normal (Exercise 5).

## Exercises

1. Prove that the set $R$ of real numbers with the topology described in Example 11 of Chapter 3 is normal.

2. Prove that a space $X,\tau$ is $T_4$ if and only if given any two disjoint closed subsets $F$ and $F'$ of $X$, there are open sets $U$ and $V$ such that

   $$
   F\subset U,\qquad F'\subset V,\qquad\text{and}\qquad\operatorname{Cl}U\cap\operatorname{Cl}V=\phi.
   $$

3. Suppose $X,\tau$ is a normal space and $F$ is a closed subset of $X$. Let $X/F$ be the identification space formed by identifying all the points of $F$ with one another; more figuratively, $X/F$ is the identification space obtained by squashing $F$ to a point (as in Proposition 6). Prove that $X/F$ is normal.

4. Prove that every subspace of a metric space is normal. Thus if we were to find a normal space which had a subspace which was not normal, we would know such a space was not a metric space.

5. Prove that the property of being normal is preserved by homeomorphisms but not by continuous functions.

6. We saw in Proposition 20, Chapter 4, that if $\mathop{\Large\times}_I X_i$ is the product space of the family of nonempty spaces $\{X_i,\tau_i\}$, $i\in I$, then each $X_i,\tau_i$ is homeomorphic <span id="printed-page-106"></span><!-- Source: PDF page 104, printed page 106; section 5.4 fragment. --> to a subspace of $\mathop{\Large\times}_I X_i$. Prove that if $\mathop{\Large\times}_I X_i$ is $T_1$, each $X_i,\tau_i$ is homeomorphic to a closed subspace of $\mathop{\Large\times}_I X_i$.

7. Are any of the spaces given in Exercise 6, Section 5.3, normal besides that given in (c)?

8. Review the proof of Proposition 5. Discuss why an analogous proof will not hold for normal spaces. That is, try to find out what goes wrong in attempting to apply to normality the techniques which gave us the “hereditary” properties of the other separation axioms.

9. Suppose $X,\tau$ and $Y,\tau'$ are normal and $f$ is a continuous function from a subspace $A$ of $X$ into $Y$. Let $Z=X\cup Y$ have the topology $\tau''$ defined by using $\tau\cup\tau'$ as a basis. Prove that the identification space formed from $Z$ using $f$, that is, by setting $x$ equivalent to $f(x)$, is normal.

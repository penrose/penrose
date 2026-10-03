---
title: Ultranets and Ultrafilters — Elementary Topology
description: The available text of section 6.8 of the supplied second-edition scan.
---

# 6.8 Ultranets and Ultrafilters

::: info Transcription note
Source: printed pages 136–138 and 140–141 (PDF pages 131–135). Printed page 139 is absent; the proof of Proposition 20, the intervening statements and definition, and the opening of the later proof are unavailable. Printed page 140's proof parts remain in their original order, b), c), a). The source's incomplete conditional in Proposition 19 and its indexing and upper-bound wording in Proposition 18 are retained.
:::

<span id="printed-page-136"></span>

<!-- Source: PDF page 131, printed page 136; section 6.8 fragment. -->

There is another concept involving nets which will prove useful in the discussion of compact spaces; this concept is that of an _ultranet_. Actually, the filter analog of an ultranet is more natural, since it is a bit difficult to motivate the notion of an ultranet, except to say that it works. We therefore introduce the concept of an ultrafilter first and then pass to the net analog.

**Definition 6.** An _ultrafilter_ on a set $X$ is a maximal filter on $X$. That is, a filter $\mathfrak{a}$ on a set $X$ is an ultrafilter on $X$ if given any filter $\mathfrak{a}'$ finer than $\mathfrak{a}$ (i.e., $\mathfrak{a}\subset\mathfrak{a}'$), $\mathfrak{a}=\mathfrak{a}'$.

<span id="printed-page-137"></span>

<!-- Source: PDF page 132, printed page 137. -->

**Example 18.** Let $X$ be any set and $x\in X$. Then the family of all subsets of $X$ which contain $x$ forms an ultrafilter $\mathfrak{a}$ on $X$. For if $\mathfrak{a}'$ is any filter finer than $\mathfrak{a}$ and $A'\in\mathfrak{a}'$, then either $x\in A'$ or $x\notin A'$. Now if $x\in A'$, then $A'\in\mathfrak{a}$. If $x\notin A'$, then $x\in X-A'$; hence $X-A'\in\mathfrak{a}\subset\mathfrak{a}'$. But then

$$
A'\cap(X-A')=\phi\in\mathfrak{a}',
$$

a contradiction to the fact that each member of $\mathfrak{a}'$ is nonempty. Therefore $x$ is an element of each member of $\mathfrak{a}'$; hence $\mathfrak{a}'\subset\mathfrak{a}$, and consequently

$$
\mathfrak{a}=\mathfrak{a}'.
$$

**Proposition 17.** A necessary and sufficient condition that a filter $\mathfrak{a}$ on a set $X$ be an ultrafilter is that given any subset $A$ of $X$, either

$$
A\in\mathfrak{a}\qquad\text{or}\qquad X-A\in\mathfrak{a}.
$$

_Proof._ Suppose $\mathfrak{a}$ is a filter on $X$ with the property that either $A$ or $X-A$ is a member of $\mathfrak{a}$ for any $A\subset X$. Suppose $\mathfrak{a}'$ is a filter on $X$ which is finer than $\mathfrak{a}$. To prove that $\mathfrak{a}=\mathfrak{a}'$, it will suffice to prove that each element of $\mathfrak{a}'$ is also an element of $\mathfrak{a}$. Let $A'\in\mathfrak{a}'$. If $A'\in\mathfrak{a}$, we are done. If $A'\notin\mathfrak{a}$, then

$$
X-A'\in\mathfrak{a}\subset\mathfrak{a}'.
$$

But then $A'$ and $X-A'$ are both members of $\mathfrak{a}'$; hence

$$
A'\cap(X-A')=\phi
$$

is a member of $\mathfrak{a}'$, a contradiction.

Suppose now that $\mathfrak{a}$ is an ultrafilter on $X$, but there is a subset $A$ of $X$ such that neither $A$ nor $X-A$ is a member of $\mathfrak{a}$. We will find a filter $\mathfrak{a}'$ on $X$ which is strictly finer than $\mathfrak{a}$. If $A\cap B\ne\phi$ for each $B\in\mathfrak{a}$, then we can take

$$
\mathfrak{D}=\{A\cap B\mid B\in\mathfrak{a}\}
$$

as a filter basis for a filter $\mathfrak{a}'$ which is strictly finer than $\mathfrak{a}$ (since $A\in\mathfrak{a}'$, but $A\notin\mathfrak{a}$).

Suppose, however, that $A\cap B_1=\phi$ for some $B_1\in\mathfrak{a}$. We will show that

$$
(X-A)\cap B\ne\phi
$$

for every $B\in\mathfrak{a}$. Suppose $(X-A)\cap B_2=\phi$ for some $B_2\in\mathfrak{a}$. Then

$$
\begin{aligned}
B_1\cap B_2&=((B_1\cap B_2)\cap A)\cup((B_1\cap B_2)\cap(X-A))\\
&\subset(B_1\cap A)\cup(B_2\cap(X-A))=\phi,
\end{aligned}
$$

<span id="printed-page-138"></span>

<!-- Source: PDF page 133, printed page 138. -->

a contradiction since $B_1\cap B_2$ is a member of $\mathfrak{a}$ and hence must be nonempty. Therefore

$$
\mathfrak{D}=\{(X-A)\cap B\mid B\in\mathfrak{a}\}
$$

is a basis for a filter $\mathfrak{a}'$ on $X$ which is strictly finer than $\mathfrak{a}$ (since $\mathfrak{a}'$ contains $X-A$).

**Proposition 18.** If $X$ is any set, then every filter $\mathfrak{a}$ on $X$ is contained in an ultrafilter.

_Proof._ Consider the family $\mathfrak{A}$ of all filters $\mathfrak{a}'$ on $X$ such that $\mathfrak{a}\subset\mathfrak{a}'$. $\mathfrak{A}$ can be partially ordered by “is finer than.” Suppose $\mathfrak{c}=\{\mathfrak{a}_k'\}$, $k\in K$, is a chain in $\mathfrak{A}$. We now show that

$$
\mathfrak{D}=\{B\mid B\in\mathfrak{a}_k'\text{ for some }k\in K\}
$$

is a basis for a filter $\mathfrak{a}''$ on $X$. First, $\mathfrak{D}$ is certainly a nonempty collection of nonempty sets, since each $\mathfrak{a}_k$ is a nonempty collection of nonempty sets. Suppose $B$ and $B'$ are members of $\mathfrak{D}$. Then $B\in\mathfrak{a}_k'$ and $B'\in\mathfrak{a}_{k'}'$ for some $k$ and $k'$ in $K$. But $\mathfrak{c}$ is a chain, and hence either $\mathfrak{a}_k'\subset\mathfrak{a}_{k'}'$, or $\mathfrak{a}_{k'}'\subset\mathfrak{a}_k'$; assume the latter. Then $B$ and $B'$ are both in $\mathfrak{a}_k'$, and thus $B\cap B'\in\mathfrak{a}_k'$. Hence

$$
B\cap B'\in\mathfrak{D}.
$$

Therefore $\mathfrak{D}$ is a basis for a filter $\mathfrak{a}''$ on $X$. Moreover, $\mathfrak{a}''$ is clearly finer than any member of $\mathfrak{c}$. Hence $\mathfrak{a}''$ is an upper bound in $\mathfrak{c}$ for $\mathfrak{A}$.

Each chain in $\mathfrak{A}$ thus has an upper bound. Applying Zorn's lemma, $\mathfrak{A}$ therefore contains a maximal element $\bar{\mathfrak{a}}$. Then $\bar{\mathfrak{a}}$ is an ultrafilter which contains $\mathfrak{a}$.

**Proposition 19.** Suppose $f$ is any function from a set $X$ onto a set $Y$ and $\mathfrak{a}$ is an ultrafilter on $X$. Let $f(\mathfrak{a})$ be the filter basis as described in Section 6.7, Exercise 5, and let $\mathfrak{a}'$ be the filter on $Y$ that it determines. Then $\mathfrak{a}'$ is an ultrafilter on $Y$.

_Proof._ Suppose $A\subset Y$. In order to show that $\mathfrak{a}'$ is an ultrafilter, we must show that either $A$ or $Y-A$ is a member of $\mathfrak{a}'$ (Proposition 17). Since $\mathfrak{a}$ is an ultrafilter on $X$, either $f^{-1}(A)$ or $f^{-1}(Y-A)=X-f^{-1}(A)$ is a member of $\mathfrak{a}$. If $f^{-1}(A)\in\mathfrak{a}$, then

$$
f(f^{-1}(A))=A\in f(\mathfrak{a})\subset\mathfrak{a}'.
$$

If $X-f^{-1}(A)$, then $Y-A\in\mathfrak{a}'$. Therefore $\mathfrak{a}'$ is an ultrafilter on $Y$.

**Proposition 20.** If $\mathfrak{a}$ is an ultrafilter on a space $X,\tau$ and $y$ is a limit point of $\mathfrak{a}$, then $\mathfrak{a}\longrightarrow y$.

::: warning Missing source page 139
Printed page 139 is absent. Proposition 20's proof, the intervening statements and definition, and the statement belonging to the proof below have not been reconstructed. The references to Propositions 21 and 22 in the supplied exercises remain as printed.
:::

<span id="printed-page-140"></span>

<!-- Source: PDF page 134, printed page 140. The proof statement is unavailable. -->

_Proof_

b) Suppose $A\subset Y$. Then $\{s_i\}$, $i\in I$, is residually in either $f^{-1}(A)$ or $X-f^{-1}(A)$. Therefore $\{f(s_i)\}$, $i\in I$, is residually in either $A$ or $Y-A$, and is hence an ultranet in $Y$.

c) Let $U$ be any neighborhood of $y$. Since $\{s_i\}$, $i\in I$, is an ultranet, it is residually in either $U$ or $X-U$. Since $y$ is a limit point of $\{s_i\}$, $i\in I$, the net could not be residually in $X-U$. Therefore $\{s_i\}$, $i\in I$, is residually in $U$; hence $s_i\longrightarrow y$.

a) Let $\{s_i\}$, $i\in I$, be any net in a set $X$ and let $\mathfrak{a}$ be the filter generated by $\{s_i\}$, $i\in I$. By Proposition 18, $\mathfrak{a}\subset\mathfrak{a}'$, where $\mathfrak{a}'$ is an ultrafilter. We first show that $\{s_i\}$, $i\in I$, is cofinally in $A$ for each $A\in\mathfrak{a}'$. Let $A\in\mathfrak{a}'$. If $\{s_i\}$, $i\in I$, is not cofinally in $A$, then $\{s_i\}$, $i\in I$, is residually in $X-A$. But then

$$
X-A\in\mathfrak{a}\subset\mathfrak{a}'.
$$

Therefore $A$ and $X-A$ are both elements of $\mathfrak{a}'$, an impossibility; hence $\{s_i\}$, $i\in I$, is cofinally in $A$. Let

$$
J=\{(i,A)\mid i\in I,\ A\in\mathfrak{a}',\text{ and }s_i\in A\}.
$$

Since $\mathfrak{a}'$ and $I$ are both directed sets ($\mathfrak{a}'$ is directed by letting $A\leq A'$ if $A'\subset A$), $J$ is directed. Define $k:J\longrightarrow I$ by $k(i,A)=i$. Then $s\circ k$ is easily verified to be a subnet of $\{s_i\}$, $i\in I$. By definition of $s\circ k$, $s\circ k$ is residually in each $A\in\mathfrak{a}'$. But $\mathfrak{a}'$ is an ultrafilter and hence contains any subset of $X$ or its complement. Thus $s\circ k$ is residually in any subset of $X$ or its complement, and is therefore an ultranet in $X$.

## Exercises

1. In Proposition 22, verify in the proof of (a) that $s\circ k$ is a subnet of $\{s_i\}$, $i\in I$.

2. Prove (b) of Proposition 21. Show that the converse of (b) is false.

3. a) Let $\{s_i\}$, $i\in I$, be a net in $X$, and suppose $\mathfrak{a}$ is the filter generated by $\{s_i\}$, $i\in I$. Is $\{s_i\}$, $i\in I$, then a net based on $\mathfrak{a}$? Is it the only net based on $\mathfrak{a}$?

   b) Suppose $\mathfrak{a}$ is a filter on a set $X$ and $\{s_A\}$, $A\in\mathfrak{a}$, is a net based on $\mathfrak{a}$. Is $\mathfrak{a}$ necessarily the filter which $\{s_A\}$, $A\in\mathfrak{a}$, generates?

4. Which of the following sequences in $R$, the set of real numbers, are ultranets? Note that the property of being an ultranet is independent of the topology on $R$.

   a) $s_n=1/n$

   b) $s_n=n$

   c) $s_n=1/n^2$

   d) $s_n=(-1)^n$

<span id="printed-page-141"></span>

<!-- Source: PDF page 135, printed page 141. -->

5. a) Prove that a function $f$ from a space $X,\tau$ to a space $Y,\tau'$ is continuous if and only if given any ultranet $\{s_i\}$, $i\in I$, such that $s_i\longrightarrow y$, $f(s_i)\longrightarrow f(y)$.

   b) Prove that $f:X,\tau\longrightarrow Y,\tau'$ is continuous if and only if given any ultrafilter $\mathfrak{a}$ in $X$ such that $\mathfrak{a}\longrightarrow y$, the filter generated by $f(\mathfrak{a})$ converges to $f(y)$.

6. Prove or disprove: A space $X,\tau$ is $T_2$ if and only if every convergent ultranet in $X$ converges to a unique limit.

7. Let $N$ be the set of positive integers and $\mathfrak{F}$ be the set of subsets of $N$ containing all but finitely many elements of $N$. Prove that $\mathfrak{F}$ is a filter. Prove that any ultrafilter containing $\mathfrak{F}$ is nontrivial, and hence there exists a nontrivial ultrafilter on $N$.

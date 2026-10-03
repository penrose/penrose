---
title: 7.3 Compactness
---

# 7.3 Compactness

<span id="printed-page-152"></span>

<!-- Source: PDF145, printed152. -->

The most important of all covering properties is compactness. As was pointed out earlier in this chapter, compactness was not originally viewed as a covering property, but it is through the use of coverings that compactness can be stated in its most workable form. Compactness is, like Lindelöf, a cardinality condition.

**Definition 4.** A space $X,\tau$ is said to be _compact_ if given any open cover $\{U_i\}$, $i\in I$, of $X$, there is a finite subcover of $\{U_i\}$, $i\in I$.

Suppose $X,\tau$ is any space and $A\subset X$. An _open cover of_ $A$ is a collection $\{U_i\}$, $i\in I$, of open subsets of $X$ whose union includes $A$. Equivalently, $\{U_i\}$, $i\in I$, is an open cover of $A$ if $\{U_i\cap A\}$, $i\in I$, is an open cover of the subspace $A$. $A$ is said to be _compact_ if every open cover of $A$ has a finite subcover. Equivalently, $A$ is compact if the subspace $A$ is compact.

Note that in order for a space to be Lindelöf, any open cover had to have a countable subcover. In order for a space to be compact, any open cover has to have a finite subcover. Certainly then, any compact space is also Lindelöf.

**Example 8.** The open interval $(0,1)$ with the absolute value topology is Lindelöf since it is a subspace of a second countable space $R$. The interval $(0,1)$ is not compact, as we see from Section 7.1, Exercise 4. Another example of a Lindelöf space which is not compact is any countably infinite set with the discrete topology.

An example of a compact space is the space presented in Example 3.

**Proposition 7.** The subspace $[0,1]$ of the space $R$ of real numbers with the absolute value topology is compact.

_Proof._ Let $\{U_i\}$, $i\in I$, be an open cover of $[0,1]$, where each $U_i$ is open in $R$. Let

$$
T=\{x\in[0,1]\mid\text{finitely many of the }U_i\text{ cover }[0,x)\}.
$$

Then $T\ne\phi$ and $1$ is an upper bound for $T$. Therefore $T$ has a least upper bound, say $u$. If $u=1$, we are done (since finitely many of the $U_i$ cover $[0,1)$, and hence at most one more of the $U_i$ will be needed to get a finite cover of $[0,1]$). Suppose then $0\le u<1$. Then either $u\in T$ or $u\notin T$.

_Case 1._ $u\in T$. Then finitely many of the $U_i$, say $U_{i_1},\ldots,U_{i_n}$ cover $[0,u)$. There is, however, $U_{i'}$ such that $u\in U_{i'}$; therefore

$$
\{U_{i'},U_{i_1},\ldots,U_{i_n}\}
$$

<span id="printed-page-153"></span>

<!-- Source: PDF146, printed153. -->

<figure class="topology-chapter-figure">
  <img src="/elementary-topology/figures/figure-7.2.svg" alt="A finite interval cover extends past its proposed upper bound u." />
  <figcaption>Figure 7.2</figcaption>
</figure>

is an open cover of $[0,u]$. It is then clear (Fig. 7.2) that $u$ could not be an upper bound for $T$.

_Case 2._ $u\notin T$. Then there is $U_j$ such that $u\in U_j$ and finitely many of the $U_i$ do not cover $[0,u)-U_j$. Therefore $u$ is not the least upper bound for $T$.

Both cases have led to contradictions; hence it could not be that $0\le u<1$. Therefore $u=1$, and hence $[0,1]$ is compact.

We now derive some important criteria for compactness.

**Proposition 8.** Let $X,\tau$ be any topological space. Then $X$ is compact if and only if given any family $\{F_i\}$, $i\in I$, of closed subsets of $X$ such that the intersection of any finite number of the $F_i$ is nonempty, $\bigcap_I F_i\ne\phi$.

_Proof._ Suppose $X$ is compact and let $\{F_i\}$, $i\in I$, be any family of closed subsets of $X$ such that $\bigcap_I F_i=\phi$. Set $U_i=X-F_i$ for each $i\in I$. Then

$$
X-\bigcap_I F_i=X-\phi=X=\bigcup_I(X-F_i)=\bigcup_I U_i.
$$

Each $U_i$ is the complement of a closed set and hence is open. Therefore $\{U_i\}$, $i\in I$, is an open cover of $X$. But $X$ is compact; hence there are finitely many of the $U_i$, say $U_{i_1},\ldots,U_{i_n}$, which cover $X$. Then

$$
F_{i_1}\cap\cdots\cap F_{i_n}=\phi.
$$

We have proved that if $X$ is a compact space, then given any family $\{F_i\}$, $i\in I$, of closed subsets of $X$ whose intersection is empty, the intersection of some finite family of $F_i$ is empty.

Suppose $X$ has the property that if the intersection of any family $\{F_i\}$, $i\in I$, of closed subsets of $X$ is empty, the intersection of finitely many of the $F_i$ is empty. Suppose $\{U_i\}$, $i\in I$, is any open cover of $X$. Then $X=\bigcup_I U_i$. Therefore setting $F_i=X-U_i$, $\{F_i\}$, $i\in I$, is a family of closed subsets of $X$ whose intersection is empty. Hence we can find finitely many of the $F_i$, say $F_{i_1},\ldots,F_{i_n}$, such that

$$
F_{i_1}\cap\cdots\cap F_{i_n}=\phi.
$$

Then $\{U_{i_1},\ldots,U_{i_n}\}$ is a finite subcover of $\{U_i\}$, $i\in I$. Therefore $X$ is compact.

**Proposition 9.** A space $X,\tau$ is compact if and only if every net in $X$ has a limit point.

<span id="printed-page-154"></span>

<!-- Source: PDF147, printed154. -->

_Proof._ Suppose $X$ is compact and let $\{x_i\}$, $i\in I$, be any net in $X$. Define $B_j=\{x_i\mid j\le i\}$. Then $\{\operatorname{Cl}B_j\}$, $j\in J$, has the property that the intersection of any finite family of the $\operatorname{Cl}B_j$ is nonempty. Since $X$ is compact, by Proposition 8, $\bigcap_I\operatorname{Cl}B_j\ne\phi$. Choose $y$ in this intersection. We now show that $y$ is a limit point of $\{x_i\}$, $i\in I$. Since $y\in\operatorname{Cl}B_j$ for any $j\in I$, any neighborhood $U$ of $y$ therefore contains at least one point of $B_j$. Suppose $U$ is a neighborhood of $y$ and $j$ and $j'$ are elements of $I$. Since $I$ is directed, there is $j''\in I$ such that $j\le j''$ and $j'\le j''$. But $y\in\operatorname{Cl}B_{j''}$, and hence there is $\bar j$ such that

$$
x_{\bar j}\in U\cap B_{j''}.
$$

Then $j\le\bar j$, $j'\le\bar j$, and $x_{\bar j}\in U$. Therefore $\{x_i\}$, $i\in I$, is cofinally in $U$; hence $y$ is a limit point of $\{x_i\}$, $i\in I$.

Suppose, on the other hand, that $X$ has the property that every net in $X$ has a limit point. Let $\{F_i\}$, $i\in I$, be any family of closed subsets of $X$ such that the intersection of finitely many of the $F_i$ is always nonempty. Let $J$ be the set of finite intersections of the $F_i$. Then $J$ is partially ordered by $\le$, where $A\le B$ if $B\subset A$; moreover, $J$ is then a directed set. Since each member of $J$ is nonempty, we can define a selection function $s$ from $J$ into $X$ such that $s(A)\in A$ for each $A\in J$. Therefore $\{s_A\}$, $A\in J$, is a net in $X$, and hence has a limit point $y$. Consider any of the $F_i$. If $A\in J$ and $F_i\le A$, then $A\subset F_i$. Thus for each of the $F_i$, the net $\{s_A\}$, $A\in J$, is residually in $F_i$. Since $y$ is a limit point of $\{s_A\}$, $A\in J$, some subnet of $\{s_A\}$, $A\in J$, converges to $y$ (Proposition 9, Chapter 6). But since $\{s_A\}$, $A\in J$, is residually in $F_i$, such a subnet would be residually in $F_i$ for each $i$ (Proposition 3, Chapter 6). Then by Proposition 13, Chapter 6, $y\in F_i$ for each $i$; hence $y\in\bigcap_I F_i$. Therefore $\bigcap_I F_i\ne\phi$. By Proposition 8, then $X$ is compact.

::: info Source notation
The first proof paragraph prints $j\in J$ for the family of closures and subsequently uses $I$; both readings are retained.
:::

**Corollary.** A space $X,\tau$ is compact if and only if every ultranet in $X$ converges.

_Proof._ If $X$ is compact and $\{s_i\}$, $i\in I$, is an ultranet in $X$, then $\{s_i\}$, $i\in I$, has a limit point. But an ultranet converges to any of its limit points (Proposition 22, Chapter 6). Conversely, if every ultranet in $X$ converges and $\{s_i\}$, $i\in I$, is any net in $X$, then some ultranet is a subnet of $\{s_i\}$, $i\in I$ (Proposition 22, Chapter 6). Therefore $\{s_i\}$, $i\in I$, has a subnet which converges to some point $y$; hence $y$ is a limit point of $\{s_i\}$, $i\in I$ (Proposition 9, Chapter 6). Then $X$ is compact by Proposition 9.

**Example 9.** We give another proof now that $[0,1]$ with the absolute value topology is compact. Since $[0,1]$ is second countable or metric, we will have shown $[0,1]$ is compact if we show that every sequence in $[0,1]$ has a limit point. Suppose $\{s_n\}$, $n\in N$, is a sequence in $[0,1]$.

<span id="printed-page-155"></span>

<!-- Source: PDF148, printed155. -->

_Case 1._ $\{s_n\}$, $n\in N$, is monotonically increasing, that is,

$$
s_1\le s_2\le\cdots\le s_n\le\cdots.
$$

Then $\{s_n\mid n\in N\}$ has a least upper bound $u$, $0\le u\le1$. If $U$ is any neighborhood of $u$, it is readily shown that $\{s_n\}$, $n\in N$, is residually in $U$, and hence $s_n\to u$. Therefore $u$ is a limit point of $\{s_n\}$, $n\in N$.

_Case 2._ $\{s_n\}$, $n\in N$, is monotonically decreasing, that is,

$$
s_1\ge s_2\ge\cdots\ge s_n\ge\cdots.
$$

Then $\{s_n\mid n\in N\}$ has a greatest lower bound $v$, $0\le v\le1$; moreover $s_n\to v$. Therefore $v$ is a limit point of $\{s_n\}$, $n\in N$.

_Case 3._ If $\{s_n\}$, $n\in N$, is either monotonically increasing or decreasing from some point on, that is, for all but finitely many elements, then the exceptional elements can be discarded without penalty, and Case 1 or 2 applied.

_Case 4._ $\{s_n\}$, $n\in N$, is neither monotonically increasing nor monotonically decreasing from some point on. Then $\{s_n\}$, $n\in N$, is “cofinally” increasing (the quotation marks here indicate that we are applying a property informally to the sequence as a whole, rather than to individual members); hence there is a monotonically increasing subsequence of $\{s_n\}$, $n\in N$. By Case 1, this subsequence converges to a point $u$ of $[0,1]$. But then $u$ is a limit point of $\{s_n\}$, $n\in N$.

Every sequence in $[0,1]$ has a limit point, and therefore $[0,1]$ is compact.

## Exercises

1. Prove that the set $J$ in Proposition 9 is a directed set.
2. Prove that the sequences in Example 9 converge as claimed. Formalize the argument in Case 4.
3. In Example 9, it is asserted that because $[0,1]$ is second countable, we need only consider sequences. Prove: A second countable space $X,\tau$ is compact if and only if every sequence in $X$ has a limit point.
4. Prove that a space $X,\tau$ is compact if and only if every filter on $X$ has a limit point. Prove that $X$ is compact if and only if every ultrafilter on $X$ converges.
5. Let $X,\tau$ be a space and let $\mathfrak{B}$ be a basis for $\tau$. Prove that $X$ is Compact if and only if every cover of $X$ by members of $\mathfrak{B}$ has a finite subcover.
6. Decide which of the following spaces are compact.

   a) the plane $R^2$ with the topology which has for a subbasis

   $$
   \mathfrak{S}=\{U\mid U=R^2-L,\text{ where }L\text{ is any straight line}\}
   $$

   <span id="printed-page-156"></span>
   <!-- Source: PDF149, printed156, Section7.3 fragment. -->

   b) the plane $R^2$, where an open set is any set of the form $R^2-C$, where $C$ contains at most countably many points of $R^2$, and $\phi$ is open

   c) the subspace of rational numbers in the usual space of real numbers

   d) the space in Example 1, Chapter 6

7. In Section 6.5, Exercise 6, the notion of a bounded sequence in $R$, the usual space of real numbers, was introduced. Let $\{s_n\}$, $n\in N$, be a bounded sequence in $R$, and let $A$ be the set of limit points of $\{s_n\}$, $n\in N$. Prove that $A\cup\{s_n\mid n\in N\}$ is compact.
8. Prove that the union of finitely many compact subsets of any space is compact. Is the intersection of two compact subsets necessarily compact?
9. Prove or disprove: Let $X$ be an infinite space with the property that the only compact subspaces of $X$ are finite subspaces. Then $X$ has the discrete topology.

[Chapter 7 contents](./index.md) · [Next: 7.4 Derived spaces, separation axioms, and compactness](./derived-spaces-and-compactness.md)

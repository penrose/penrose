---
title: Components. Local Connectedness — Elementary Topology
---

# 9.4 Components. Local Connectedness

<span id="printed-page-196"></span>

<!-- Source: PDF184, printed196, Section9.4 fragment. -->

A disconnected space may, of course, have connected subspaces. The structures of the maximal connected subspaces of any space are indispensable in any description of the entire structure of the space. Un<span id="printed-page-197"></span><!-- Source: PDF185, printed197. -->fortunately, even knowing completely the structure of every maximal connected subset of a space does not determine the structure of the space as a whole, as we see from the following.

**Example 11.** Let $Q$ be the space of rational numbers with the absolute value topology. Then $Q$ is totally disconnected (Section 9.1, Exercise 4); hence the maximal connected subsets of $Q$ are the one-point subsets. If $Q$ were to have the discrete topology, then it would still be true that the maximal connected subsets of $Q$ are the one-point subsets. Note however that relative to the metric topology, each one-point subspace of $Q$ is closed in $Q$, but not open; but that with respect to the discrete topology, each one-point subset of $Q$ is both open and closed. But clearly we cannot tell which topology $Q$ has just by knowing that each maximal connected subspace of $Q$ is a subspace of exactly one point.

**Definition 3.** Let $X,\tau$ be any space. A maximal connected subspace of $X$ is said to be a _component_ of $X$.

Thus the components of $Q$ as in Example 11 are the one-point subspaces of $Q$; this is true with respect to either the discrete or the absolute-value topology on $Q$.

**Example 12.** In Example 8, $S$ and $T$ are the components of the subspace $S\cup T$ of $R^2$. In Example 4, $H^-$ and $H^+$ are the components of $R-\{x\}$.

It has not yet been shown that each element of a space $X,\tau$ is actually contained in a component of $X$; that is, although it is clear that each $x\in X$ is contained in at least one connected subspace of $X$, i.e., $\{x\}$, we must show that there is a maximal connected subspace of $X$ which contains $x$. This is easily done, however. For let $\{A_i\}$, $i\in I$, be the family of all connected subspaces which contain $x$. Then $\bigcup_I A_i$ is a connected subspace which contains $x$ (Proposition 5), and $\bigcup_I A_i$ is clearly a maximal connected subspace of $X$ which contains $x$.

We saw in Example 11 that a component may or may not be open. The following shows, though, that a component is always closed.

**Proposition 13.** A component $A$ of a space $X,\tau$ is closed.

_Proof._ By Proposition 11, $\operatorname{Cl}A$ is also connected. But $A\subset\operatorname{Cl}A$ and $A$ is a maximal connected subspace of $X$; hence $A=\operatorname{Cl}A$. Therefore $A$ is closed.

**Proposition 14.** Let $\{A_i\}$, $i\in I$, be the set of components of a space $X,\tau$. (We see from Example 11 that $\{A_i\}$, $i\in I$, need not be a finite set.) Then $X=\bigcup_I A_i$, and if $i\ne j$, then $A_i\cap A_j=\phi$.

_Proof._ Since each $x\in X$ is in some component of $X$ (at least one connected subspace of $X$ contains $x$, hence a maximal connected subspace of $X$ contains $x$), $\bigcup_I A_i=X$.

<span id="printed-page-198"></span>

<!-- Source: PDF186, printed198. -->

If $i\ne j$, but $A_i\cap A_j\ne\phi$, then $A_i\cup A_j$ is a connected subspace of $X$ which contains both $A_i$ and $A_j$, a contradiction to the maximality of both $A_i$ and $A_j$.

Just as we could in a sense localize the notion of compactness, we can also localize connectedness. As we shall see, spaces which are _locally connected_ have particularly nice components.

**Definition 4.** A space $X,\tau$ is said to be _locally connected_ if there is an open neighborhood system for $\tau$ such that for each $x\in X$, $\mathfrak{N}_x$ consists of connected subspaces. Equivalently, $X$ is locally connected if given any $x\in X$ and any neighborhood $U$ of $x$, there is a connected neighborhood $V$ of $x$ such that $V\subset U$ (Exercise 1).

**Example 13.** If $X$ is any space with the discrete topology, then for each $x\in X$, if we set $\mathfrak{N}_x=\{\{x\}\}$, we obtain an open neighborhood system for the discrete topology. Each subspace $\{x\}$ is, of course, connected. Thus, even though $X$ is totally disconnected, $X$ is still locally connected.

**Example 14.** Euclidean $n$-space $R^n$ has a basis for its topology which consists of convex, and therefore connected, subspaces (cf. the remarks preceding Proposition 8). But if a space has a basis which consists of connected subspaces, it is certainly locally connected (Proposition 6, Chapter 3).

Euclidean $n$-space is thus both connected and locally connected. We now give an example of a space which is connected, but not locally connected.

**Example 15.** The space

$$
Y=\{(x,y)\mid y=\sin(1/x),\ 0<x\}\cup\{(0,0)\}
$$

is connected as we saw in Example 9. But it is not locally connected. As a matter of fact, if $0<p<1$, then $N((0,0),p)$ has an infinite number of components (Fig. 9.10). The space $Y$, however, is the image of a locally connected space under a continuous function; hence we see that local connectedness is not generally preserved by a continuous function. Specifically, let

$$
A=\{(x,y)\mid y=\sin(1/x),\ 0<x\}
$$

and

$$
B=\{(-1,0)\}.
$$

::: info Figure 9.10 — unlocated in the available scan
The reference above is retained. No captioned Figure 9.10 has been located in the available supplied pages. [View its reference on printed page 198](../reader?page=198).
:::

::: warning Missing printed page 199
Printed page 199 is absent. The continuation of Example 15 and any intervening examples, proposition statements, and proof openings are unavailable. Printed page 200 begins within the following proof; its missing statement is not reconstructed.
:::

<span id="printed-page-200"></span>

<!-- Source: PDF187, printed200. -->

$x\in C^\circ$, a contradiction, since $C^\circ\cap\operatorname{Fr}C=\phi$. Therefore $x\notin Y^\circ$, and hence $x\in\operatorname{Fr}Y$; thus $\operatorname{Fr}C\subset\operatorname{Fr}Y$.

Statement (c) implies statement (b). Suppose $C$ is a component of $U$, an open subspace of $X$. Then

$$
C\cap\operatorname{Fr}C\subset U\cap\operatorname{Fr}U;
$$

hence $C\cap\operatorname{Fr}C=\phi$ (since $U=U^\circ$ and $U^\circ\cap\operatorname{Fr}U=\phi$). Therefore $C\subset C^\circ$. But $C^\circ\subset C$, and hence $C=C^\circ$. Therefore $C$ is open.

Statement (b) implies statement (a). Suppose $U$ is any neighborhood of $x\in X$. Let $V$ be the component of $U$ which contains $x$. Then $V$ is open; hence $V$ is a connected neighborhood of $x$ which is a subset of $U$. Therefore $X$ is locally connected.

Note that in a locally connected space $X,\tau$, the components of $X$ are both open and closed. We immediately have the following corollary.

**Corollary.** A compact, locally connected space $X,\tau$ has at most finitely many components.

_Proof._ Let $\{A_i\}$, $i\in I$, be the family of components of $X$. Then $\{A_i\}$, $i\in I$, is an open cover of $X$, and hence finitely many of the $A_i$, say $A_{i_1},\ldots,A_{i_n}$ cover $X$. But if $i\ne j$, $A_i\cap A_j=\phi$; therefore no $A_j$ can be omitted from $\{A_i\}$, $i\in I$, such that the remaining components still form a cover of $X$. The components $A_{i_1},\ldots,A_{i_n}$ then must be all of the components of $X$.

We have seen that local connectedness is not preserved by continuous functions. Like local compactness, local connectedness is preserved by continuous open mappings.

**Proposition 16.** If $f$ is a continuous open function from a locally connected space $X,\tau$ onto a space $Y,\tau'$, then $Y$ is locally connected.

_Proof._ Suppose $y\in Y$ and $U$ is any neighborhood of $y$. Since $f$ is onto, there is $x\in X$ such that $f(x)=y$. Then $f^{-1}(U)$ is a neighborhood of $x$. Since $X$ is locally connected, there is a connected neighborhood $V$ of $x$. Since $f$ is both continuous and open, $f(V)$ is a connected neighborhood of $y$ with $f(V)\subset U$. Therefore $Y$ is locally connected.

Proposition 17 illustrates another property of local connectedness which is similar to the corresponding property of local compactness (Chapter 8, Proposition 10).

**Proposition 17.** Let $\{X_i,\tau_i\}$, $i\in I$, be a countable family of nonempty topological spaces. Then the product space $\mathop{\Large\times}_I X_i$ is locally connected if and only if each $X_i$ is locally connected and all but finitely many of the $X_i$ are connected.

<span id="printed-page-201"></span>

<!-- Source: PDF188, printed201. -->

_Proof._ Suppose $\mathop{\Large\times}_I X_i$ is locally connected. Since the projection mapping $p_i$ from $\mathop{\Large\times}_I X_i$ onto $X_i$ is open and continuous, each $X_i$ is locally connected. Let $U$ be any connected neighborhood of any point $y$ of $\mathop{\Large\times}_I X_i$. Then $U$ contains a basic neighborhood of $y$ of the form $\mathop{\Large\times}_I V_i$, where $V_i$ is open in $X_i$, and $V_i=X_i$ for all but at most finitely many $i$. Then $p_i(U)=X_i$ for all but at most finitely many $i$. Since $p_i$ is continuous and connectedness is preserved by continuous functions, $X_i$ is connected for all but at most finitely many $i$.

Suppose that each $X_i$ is connected and all but finitely many of the $X_i$ are connected. Suppose $y\in\mathop{\Large\times}_I X_i$, and $U$ is a neighborhood of $y$. Then there is a basic neighborhood $\mathop{\Large\times}_I V_i$ of $y$ which is contained in $U$, where $V_i$ is open in $X_i$ and $V_i=X_i$ for all but at most finitely many $i$, say $i_1,\ldots,i_n$. For each $i_k$, $k=1,\ldots,n$, there is a connected neighborhood of $y_{i_k}$ (the $i_k$th coordinate of $y$) in $X_{i_k}$, call it $W_{i_k}$, such that $W_{i_k}\subset V_{i_k}$. There are at most finitely many more $i$, say $i_{n+1},\ldots,i_m$, such that

$$
X_{i_{n+1}},\ldots,X_{i_m}
$$

are locally connected, but not connected. For each of these $i_k$,

$$
k=n+1,\ldots,k=m,
$$

there is a connected neighborhood $W_{i_k}$ of $y_{i_k}$ with $W_{i_k}\subset V_{i_k}$. For each

$$
i\notin\{i_1,\ldots,i_n,i_{n+1},\ldots,i_m\},
$$

set $W_i=X_i$. Set $W=\mathop{\Large\times}_I W_i$. Then $W$ is a neighborhood of $y$ and $W\subset\mathop{\Large\times}_I V_i\subset U$. But $W$ is the product of connected sets and is therefore connected (Proposition 12). Therefore $\mathop{\Large\times}_I X_i$ is locally connected.

::: info Source wording
Example 12 refers to $R-\{x\}$ following Example 4’s use of $y$. Proposition 16’s proof does not explicitly state a containment condition when selecting $V$. Proposition 17’s second proof direction begins “each $X_i$ is connected,” followed by a further connectedness assumption; these source readings are retained.
:::

## Exercises

1. Prove that the two definitions of local connectedness given in Definition 4 are equivalent.
2. Prove that every open subspace of a locally connected space is locally connected.
3. Prove that the components of the space $Q$ of rational numbers with the absolute value topology are the one-point subspaces. Prove that $Q$ is not locally connected.
4. Let $X,\tau$ be any space. Define an equivalence relation $E$ on $X$ by letting $xEy$ if $x$ and $y$ are contained in some (the same) connected subset of $X$. Show that the $E$-equivalence classes are the components of $X$.
5. Decide which of the following spaces are locally connected. Also describe the components of each space. Both $R$ and $R^2$ are assumed to have the usual metric topologies.

   a) $R^2-C$, where $C$ is a compact subset of $R^2$

   <span id="printed-page-202"></span>
   <!-- Source: PDF189, printed202, Section9.4 fragment. -->

   b) $\{(x,y)\mid y=x/n,\ n=1,2,3,\ldots\}\subset R^2$

   c) $\{(x,y)\mid y=x/n,\ n=1,2,\ldots;\text{ or }y=0\}\subset R^2$

   d) $\{(x,y)\mid y=x/n,\ n=1,2,\ldots,\text{ and }x\ne0\}\subset R^2$

   e) $\{x\mid x=1/n,\ n=1,2,3,\ldots\}\cup\{0\}\subset R$

   f) the space $N$ in Example 3 of Chapter 7

6. A subset $A$ of a space $X,\tau$ is said to be a _path component_ of $X$ if $A$ is a maximal path-connected subset of $X$ (see Definition 2).

   a) What is the relation between the path components of a space and the components of the space?

   b) What are the path components of the spaces described in Examples 7 and 10?

   c) Prove that for open subspaces of $R^n$, a path component is also a component.

   d) What would be meant by saying that a space is locally path connected? Prove that each path component of a locally path-connected space is open.

7. Prove that if a space has only finitely many components, each component is open. Thus it is not sufficient for a space to have only open components in order for the space to be locally connected.
8. If $X,\tau$ is locally connected, is the one-point compactification of $X$ necessarily locally connected? Explore conditions under which the one-point compactification of a locally connected space will be locally connected.

[Chapter 9 contents](./index.md) · [Next: 9.5 Connectedness and Compact $T_2$-Spaces](./connectedness-and-compact-t2-spaces.md)

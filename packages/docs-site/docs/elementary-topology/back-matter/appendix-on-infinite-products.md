---
title: Appendix on Infinite Products — Elementary Topology
---

# Appendix on Infinite Products

::: info Transcription note
All available appendix text on printed pages 261–264 (PDF pages 241–244) is transcribed. Source notation, proofs, examples, references, and exercises are retained. Notes in these boxes are editorial; exercise instructions belong to the source.
:::

<span id="printed-page-261"></span>

<!-- Source: PDF241, printed261. -->

Let $\{X_i,\tau_i\}$, $i\in I$, be any family of topological spaces indexed by some set $I$. If $I$ is countable, then we already have a definition of the product space of this family. But $I$ need not always be countable; thus if we are to consider product spaces in their full generality, we need to have a product of uncountably many spaces also.

Let $R$ be the set of real numbers. Then, as we know, $R\times R=R^2$ is the set of all ordered pairs $(x,y)$ where $x$ and $y$ are elements of $R$. To each ordered pair $(x,y)$, we can associate a function $c$ from the set $\{1,2\}$ into $R$ defined by $c(1)=x$ and $c(2)=y$. For each $(x,y)\in R$, there is a distinct function from $\{1,2\}$ into $R$, and for each function $c:\{1,2\}\to R$, there is a unique point $(c(1),c(2))$ of $R^2$.

If $X_1$ and $X_2$ are any two sets, then

$$
X_1\times X_2=\{(x_1,x_2)\mid x_1\in X_1\text{ and }x_2\in X_2\}.
$$

But we are able to show that $X_1\times X_2$ can be associated in a natural way with the set of all functions $c$ from $\{1,2\}$ into $X_1\cup X_2$ which have the property that $c(1)\in X_1$ and $c(2)\in X_2$. Note that $\{1,2\}$ is the index set for the family of sets $\{X_1,X_2\}$ of which we are taking the product. We therefore make the following definition.

**Definition 1.** Let $\{X_i\}$, $i\in I$, be any family of sets. Define the _product_ of the family $\{X_i\}$, $i\in I$, to be the collection of all functions $c$ from $I$ into $\bigcup_I X_i$ such that

$$
c(i)\in X_i\quad\text{for each }i\in I.
$$

The product of $\{X_i\}$, $i\in I$, is denoted by $\mathop{\Large\times}_I X_i$. If $c\in\mathop{\Large\times}_I X_i$, then $c(i)$, usually denoted by $c_i$, is called the _$i$th coordinate_ of $c$. $X_i$ is called the _$i$th component_ of the set $\mathop{\Large\times}_I X_i$.

Note that this definition of the product does not depend on the cardinality of $I$. It should not cause the reader much work to show that where $I$ is countable between the old definition of $\mathop{\Large\times}_I X_i$ and the new, there is a natural equivalence. We now use the considerations put forth in Chapter 4 concerning what properties the product topology should have to define a topology on $\mathop{\Large\times}_I X_i$ if each $X_i$ is also a topological space.

<span id="printed-page-262"></span>

<!-- Source: PDF242, printed262. -->

**Definition 2.** Let $\{X_i,\tau_i\}$, $i\in I$, be a family of spaces, and let $\mathop{\Large\times}_I X_i$ be the product set of the family of $X_i$ as defined in Definition 1. Let

$$
\begin{aligned}
\mathfrak S=\{\mathop{\Large\times}_I V_i\mid{}&V_i=X_i\text{ for all but at most one }i\in I\\
&\text{and each }V_i\text{ is an open subset of }X_i\}.
\end{aligned}
$$

Then $\mathfrak S$ is a subbasis for a topology $\tau$ on $\mathop{\Large\times}_I X_i$ called the _product topology_. The space $\mathop{\Large\times}_I X_i,\tau$ is called the _product space_ of $\{X_i,\tau_i\}$, $i\in I$. (Compare this to Definition 7, Chapter 4.)

There is a natural function

$$
p_i:\mathop{\Large\times}_I X_i\to X_i
$$

defined by $p_i(c)=c_i$ for each $i\in I$. We call $p_i$ the _projection into the $i$th component_.

The reader should promptly prove that the product topology is the coarsest topology which makes each projection $p_i$ continuous.

The propositions and proofs in this text which deal with product spaces have purposely been designed so that it would be easy to adapt them to the more general notion of a product space (provided that they are valid when generalized). We now give two examples of propositions and their proofs which generalize and one example of a proposition which is not true when stated for the product of uncountably many spaces.

**Proposition 1.** Let $Y=\mathop{\Large\times}_I X_i$ be the product space of the family of nonempty spaces $\{X_i,\tau_i\}$, $i\in I$. Then $Y$ is $T_2$ if and only if each $X_i$ is $T_2$. (See Proposition 3, Chapter 5.)

_Proof._ Suppose each $X_i$ is $T_2$, and let $x$ and $y$ be distinct points of $Y$. We will use $x_i$ and $y_i$ to denote the $i$th coordinate of $x$ and $y$, respectively. Since $x\ne y$, $x_i\ne y_i$ for at least one $i\in I$, say for $i'$. Therefore there are open sets $U_{i'}$ and $V_{i'}$ in $X_{i'}$ such that

$$
x_{i'}\in U_{i'},\quad y_{i'}\in V_{i'},\quad\text{and}\quad U_{i'}\cap V_{i'}=\phi.
$$

Set $U=\mathop{\Large\times}_I H_i$, where $H_i=X_i$, $i\ne i'$, and $H_{i'}=U_{i'}$; and set $V=\mathop{\Large\times}_I G_i$, where $G_i=X_i$, $i\ne i'$, and $G_{i'}=V_{i'}$. Then $U$ and $V$ are neighborhoods of $x$ and $y$, respectively, and since any point of $U$ differs from any point of $V$ at least in the $i$th coordinate, $U\cap V=\phi$. Therefore $Y$ is $T_2$.

By the generalization of Proposition 20, Chapter 4, each $X_i,\tau_i$ is homeomorphic to a subspace of $Y$. Thus if $Y$ is $T_2$, then each $X_i$ is also $T_2$.

**Proposition 2.** Suppose $f$ is a function from a space $X,\tau$ into the product space $\mathop{\Large\times}_I X_i,\tau'$. Define $f_i:X,\tau\to X_i,\tau_i$ by

$$
f_i(x)=p_i\circ f(x)\quad\text{for each }x\in X,
$$

where $p_i$ is the projection into the $i$th component. Then $f$ is continuous if and only if $f_i$ is continuous for each $i\in I$. (Cf. Proposition 21, Chapter 4.)

<span id="printed-page-263"></span>

<!-- Source: PDF243, printed263. -->

_Proof._ If $f$ is continuous, then $f_i=f\circ p_i$ is the composition of two continuous functions and therefore is continuous.

Suppose now that $f_i$ is continuous for each $i\in I$. We first note that the $i$th coordinate of $f(x)$ is $f_i(x)$ for each $x\in X$. A basis $\mathfrak B$ for $\tau'$ consists of all sets of the form $\mathop{\Large\times}_I V_i$, where $V_i$ is open in $X_i$ for each $i\in I$ and $V_i=X_i$ for all but finitely many $i$. Suppose $\mathop{\Large\times}_I V_i$ is any member of $\mathfrak B$, and $V_i=X_i$ for each $i\in I$ except $i_1,\ldots,i_m$. Now $f^{-1}(\mathop{\Large\times}_I V_i)$ is the set of all points $x$ of $X$ such that

$$
f(x)\in\mathop{\Large\times}_I V_i.
$$

But this is easily seen to be $\bigcap_I f_i^{-1}(V_i)$. For every $i\in I$, except $i_1,\ldots,i_m$, $f_i^{-1}(V_i)=X$ (because $V_i=X_i$). Since $f_i$ is continuous for each $i\in I$, $f_{i_j}^{-1}(V_{i_j})$ is open in $X$, $j=1,\ldots,m$. Therefore

$$
f^{-1}(\mathop{\Large\times}_I V_i)=f_{i_1}^{-1}(V_{i_1})\cap\cdots\cap f_{i_m}^{-1}(V_{i_m}),
$$

which is open in $X$ since it is the intersection of finitely many open sets. Hence, by Proposition 7, Chapter 4, $f$ is continuous.

::: info Source composition
The proof's first sentence prints $f_i=f\circ p_i$, retained here, whereas the preceding definition uses $p_i\circ f$.
:::

**Proposition 3.** Suppose $\{s_i\}$, $i\in I$, is a net in the product space $\mathop{\Large\times}_J X_j$. Then $s_i\to y$ if and only if $s_i^j\to y_j$, where $y_j$ is the $j$th coordinate of $y$ and $\{s_i^j\}$, $i\in I$, is defined to be $\{p_j(s_i)\}$, $i\in I$.

The proof of Proposition 12 of Chapter 6 may be used _verbatim_.

**Proposition 4 (Tychonoff).** Let $\mathop{\Large\times}_I X_i$ be the product space of the nonempty family of nonempty spaces $\{X_i,\tau_i\}$, $i\in I$. Then $\mathop{\Large\times}_I X_i$ is compact if and only if each component space is compact.

The proof of Proposition 13 of Chapter 7 may be used _verbatim_; alternatively, one may use the proof of Proposition 16.

The propositions we would expect not to generalize are those which deal with the cardinality of a basis for the product topology. For example, Proposition 4(d), Chapter 7, does not generalize, as the following example shows.

**Example 1.** Let $I$ be an uncountable set, and let $X_i=\{0,1\}$ for each $i\in I$. Give each $X_i$ the discrete topology. Then certainly each $X_i$ is second countable. But the product space $\mathop{\Large\times}_I X_i$ is not second countable. This is true since the subbasis $\mathfrak S$, as described in Definition 2 for the product topology on $\mathop{\Large\times}_I X_i$, contains uncountably many distinct members of the form $\mathop{\Large\times}_I V_i$, where $V_i=X_i$, except for precisely one $i\in I$. The family of such members of $\mathfrak S$ can be shown to be a minimal subbasis for the product topology; thus the product topology could not be second countable. We could also prove that the product space $\mathop{\Large\times}_I X_i$ is not second countable as follows: If $\mathop{\Large\times}_I X_i$ is second countable, then $\mathop{\Large\times}_I X_i$ is Lindelöf, and hence every open cover of $\mathop{\Large\times}_I X_i$ has a countable subcover. But the collection of $\mathop{\Large\times}_I V_i$, where $V_i=X_i$, except for precisely one $i$, is an open cover of $\mathop{\Large\times}_I X_i$ which has no countable subcover.

The generalization of the notion of a product space also expands our horizons as to the possible uses of product spaces.

<span id="printed-page-264"></span>

<!-- Source: PDF244, printed264. -->

**Example 2.** Given spaces $X,\tau$ and $Y,\tau'$, then $Y^X$ was used to denote the family of all continuous functions from $X$ into $Y$ (cf. Proposition 1, Chapter 11). Actually, $Y^X$ more appropriately stands for the family of _all_ functions from $X$ into $Y$, that is, the product set $Y^X$. Since $Y$ is a space, $Y^X$ can be given a topology as a product space; the family of continuous functions from $X$ into $Y$ is a subspace of this space. This in turn leads to the possibility of putting topologies on the set of homotopy equivalence classes of functions from $X$ into $Y$ (using the identification topology) and of even giving a topology to fundamental groups. In point of fact, more important topologies than the product topology are used on $Y^X$, but having a generalized notion of product has awakened us to this possibility.

## Exercises

1. If $\{X_i,\tau_i\}$, $i\in I$, is a countable family of spaces, show that there is a natural correspondence between the product space of this family as defined in this appendix and the product space as defined previously. In other words, prove that the product spaces formed in both ways are actually homeomorphic.

2. Formulate and prove a generalization of Proposition 20, Chapter 4.

3. Prove or disprove: The product of any family of nonempty first countable spaces is first countable.

4. Show that the product of a family of discrete spaces may not have the discrete topology. Does the product of any family of spaces with the trivial topology necessarily have the trival topology? Is it possible for an infinite product of infinite spaces to have the discrete topology?

5. Prove that the product topology on $Y^X$ (Example 2) is equivalent to the topology generated by saying that any net $\{f_i\}$, $i\in I$, in $Y^X$ converges to $f$ if and only if $f_i(x)\to f(x)$ for all $x\in X$.

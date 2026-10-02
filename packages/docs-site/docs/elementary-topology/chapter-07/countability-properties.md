---
title: Countability Properties — Elementary Topology
description: The available text of section 7.2 of the supplied second-edition scan.
---

# 7.2 Countability Properties

::: info Transcription note
Source: printed pages 144–146 and 148–151 (PDF pages 138–144). Printed page 147 is absent. All available definitions, statements, proof fragments, exercises, and source references are retained. In particular, the separation inequalities in Proposition 5, the references to Proposition 6 in Exercise 8, and the variable $x\in X$ in Exercise 7 remain as printed.
:::

<span id="printed-page-144"></span>

<!-- Source: PDF page 138, printed page 144; section 7.2 fragment. -->

One would rightly suspect that covering properties take the following general form: If $\{U_i\}$, $i\in I$, is any open cover of a space $X,\tau$, then there is an open subcover (or refinement) of $\{U_i\}$, $i\in I$, satisfying some special condition. One of the most natural conditions the subcover might satisfy is a cardinality condition. Such a cardinality condition is given in the following definition.

**Definition 2.** A space $X,\tau$ is said to be a _Lindelöf space_ if every open cover of $X$ has a countable open subcover.

<span id="printed-page-145"></span>

<!-- Source: PDF page 139, printed page 145. -->

**Example 4.** The space presented in Example 3 is certainly a Lindelöf space, since every open cover of $N$ not only has a countable subcover, but even has a finite subcover.

In order to discuss Lindelöf spaces more completely, more terminology is needed.

**Definition 3.** Let $X,\tau$ be a topological space. $X$ is said to be _first countable_ if there is an open neighborhood system for $\tau$ such that $\mathfrak{N}_x$ is countable for each $x\in X$. (See Section 6.1, Exercise 2.) $X$ is said to be _second countable_ if there is a basis for $\tau$ which consists of countably many sets. $X$ is said to be _separable_ if $X$ contains a countable dense subset. (For the definition of a dense subset, see Section 3.6).

**Example 5.** Let $R$ be the set of real numbers with the absolute value topology $\tau$. For each $x\in R$, let

$$
\mathfrak{N}_x=\{N(x,1/n)\mid n\text{ a positive integer}\}.
$$

It is easily verified that the collection of $\mathfrak{N}_x$ forms an open neighborhood system for $\tau$. However, each $\mathfrak{N}_x$ is countable; hence $R$ is first countable. (Actually, from Section 3.3, Exercise 6, we have the more general result that any metric space is first countable.) The set of rational numbers forms a countable dense subset of $R$, and hence $R$ is also separable. Proposition 5 will tell us that $R$ is second countable as well, and therefore is Lindelöf (Proposition 3).

**Example 6.** Let $X$ be any uncountable set with the discrete topology. For each $x\in X$, set $\mathfrak{N}_x=\{\{x\}\}$. Then the collection of $\mathfrak{N}_x$ forms an open neighborhood system for the discrete topology on $X$. Thus $X$ is first countable. Since $\operatorname{Cl}A=A$ for every $A\subset X$ (since every subset of $X$ is closed), the only dense subset of $X$ is $X$ itself. But $X$ is uncountable, and hence there is no countable dense subset of $X$; $X$ is therefore not separable. $X$ is neither second countable, nor Lindelöf (Exercise 6).

**Proposition 1.** Any second countable space is first countable.

_Proof._ Let $X,\tau$ be any second countable space, and let $\mathfrak{B}$ be a countable basis for $\tau$. Then the collection of sets of the form

$$
\mathfrak{N}_x=\{B\in\mathfrak{B}\mid x\in B\}\qquad\text{for all }x\in X
$$

forms an open neighborhood system for $\tau$ (Chapter 3, Proposition 6). Since $\mathfrak{B}$ is countable, each $\mathfrak{N}_x$ is also countable. Therefore $X$ is first countable.

**Proposition 2.** Any second countable space is separable.

<span id="printed-page-146"></span>

<!-- Source: PDF page 140, printed page 146. -->

_Proof._ Let $X,\tau$ be a second countable space, and let $\mathfrak{B}$ be a countable basis for $\tau$. For each $B\in\mathfrak{B}$, select $x_B\in B$. Then $\{x_B\mid B\in\mathfrak{B}\}$ is a countable subset of $X$. The proof that it is also dense is left as an exercise.

**Proposition 3.** Any second countable space is Lindelöf.

_Proof._ Let $X,\tau$ be a second countable space with $\mathfrak{B}$ as a countable basis. Suppose $\{U_i\}$, $i\in I$, is any open cover of $X$. We select a subcover of $\{U_i\}$, $i\in I$, as follows: Number the elements of $\mathfrak{B}$ sequentially, that is, $B_1,B_2,\ldots,B_n,\ldots$. Select $B_k$ from $\mathfrak{B}$ if there is a member $U_i$ of the open cover such that $B_k\subset U_i$. For each $B_k$ selected, choose one $U_i$ for which $B_k\subset U_i$ and call it $U_{k_i}$. Since the collection of $B_k$ selected must be countable, the collection of $U_{k_i}$ is also countable. It remains to be shown that

$$
\{U_{k_i}\mid B_k\text{ was selected}\}
$$

is actually a subcover of $\{U_i\}$, $i\in I$. Since $\{U_i\}$, $i\in I$, is an open cover of $X$ and each $U_i$ is the union of elements of $\mathfrak{B}$, the collection of selected $B_k$ actually forms a refinement of $\{U_i\}$, $i\in I$; therefore $\{U_{k_i}\mid B_k\text{ was selected}\}$ is an open subcover of $\{U_i\}$, $i\in I$.

For general topological spaces, no other implications hold between Lindelöf, first and second countable, and separable, other than those given in Propositions 1, 2, and 3.

The following proposition describes how these properties behave with respect to subspaces and product spaces.

**Proposition 4**

a) Any subspace of a first countable space is first countable.

b) Every subspace of a second countable space is second countable, and hence is also separable.

c) Every closed subspace of a Lindelöf space is Lindelöf; however, it is not true that every subspace of a Lindelöf space is necessarily Lindelöf.

d) The product space of a countable family of nonempty spaces is second countable if and only if each component space is second countable. (This is an example of a proposition which does not generalize to the product of an arbitrary family of spaces.)

e) The product of a countable family of nonempty Lindelöf spaces is not necessarily Lindelöf, but if a product space is Lindelöf and each component space is $T_1$, then each component space is also Lindelöf.

f) Any open subspace of a separable space is separable.

g) The product of a countable family of nonempty spaces is separable if and only if each component space is separable.

::: warning Missing source page 147
Printed page 147 is absent. Any intervening discussion or proof following Proposition 4 has not been reconstructed. The supplied text resumes on printed page 148 with the following introduction to Proposition 5.
:::

<span id="printed-page-148"></span>

<!-- Source: PDF page 141, printed page 148. -->

The next proposition shows that in metric spaces, the properties of being Lindelöf, separable, and second countable are all equivalent. We have already seen that any metric space is first countable. Example 6 together with Exercise 6 furnishes an example of a metric space which is not second countable.

**Proposition 5.** If $X,D$ is a metric space, then the following statements are equivalent:

a) $X$ is Lindelöf. b) $X$ is separable. c) $X$ is second countable.

_Proof._ Since it has already been shown in Propositions 2 and 3 that any second countable space is both separable and Lindelöf, it will suffice to show that if $X$ is either separable or Lindelöf, then $X$ is second countable.

Statement (b) implies statement (c). Suppose $X$ is separable and let $\{x_n\mid n\in N\}$ be a countable dense subset of $X$. Let $B(n,m)=N(x_n,1/m)$, where $m$ and $n$ are in $N$. We shall show that

$$
\mathfrak{B}=\{B(n,m)\mid n,m\in N\}
$$

is a basis for the metric topology on $X$. Let $U$ be any open subset of $X$ and let $x$ be any point of $U$. Since $U$ is open, there is a positive number $p$ such that $N(x,p)\subset U$. Choose any integer $m>2/p$. Since $N(x,1/2m)$ is open and $\{x_n\mid n\in N\}$ is dense, there is some

$$
x_n\in N(x,1/2m)
$$

(Proposition 14, Chapter 3). Then $x\in N(x_n,1/m)$. Since $m>2/p$, $1/m<p/2$; thus $N(x_n,1/m)\subset N(x,p)$. Therefore $N(x_n,1/m)\subset U$ as well. But then $U$ is the union of members of $\mathfrak{B}$ [for $x$ was an arbitrary element of $U$ and $N(x_n,1/m)\in\mathfrak{B}$]. Since $U$ was an arbitrary open set, $\mathfrak{B}$ is a basis for the metric topology. Moreover $\mathfrak{B}$ is countable, and hence $X$ is second countable.

<figure class="topology-chapter-figure">
  <img src="/elementary-topology/figures/figure-7.1.svg" alt="Separated closed metric neighborhoods around points of E, with V outside their union." />
  <figcaption>Figure 7.1</figcaption>
</figure>

Statement (a) implies statement (b). Suppose $X$ is Lindelöf. Choose some $p>0$, and let $E$ be a maximal subset of $X$ having the property that $D(a,b)\geq p$ for all $a,b\in E$. Such a maximal subset can be shown to <span id="printed-page-149"></span><!-- Source: PDF page 142, printed page 149. --> exist by Zorn's lemma. For each $a\in E$, consider

$$
N(a,p/2)\qquad\text{and}\qquad V=X-\bigcup\{\operatorname{Cl}N(a,p/4)\mid a\in E\}
$$

(Fig. 7.1). $V$ is open (Exercise 2). Then $\{V\}\cup\{N(a,p/2)\mid a\in E\}$ is a covering of $X$ by open sets. Since $X$ is Lindelöf, there is a countable subcovering. But if $N(a,p/2)$ were omitted from the original cover for any $a\in E$, the remaining sets would fail to cover $X$ since none of them would contain $a$. Therefore $\{N(a,p/2)\mid a\in E\}$ must itself be countable; hence $E$ is countable.

Carry out the construction described above for $p=1/n$, $n=1,2,3,\ldots$, and get $\{E_n\}$, $n\in N$, where $E_n$ is the set corresponding to $p=1/n$; that is, $E_n$ is a maximal set having the property that $D(a,b)\geq1/n$ for any $a,b\in E_n$. Let $S=\bigcup_N E_n$. Since $S$ is the union of countably many countable sets, $S$ is countable. We now show that $S$ is dense in $X$.

Suppose $x\in X$ and $q>0$; we will show that there is $z\in S$ such that $z\in N(x,q)$. Take $n>1/q$. Then there is $z\in E_n$ such that $z\in N(x,q)$. For if not, then $x$ has the property that $D(x,w)\geq1/n$ for each $w\in E_n$, but $x\notin E_n$, and thus $E_n$ would not be maximal. If $U$ is any nonempty open subset of $X$, choose $x\in U$ and $q>0$ such that $N(x,q)\subset U$. Then $N(x,q)$, and hence $U$, contains an element of $S$. Therefore $S$ is dense (Proposition 14, Chapter 3).

**Corollary.** The space $R$ of real numbers with the absolute value topology is second countable (Example 5) as is the product space $R^n$ for any $n$ (Proposition 4d).

We close this section with a proposition that will be needed in the proof of a key result in a later chapter.

**Proposition 6.** A $T_3$ Lindelöf space is $T_4$.

_Proof._ Let $X,\tau$ be a $T_3$ Lindelöf space and let $A$ and $B$ be disjoint closed subsets of $X$. If $x\in A$, then $X-B$ is a neighborhood of $x$. Since $X$ is $T_3$, there is a neighborhood $U_x$ of $x$ such that $\operatorname{Cl}U_x\subset X-B$ (Chapter 5, Proposition 4). Similarly, if $x\in B$, there is a neighborhood $U_x$ of $x$ such that $\operatorname{Cl}U_x\subset X-A$. If $x$ is not an element of either $A$ or $B$, then $X-(A\cup B)$ is a neighborhood of $x$; hence we may find a neighborhood $U_x$ of $x$ such that $\operatorname{Cl}U_x\subset X-(A\cup B)$ (and thus $\operatorname{Cl}U_x\cap(A\cup B)=\phi$). The family of $U_x$ for each $x\in X$ is an open cover for $X$. Since $X$ is Lindelöf, this cover has a countable subcover $\{U_{x_n}\mid n=1,2,3,\ldots\}$.

Let $U_1,U_2,\ldots$ be the $U_{x_n}$ (relabeled for convenience) which meet $A$, and let $V_1,V_2,\ldots$ be the $U_{x_n}$ which meet $B$. Then for each positive integer $n$, $\operatorname{Cl}U_n\cap B=\phi$ and $\operatorname{Cl}V_n\cap A=\phi$; moreover $A\subset\bigcup_N U_n$ and $B\subset\bigcup_N V_n$. Define $W_1=U_1$ and set $Y_1=V_1-\operatorname{Cl}W_1$. Let $W_2=U_2-\operatorname{Cl}Y_1$ and $Y_2=V_2-(\operatorname{Cl}W_1\cup\operatorname{Cl}W_2)$. Suppose $W_n$ and $Y_n$ <span id="printed-page-150"></span><!-- Source: PDF page 143, printed page 150. --> have been defined. Then set

$$
W_{n+1}=U_{n+1}-(\operatorname{Cl}Y_1\cup\operatorname{Cl}Y_2\cup\cdots\cup\operatorname{Cl}Y_n)
$$

and

$$
Y_{n+1}=V_{n+1}-(\operatorname{Cl}W_1\cup\operatorname{Cl}W_2\cup\cdots\cup\operatorname{Cl}W_{n+1}).
$$

$W_n$ is always an open set since

$$
\begin{aligned}
W_n&=U_n\cap(X-(\operatorname{Cl}Y_1\cup\cdots\cup\operatorname{Cl}Y_{n-1}))\\
&=U_n\cap(X-\operatorname{Cl}(Y_1\cup\cdots\cup Y_{n-1}));
\end{aligned}
$$

hence $W_n$ is the intersection of two open sets, and is therefore open. Similar reasoning shows that $Y_n$ is open for each $n$.

Set $H=\bigcup_N W_n$ and $K=\bigcup_N Y_n$. Since $H$ and $K$ are the union of open sets, they are open. Suppose $a\in A$. Then $a\in U_n$ for some $n$, and

$$
W_n=U_n-(\operatorname{Cl}Y_1\cup\cdots\cup\operatorname{Cl}Y_{n-1}).
$$

But for any $k$, $\operatorname{Cl}Y_k\subset\operatorname{Cl}V_k$ and $\operatorname{Cl}V_k\cap A=\phi$. Therefore $a\notin\operatorname{Cl}Y_k$ for any $k$. We have then that $a\in W_n$. Therefore $A\subset\bigcup_N W_n=H$. Similarly, $B\subset K$. In order to show that $X$ is $T_4$, we now have merely to prove that $H\cap K=\phi$.

Suppose $x\in H\cap K$. Then $x\in W_n\cap Y_m$ for some $m$ and $n$. Suppose $m\geq n$. Then

$$
x\in Y_m=V_m-(\operatorname{Cl}W_1\cup\cdots\cup\operatorname{Cl}W_n\cup\cdots\cup\operatorname{Cl}W_m);
$$

hence $x$ could not be in $\operatorname{Cl}W_n$, a contradiction. On the other hand, if $m<n$, then

$$
x\in W_n=U_n-(\operatorname{Cl}Y_1\cup\cdots\cup\operatorname{Cl}Y_m\cup\cdots\cup\operatorname{Cl}Y_{n-1}).
$$

Thus $x\notin\operatorname{Cl}Y_m$, again a contradiction. Therefore $H$ and $K$ are disjoint open subsets of $X$, which contain $A$ and $B$, respectively, and hence $X$ is $T_4$.

**Example 7.** Let $R$ be the set of real numbers with the topology $\tau$ as described in Example 11 of Chapter 3. The set $Q$ of rational numbers is a dense subset of $R$ since any basis element of $\tau$ [i.e., an interval of the form $[a,b)$] contains a rational number. Therefore $R$ is separable. $R$ is also first countable [for each $x\in R$, set $\mathfrak{N}_x=\{[x,x+1/n)\mid n\in N\}$]. $R$ cannot be second countable, however, For if $R$ were second countable, then the product space $R^2$ would also be second countable and hence Lindelöf. But product space $R^2$ was shown in Example 13 of Chapter 5 to be $T_3$, but not $T_4$. If $R^2$ were Lindelöf and $T_3$, then by Proposition 6 it would have to be $T_4$.

<span id="printed-page-151"></span>

<!-- Source: PDF page 144, printed page 151. -->

## Exercises

1. Prove that the set $\{x_B\mid B\in\mathfrak{B}\}$ in Proposition 2 is dense in $X$.

2. The following refer to the proof of Proposition 5.

   a) Prove that there is a maximal subset $E$ as claimed.

   b) Prove that the set

   $$
   V=X-\bigcup\{\operatorname{Cl}N(a,p/4)\mid a\in E\}
   $$

   is open. There are a number of possible approaches to this problem. One approach, for example, is to show that given any $w\in V$, $N(w,1)$ intersects $\operatorname{Cl}N(a,p/4)$ for at most finitely many $a\in E$. This means that there is $p'>0$ such that $N(w,p')$ does not intersect any of the $\operatorname{Cl}N(a,p/4)$.

3. Prove (g) of Proposition 4.

4. By Proposition 4(g), $R^2$ as described in Example 7 is separable. Let

   $$
   A=\{(x,y)\mid x+y=0\}\subset R^2.
   $$

   Prove that $A$ is a nonseparable subspace of $R^2$. Is $A$ closed? Is $A$ Lindelöf? Give another proof that $R^2$ is not Lindelöf without appealing to Proposition 6.

5. A point $x$ of a space $X,\tau$ is said to be a _condensation point_ of a subset $A$ of $X$ if each neighborhood of $x$ meets $A$ in uncountably many points. Let $A^-$ denote the set of condensation points of $A$. Suppose $X$ is a Lindelöf space. Prove that if $A$ is uncountable, then $A^-\ne\phi$. [Hint: Try to construct a countable open cover of $X$ each of whose members contains countably many of the elements of $A$, and hence arrive at the contradiction that $A$ is countable.]

6. Let $X$ be an uncountable set with the discrete topology. Prove that the collection of $\mathfrak{N}_x$ as described in Example 6 forms an open neighborhood system for the discrete topology. Find a metric on $X$ which induces the discrete topology. Prove that $X$ is not Lindelöf, and hence that $X$ is neither separable nor second countable (Proposition 5).

7. Let $X$ be the set of continuous functions from the space $R$ of real numbers with the absolute value topology into itself. For each $f\in X$ and $p>0$, define

   $$
   N(f,p)=\{g\in X\mid |f(x)-g(x)|<p\text{ for all }x\in R\}.
   $$

   The family of $N(f,p)$ for all $f\in X$ and all $p>0$ forms the basis for a topology $\tau$ on $X$. Try to determine if $X,\tau$ is second countable. Let

   $$
   Y=\{f\in X\mid f\text{ has derivatives of all orders at each }x\in X\}.
   $$

   Is $Y$ a second countable subspace of $X$?

8. Prove directly, that is, without using Proposition 6, that the space $R$ of real numbers with the usual absolute value metric topology is second countable. [Hint: Prove that $\{N(x,q)\mid q>0,\ q\text{ and }x\text{ rational}\}$ gives a countable basis.]

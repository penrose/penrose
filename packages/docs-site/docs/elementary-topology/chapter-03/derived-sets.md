---
title: Derived Sets — Elementary Topology
description: Section 3.5, printed pages 55–59, of Elementary Topology, second edition.
---

# 3.5 Derived Sets

::: info Transcription note
Source: printed pages 55–59 (PDF pages 58–62). Printed page 59 continues with §3.6 after the exercises. The source's definitions of closure, interior, frontier, exterior, and derived set, and its references to missing Chapter 2 material, are retained.
:::

<span id="printed-page-55"></span>

<!-- source: PDF 58, printed 55; section 3.5 fragment -->

Let $X,\tau$ be a topological space. Then associated with any subset $A$ of $X$, there are a number of sets which are topologically related to or “derived” from $A$. We have already encountered some of these sets in the discussion of metric spaces.

**Definition 7.** If $A\subset X$, where $X,\tau$ is a topological space, then we define

a) the _closure_ of $A$, denoted by $\operatorname{Cl}A$, to be the intersection of all closed sets which contain $A$ (cf. Chapter 2, Definition 8 and Proposition 15);

b) the _interior_ of $A$, denoted by $A^\circ$, to be the union of all open sets which are contained in $A$ (Section 3.1, Exercise 2);

c) the _frontier_ of $A$, denoted by $\operatorname{Fr}A$, to be

$$
\begin{gathered}
\{x\mid\text{each open set which contains }x\text{ contains points of}\\
\text{both }A\text{ and }X-A\}
\end{gathered}
$$

(Section 2.7, Exercise 6);

<span id="printed-page-56"></span>

<!-- source: PDF 59, printed 56 -->

d) the _exterior_ of $A$, denoted by $\operatorname{Ext}A$, to be $X-\operatorname{Cl}A$; and

e) the _derived set_ of $A$ (sometimes called the _weak derived set_), denoted by $A'$, to be

$$
\begin{gathered}
\{x\mid\text{if }x\in U,U\text{ an open set, then }A\cap(U-\{x\})\neq\phi;\\
\text{that is, if }x\in U,\text{ then }U-\{x\}\text{ contains some point of }A\}.
\end{gathered}
$$

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-3.4.svg" alt="Figure 3.4: The unit disk and its topologically derived sets." /><figcaption>Figure 3.4</figcaption></figure>

**Example 18.** Let $R^2$ be the plane with the topology induced by the Pythagorean metric $D$. Let

$$
A=\{(x,y)\mid x^2+y^2<1\}
$$

(Fig. 3.4). Then

$$
\begin{gathered}
\operatorname{Cl}A=\{(x,y)\mid x^2+y^2\leq1\},\qquad A^\circ=A,\\
\operatorname{Fr}A=\{(x,y)\mid x^2+y^2=1\},\\
\operatorname{Ext}A=\{(x,y)\mid x^2+y^2>1\},\quad\text{and}\quad A'=\operatorname{Cl}A.
\end{gathered}
$$

The reader is not expected to see all of these equalities until more information has been obtained about these topologically derived sets, but he should verify as many as possible. He should also examine this example for possible relations that might hold between the sets topologically associated with $A$. These reflections also hold for the following example.

**Example 19.** Let $R$ be the set of real numbers with the topology induced by the absolute value metric, and let $A$ be the set of rational numbers. Then $\operatorname{Cl}A=R$, $A^\circ=\phi$, $\operatorname{Fr}A=R$, $\operatorname{Ext}A=\phi$ (note that both $A^\circ$ and $\operatorname{Ext}A$ can simultaneously be empty), and $A'=R$. If $R$ is given the trivial topology, then these sets topologically associated with $A$ are exactly the same as in the metric topology. We can conclude then that what the topologically derived sets happen to be for any one subset of $R$

<span id="printed-page-57"></span>

<!-- source: PDF 60, printed 57 -->

does not give much information about the topology. If, however, we know, say, $\operatorname{Cl}A$ for _every_ $A\subset X$, then the topology is completely determined, as we shall see from Proposition 11.

**Proposition 10.** Suppose $X,\tau$ is a topological space and $A$ and $B$ are any subsets of $X$. Then

i) $A\subset\operatorname{Cl}A$;

ii) $\operatorname{Cl}(\operatorname{Cl}A)=\operatorname{Cl}A$;

iii) $\operatorname{Cl}(A\cup B)=\operatorname{Cl}A\cup\operatorname{Cl}B$;

iv) $\operatorname{Cl}\phi=\phi$;

v) $A$ is closed if and only if $A=\operatorname{Cl}A$.

_Proof._ $\operatorname{Cl}A$ is the intersection of a family of sets each of which contains $A$; therefore $A\subset\operatorname{Cl}A$, and (i) is proved. Since $\operatorname{Cl}A$ is the intersection of a family of closed sets, $\operatorname{Cl}A$ is closed. If $A$ is already closed, then $A$ is one of the closed sets which contains $A$; hence $\operatorname{Cl}A\subset A$. Since $A\subset\operatorname{Cl}A$ by (i), $A=\operatorname{Cl}A$. We have therefore proved (v), (ii), and (iv).

It still remains to prove (iii). Since $A\subset\operatorname{Cl}(A\cup B)$ and $B\subset\operatorname{Cl}(A\cup B)$, we have

$$
\begin{gathered}
\operatorname{Cl}A\subset\operatorname{Cl}(\operatorname{Cl}(A\cup B))=\operatorname{Cl}(A\cup B)\\
\text{and}\quad\operatorname{Cl}B\subset\operatorname{Cl}(A\cup B).
\end{gathered}
$$

Therefore $\operatorname{Cl}A\cup\operatorname{Cl}B\subset\operatorname{Cl}(A\cup B)$. On the other hand, since $\operatorname{Cl}A\cup\operatorname{Cl}B$ is the union of two closed sets, it is closed. Thus $\operatorname{Cl}A\cup\operatorname{Cl}B$ is a closed set which contains $A\cup B$; consequently, $\operatorname{Cl}(A\cup B)\subset\operatorname{Cl}A\cup\operatorname{Cl}B$. Therefore $\operatorname{Cl}(A\cup B)=\operatorname{Cl}A\cup\operatorname{Cl}B$.

**Proposition 11.** Let $X$ be any set, and suppose that $\operatorname{Cl}$ is a function from the set of subsets of $X$ into the set of subsets of $X$ such that $\operatorname{Cl}$ satisfies (i) through (iv) of Proposition 10. Then if we define a subset of $X$ to be closed in accordance with (v), the collection $\mathfrak{F}$ of closed subsets thus obtained satisfies (i′) through (iii′) of Proposition 2 and hence determines a topology $\tau$ on $X$. Moreover, $\operatorname{Cl}A$ is the closure of $A$ with respect to $\tau$ for each subset $A$ of $X$.

_Proof._ We must verify (i′) through (iii′) of Proposition 2.

i′) Since $X\subset\operatorname{Cl}X\subset X$, $X=\operatorname{Cl}X$; therefore $X$ is closed. Then $\operatorname{Cl}\phi=\phi$ by (iv).

ii′) Let $F$ and $F'$ be any two closed sets. Then $F=\operatorname{Cl}A$ and $F'=\operatorname{Cl}B$, where $A$ and $B$ are subsets of $X$. Hence

$$
F\cup F'=\operatorname{Cl}A\cup\operatorname{Cl}B=\operatorname{Cl}(A\cup B),
$$

and it follows that $F\cup F'$ is also a closed subset.

iii′) Let $\{F_i\}$, $i\in I$, be any family of closed subsets of $X$. Then $F_i=\operatorname{Cl}F_i$ for each $i$. Now $\bigcap_I F_i\subset F_i$ for each $i$; it follows

<span id="printed-page-58"></span>

<!-- source: PDF 61, printed 58 -->

therefore that

$$
\operatorname{Cl}\left(\bigcap_I F_i\right)\subset\operatorname{Cl}F_i=F_i.
$$

[For $\bigcap_I F_i\subset F_i$ implies $(\bigcap_I F_i)\cup F_i=F_i$, and thus

$$
\begin{aligned}
\operatorname{Cl}\left(\left(\bigcap_I F_i\right)\cup F_i\right)
&=\operatorname{Cl}\left(\bigcap_I F_i\right)\cup\operatorname{Cl}F_i\\
&=\operatorname{Cl}F_i=\operatorname{Cl}\left(\bigcap_I F_i\right)\cup F_i=F_i.
\end{aligned}
$$

]

Therefore

$$
\operatorname{Cl}\left(\bigcap_I F_i\right)\subset\bigcap_I F_i.
$$

By (i), however, $\bigcap_I F_i\subset\operatorname{Cl}(\bigcap_I F_i)$; hence

$$
\bigcap_I F_i=\operatorname{Cl}\left(\bigcap_I F_i\right).
$$

We have shown then that $\bigcap_I F_i$ is closed. The family $\mathfrak{F}$ of closed subsets of $X$ therefore satisfies (i′) through (iii′) of Proposition 2 and hence defines a topology on $X$.

Now if $A$ is any subset of $X$, then since $\operatorname{Cl}(\operatorname{Cl}A)=\operatorname{Cl}A$, $\operatorname{Cl}A$ is closed with respect to $\tau$. But $A\subset\operatorname{Cl}A$ by (i); hence the $\tau$-closure of $A$ is a subset of $\operatorname{Cl}A$. If $F$ is any set such that $\operatorname{Cl}F=F$ and $A\subset F$, then $\operatorname{Cl}A\subset\operatorname{Cl}F=F$. Therefore the intersection of all such $F$, the $\tau$-closure of $A$, contains $\operatorname{Cl}A$; that is, $\operatorname{Cl}A\subset\tau$-closure of $A$. Hence $\operatorname{Cl}A$ is the same as the $\tau$-closure of $A$.

## Exercises

1. Suppose $X,\tau$ a topological space. If $A$ and $B$ are any two subsets of $X$, show that it is not true in general that $\operatorname{Cl}(A\cap B)=\operatorname{Cl}A\cap\operatorname{Cl}B$.
2. Let $X,\tau$ be any topological space. Compute $\operatorname{Cl}X$, $X^\circ$, $\operatorname{Fr}X$, $\operatorname{Ext}X$, $X'$, and the corresponding sets for $\phi$.
3. Suppose $X$ is any set and $\circ$ is a function from the set of subsets of $X$ to the set of subsets of $X$ with the following properties:

   i) $A^\circ\subset A$,

   ii) $(A^\circ)^\circ=A^\circ$,

   iii) $(A\cap B)^\circ=A^\circ\cap B^\circ$, and

   iv) $X^\circ=X$, where $A$ and $B$ are any subsets of $X$.

   Define a subset $U$ of $X$ to be open if and only if $U^\circ=U$. Prove that the set of open sets thus defined gives a topology on $X$.

4. Suppose that $X$ is any set and that $\tau$ and $\tau'$ are topologies on $X$ with $\tau$ finer than $\tau'$. If $A\subset X$, we will denote the closure of $A$ with respect to $\tau$ by $\operatorname{Cl}A$ and the closure of $A$ with respect to $\tau'$ by $\operatorname{Cl}'A$. Analogous notation will be used with regard to the other sets topologically derived from $A$. Prove

<span id="printed-page-59"></span>

<!-- source: PDF 62, printed 59; section 3.5 fragment -->

that

a) $\operatorname{Cl}A\subset\operatorname{Cl}'A$;

b) $A^{\circ'}\subset A^\circ$.

c) Find relations between the corresponding other sets topologically derived from $A$.

5. Try to find a method for specifying a topology on a set $X$ by specifying $\operatorname{Fr}A$ for each $A\subset X$. Do likewise for $\operatorname{Ext}$.
6. Suppose that $X$ is a set with the discrete topology and that $A\subset X$. Find sets topologically associated with $A$.
7. Is it possible for two distinct subsets of a topological space to have exactly the same topologically derived sets? Support your assertion.
8. Suppose $\tau$ and $\tau'$ are topologies on a set $X$. Determine if each of the following conditions implies either $\tau\subset\tau'$ or $\tau'\subset\tau$. In the following, $A$ stands for any subset of $X$; we use $'$ to indicate that a derived set is being taken relative to $\tau'$.

   a) $\operatorname{Fr}A\subset\operatorname{Fr}'A$

   b) $\operatorname{Cl}A\subset\operatorname{Cl}'A$

   c) $\operatorname{Ext}A\subset\operatorname{Ext}'A$

[Continue to 3.6 More About Topologically Derived Sets](./topologically-derived-sets)

<style>
.topology-chapter-figure { max-width: 28rem; margin: 1.75rem auto; text-align: center; }
.topology-chapter-figure img { width: 100%; background: white; }
.topology-chapter-figure figcaption { font-family: Georgia, serif; font-size: 0.875rem; margin-top: 0.4rem; }
</style>

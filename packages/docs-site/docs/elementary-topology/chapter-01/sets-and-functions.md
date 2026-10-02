---
title: Sets and Functions — Elementary Topology
description: Section 1.1, available printed pages 1–3, of Elementary Topology, second edition.
---

# 1.1 Sets and Functions

::: info Transcription note
Source: printed pages 1–3 (PDF pages 11–13). Printed page 4 is missing. The source uses $\phi$ for the empty set and $\subset$ for the subset relation, including equality; these notations are retained.
:::

<span id="printed-page-1"></span>

<!-- source: PDF 11, printed 1 -->

It is assumed that any reader of this book has already had some experience with sets; hence most of what is said in this section will be for the sake of review rather than for the purpose of presenting new material.

We will not deal with sets axiomatically. A set will be taken to be any well-defined collection of objects; the objects in a set are called _elements_, or _points_, of the set. If $x$ is an element of the set $S$, we write $x\in S$. We denote the phrase _is not an element of_ by $\notin$.

Sets may be denoted either by explicitly listing their elements inside of braces (for example, $\{1,2,3\}$ is the set having 1, 2, and 3 as elements) or by giving the rule by which a typical object of the set is determined (for example, $\{x\mid x\text{ is a red schoolhouse}\}$ is the set of all red schoolhouses or, alternatively, the set of all $x$ such that $x$ is a red schoolhouse).

A set $S$ is said to be a _subset_ of a set $T$ if each element of $S$ is an element of $T$. We usually denote _$S$ is a subset of $T$_ by $S\subset T$. Two sets $S$ and $T$ are _equal_ if they contain exactly the same elements; that is, $S=T$ if $S\subset T$ and $T\subset S$. The phrase _is not a subset of_ is denoted by $\not\subset$.

The _empty set_, that is, the set which contains no elements whatsoever, is denoted by $\phi$.

If $S$ and $T$ are any two sets, then the _complement_ of $S$ in $T$ is the set of all elements of $T$ which are not elements of $S$; we denote the complement of $S$ in $T$ by $T-S$. Similarly, the complement of $T$ in $S$, denoted by $S-T$, is the set of all elements of $S$ which are not elements of $T$.

The two most basic set operations are _union_ and _intersection_. If $\{S_i\}$, $i\in I$, is any family of sets indexed by some set $I$, then the _union_ of this family of sets is $\{x\mid x\in S_i\text{ for at least one }i\in I\}$. (We will rigorously define the notion of an index set later in this section; for now the reader can consider $I$ to be merely a set of labels distinguishing the various members of the family of sets.) The union of $\{S_i\}$, $i\in I$, may be denoted by $\bigcup_I S_i$, or $\bigcup\{S_i\mid i\in I\}$. The _intersection_ of this family of sets is $\{x\mid x\in S_i\text{ for every }i\in I$, that is, $x$ is an element of every member of the family of sets$\}$. The intersection of $\{S_i\}$, $i\in I$, may be denoted by $\bigcap_I S_i$, or

<span id="printed-page-2"></span>

<!-- source: PDF 12, printed 2 -->

$\bigcap\{S_i\mid i\in I\}$. Where only a few sets are involved, say $\{S_1,S_2,S_3\}$, the intersection and union of these sets may be denoted by $S_1\cap S_2\cap S_3$ and $S_1\cup S_2\cup S_3$, respectively.

It is assumed that the reader is moderately familiar with these set operations, at least so far as any finite family of sets is concerned. We now prove the _DeMorgan formulas_ for an arbitrary family of sets.

**Proposition 1.** Suppose $\{S_i\}$, $i\in I$, is a family of subsets of some set $T$. Then

$$
\begin{aligned}
\text{a)}\quad &\bigcup_I(T-S_i)=T-\bigcap_I S_i;\\
\text{b)}\quad &\bigcap_I(T-S_i)=T-\bigcup_I S_i.
\end{aligned}
$$

_Proof._ a) To prove that any two sets are equal, we must show that they contain the same elements. Suppose $x\in\bigcup_I(T-S_i)$; then $x\in T-S_i$ for at least one $i\in I$. Therefore $x\notin S_i$ for at least one $i\in I$. Then $x$ is not in $S_i$ for every $i\in I$; hence $x\notin\bigcap_I S_i$. Consequently, $x\in T-\bigcap_I S_i$. We have thus proved that every element of $\bigcup_I(T-S_i)$ is an element of $T-\bigcap_I S_i$; that is,

$$
\bigcup_I(T-S_i)\subset T-\bigcap_I S_i.
$$

Suppose $x\in T-\bigcap_I S_i$. Then there is some $i\in I$ for which $x\notin S_i$ (or else $x$ would be an element of $\bigcap_I S_i$). Therefore $x\in T-S_i$ for some $i\in I$. Then $x\in\bigcup_I(T-S_i)$. Consequently, $T-\bigcap_I S_i\subset\bigcup_I(T-S_i)$; hence

$$
T-\bigcap_I S_i=\bigcup_I(T-S_i).
$$

The proof of (b) is left as an exercise.

If $S$ and $T$ are any two sets, then the _Cartesian product_ of $S$ and $T$ is defined to be the set of all ordered pairs $(s,t)$ such that $s\in S$ and $t\in T$. The Cartesian product of $S$ and $T$ is denoted by $S\times T$.

If $S$ and $T$ are any sets, then a subset $R$ of $S\times T$ is said to be a _relation between $S$ and $T_. A subset of $S\times S$ is said to be a _relation on $S$_. If $R$ is a relation between $S$ and $T$, that is, if $R\subset S\times T$, then if $(s,t)\in R$, we may also write $sRt$, or say that $s$ and $t$ are _$R$-related_. Some special types of relations will be discussed in the next section. Although strictly speaking a relation is a set, at times a phrase or symbol defining the relation will be used in place of the actual set. For example, although _is equal to_ defines a relation on the collection of subsets of some set, we usually write simply $S=T$ if $S$ and $T$ are equal subsets, rather than explicitly refer to any relation.

A _function_ $f$ from a set $S$ into a set $T$ is a relation between $S$ and $T$ such that each element of $S$ is $f$-related to one and only one element of $T$.

<span id="printed-page-3"></span>

<!-- source: PDF 13, printed 3 -->

If $(s,t)\in f$, then we may write $t=f(s)$. Functions are usually defined by giving a rule which enables us to find $f(s)$ whenever $s$ is given. Again, rarely is explicit mention made of the fact that a function is a set. Functions are also called _maps_ or _mappings_.

If $f$ is a function from $S$ into $T$, then $S$ is called the _domain_ of $f$, $T$ the _range_ of $f$, and $\{t\in T\mid t=f(s)\text{ for some }s\in S\}$ the _image_ of $f$. The image of $f$ may be denoted by $f(S)$.

If $f$ is a function from $S$ into $T$ and $W\subset S$, then the _restriction_ of $f$ to $W$, denoted by $f\mid W$, is a function from $W$ into $T$ defined by $f\mid W(w)=f(w)$ for each $w\in W$.

If $f$ is a function from $S$ into $T$, we may write $f:S\to T$. If $f(S)=T$, then $f$ is said to be _onto_. If $f(s)=f(s')$ implies $s=s'$ for any $s,s'\in S$, then $f$ is said to be _one-one_; that is, $f$ is one-one if each element of $T$ is the image of at most one element of $S$.

Suppose $f:S\to T$. If $t\in T$, then

$$
f^{-1}(t)=\{s\in S\mid f(s)=t\}.
$$

If $U\subset T$, then

$$
f^{-1}(U)=\{s\in S\mid f(s)\in U\}.
$$

By $f^{-1}$ we mean $\{(t,s)\mid(s,t)\in f\}$. Note that $f^{-1}$ is a relation between $T$ and $S$, called the _inverse relation_ of $f$, and that it is a function from $T$ to $S$ if and only if $f$ is one-one and onto.

Suppose $f:S\to T$ and $g:T\to W$. Then $g\circ f$ is defined by

$$
\begin{gathered}
\{(s,w)\mid s\in S,\ w\in W,\text{ such that there is some }t\in T\text{ with }t=f(s)\text{ and}\\
w=g(t);\text{ that is, }w=g(f(s))\text{ for some }s\in S\}.
\end{gathered}
$$

$g\circ f$ is a function from $S$ into $W$ and is called the _composition_ of $g$ with $f$.

There are two special types of functions, _sequences_ and _indices_, which the reader should already have encountered at least informally. A _sequence_ $u$ in a set $S$ is any function from the set $N$ of positive integers into $S$. If $u$ is a sequence in $S$, then $u(n)$ is usually denoted by $u_n$; the sequence itself may be denoted by $u$, $\{u_n\}$, $n\in N$, or $\{u_1,u_2,u_3,\ldots\}$.

Sometimes the elements of one set are used to label the elements of another set, this often being a convenient way to express a collection of objects, or sets. For example, the elements of $\{1,2,3\}$ are used to label the elements of $\{t_1,t_2,t_3\}$. A one-one and onto function $f$ from some set $I$ onto a set $S$ for the purpose of labeling the elements of $S$ is called a _system of indices_ for $S$, and $I$ is called the _set of indices_, or the _index set_. The set $S$ is said to be _indexed by $I$_, and we may represent this relationship by writing $S$ as $\{s_i\}$, $i\in I$.

::: warning Source gap: printed page 4
Printed page 4 is absent from the supplied scan. No text or exercises from that page have been reconstructed. The next available page is printed page 5, which is already within §1.2 and begins in the middle of a sentence.
:::

[Continue to 1.2 Orderings; Equivalence Relations](./orderings-equivalence-relations)

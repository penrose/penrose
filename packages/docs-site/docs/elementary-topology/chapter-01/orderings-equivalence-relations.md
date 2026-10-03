---
title: Orderings; Equivalence Relations — Elementary Topology
description: The available text of section 1.2, printed pages 5–9, of Elementary Topology, second edition.
---

# 1.2 Orderings; Equivalence Relations

::: warning Source gap: printed page 4
The beginning of this section is on a page missing from the supplied scan. The transcription below starts with the first available words on printed page 5, without supplying the missing beginning of the sentence.
:::

<span id="printed-page-5"></span>

<!-- source: PDF 14, printed 5 -->

to is denoted by $\leq$, then $\leq$ defines a relation on $R$ having the properties that

- **P1)** $x\leq x$, for any $x\in R$;
- **P2)** $x\leq y$ and $y\leq x$ implies $x=y$, for any $x,y\in R$; and
- **P3)** $x\leq y$ and $y\leq z$ implies $x\leq z$ for any $x,y$, and $z$ in $R$.

Any relation on any set $S$ which shares properties P1 through P3 is called a _partial ordering_ on $S$. If $S$ has a partial ordering defined on it, then $S$ is said to be a _partially ordered set_. We may denote a set $S$ with partial ordering $\leq$ by $S,\leq$.

**Example 1.** Let $P(S)$ be the family of subsets of a set $S$. Then $\subset$ defines a partial ordering on $P(S)$. We verify that $\subset$ satisfies P1 through P3.

- P1) If $W$ is any subset of $S$, then $W\subset W$.
- P2) If $W$ and $T$ are any two subsets of $S$ such that $W\subset T$ and $T\subset W$, then $W=T$.
- P3) If $W,T$, and $Z$ are any sets such that $W\subset T$ and $T\subset Z$, then each element of $W$ is an element of $T$. But since $T\subset Z$, each element of $T$ is also an element of $Z$; therefore each element of $W$ is an element of $Z$, that is, $W\subset Z$.

Note that in Example 1, it is not true that given any two subsets $W$ and $T$ of $S$, either $W\subset T$ or $T\subset W$. It is true, however, that given any two real numbers $s$ and $t$, either $s\leq t$ or $t\leq s$. In $P(S),\subset$, any two elements are not necessarily comparable, whereas in $R,\leq$, any two elements are comparable. If $S$ is any set with a partial ordering $\leq$, then the partial ordering of $S$ is said to be a _total ordering_ if given any elements $s$ and $t$ of $S$, either $s\leq t$ or $t\leq s$. The partial ordering $\leq$ on the set of real numbers is a total ordering, but $\subset$ does not define a total ordering on $P(S)$ in Example 1.

Since the less than or equal to relation on the set of real numbers is the prototype of a partial ordering, we will generally denote a partial ordering by $\leq$, unless there is a special symbol called for.

Suppose $S$ is a set partially ordered by $\leq$, and $W\subset S$. Then $W$ can also be considered to be partially ordered by $\leq$ through the device of letting $w\leq w'$ for any two elements of $W$ if and only if $w\leq w'$ considering $w$ and $w'$ as elements of $S$. We say the ordering $\leq$ on $S$ _induces_ an ordering on $W$.

Let $S,\leq$ be any partially ordered set, and suppose $W\subset S$. An element $u$ of $S$ is said to be an _upper bound_ for $W$ if $w\leq u$ for each $w\in W$. An element $v$ of $S$ is said to be a _lower bound_ for $W$ if $v\leq w$ for each $w\in W$.

It is not necessarily true that every nonempty subset of a partially ordered set $S,\leq$ has an upper or a lower bound.

<span id="printed-page-6"></span>

<!-- source: PDF 15, printed 6 -->

**Example 2.** Let $S=\{x\mid 0<x<1\}$ be partially ordered by $\leq$. Then if $W=S$, $W$ has no upper bound, nor any lower bound. Suppose that $u$ is an upper bound for $W$; then $0<u<1$. Therefore

$$
0<u<(u+1)/2<1.
$$

Hence $(u+1)/2$ is an element of $W$ which is greater than $u$; thus $u$ could not be an upper bound for $W$. Similarly, $W$ has no lower bound.

The partially ordered set $R,\leq$ of real numbers has neither an upper nor a lower bound since, given any real number, we can find both a larger real number and a smaller real number.

Suppose that $W$ is a subset of a partially ordered set $S,\leq$. Then an element $U$ of $S$ is said to be a _least upper bound_ for $W$ if $U$ is an upper bound for $W$ and $U\leq u$ if $u$ is any upper bound of $W$. An element $L$ of $S$ is said to be the _greatest lower bound_ of $W$ if $L$ is a lower bound for $W$ and if $v$ is any lower bound for $W$, then $v\leq L$. The least upper bound and greatest lower bound for $W$ may be denoted by $\operatorname{lub}W$ and $\operatorname{glb}W$, respectively.

It is not always true that any nonempty subset of $S,\leq$ which has an upper bound has a least upper bound.

**Example 3.** Let $Q$ be the set of rational numbers partially ordered by $\leq$. Let $W$ be the set of rational numbers less than $\sqrt{2}$. Then 3 is an upper bound of $W$; but since $\sqrt{2}$ is an irrational number, it can be shown that $W$ has no least upper bound in $Q$. Note that $W$ does have a least upper bound in the full set of real numbers, namely, $\sqrt{2}$.

Every nonempty subset of the set of real numbers which has an upper bound (lower bound) has a least upper bound (greatest lower bound).

**Example 4.** Let $P(S),\subset$ be the partially ordered set described in Example 1. Suppose $\{U_i\}$, $i\in I$, is any collection of subsets of $S$. Then this collection has a least upper bound $\bigcup_I U_i$ and a greatest lower bound $\bigcap_I U_i$. Note that $\operatorname{glb}P(S)=\phi$ and $\operatorname{lub}P(S)=S$.

Let $S,\leq$ be any partially ordered set, and let $W\subset S$. An element $M$ of $W$ is said to be _maximal in $W$_ if $M\nleq w$ for each $w\in W-\{M\}$. An element $m$ of $W$ is said to be _minimal in $W$_ if $w\nleq m$ for each $w\in W-\{m\}$. An element $M$ of $S$ is said to be _maximal_ (_minimal_) if $M$ is maximal (minimal) in $S$.

**Example 5.** Let $R$ be the set of real numbers and $W=\{1,2,4\}$. Then 4 is maximal in $W$ and 1 is minimal in $W$. $R$ contains no maximal or minimal element.

Suppose $P(R)$ is the collection of subsets of $R$ partially ordered by $\subset$. Let $W=\{\{1\},\{2\},\{4\}\}$. Then each element of $W$ is both maximal and minimal in $W$. $R$ is a maximal element and $\phi$ is a minimal element of $P(R)$.

<span id="printed-page-7"></span>

<!-- source: PDF 16, printed 7 -->

If a subset $W$ of a partially ordered set $S,\leq$ is _totally ordered_ by the ordering on $W$ induced by $\leq$, then $W$ is said to be a _chain_ in $S$. That is, $W\subset S$ is a chain in $S$ if given any two elements $w$ and $w'$ of $W$, either $w\leq w'$, or $w'\leq w$.

**Example 6.** Let $S=\{1,2,3,4,5\}$ and $P(S)$ be the family of subsets of $S$ partially ordered by $\subset$. Then

$$
\{\{1\},\{1,2\},\{1,2,3\},\{1,2,3,4\},S\}
$$

is an example of a chain in $P(S)$.

One of the fundamental axioms in the theory of sets (and hence in mathematics) is the _axiom of choice_. As its name implies, the axiom of choice is a true axiom, assumed and not proved, although there are different ways in which it can be formulated. The axiom of choice properly so-called is stated as follows.

**The axiom of choice.** Suppose $\{S_i\}$, $i\in I$, is a family of nonempty sets. Then there is a function $f$ from $I$ into $\bigcup_I S_i$ such that $f(i)\in S_i$ for each $i\in I$.

The axiom of choice essentially says that given any collection of nonempty sets, it is possible to form a set by choosing one element from each set in the collection. It all sounds simple enough, but it is hardly simple; it has stemmed from and led to some of the deepest thinking in the foundations of mathematics. The purpose of this book is not to delve into this problem, however.

The axiom of choice has several apparently different but actually equivalent formulations. The particular formulation we will be interested in later in this book is known as _Zorn's lemma_.

**Zorn's lemma.** Suppose $S,\leq$ is a partially ordered set with the property that every chain in $S$ has an upper bound. Then $S$ contains a maximal element.

A partial ordering is an example of a special kind of relation that can be defined on a set. Another particularly important type of relation is an _equivalence relation_. The prototype for an equivalence relation is $=$, just as $\leq$ is the prototype for a partial ordering. Since ambiguity is likely to result if $=$ is used to denote an arbitrary equivalence relation, $E$ will be used instead. A relation $E$ on a set $S$ is said to be an _equivalence relation_ on $S$ if $E$ satisfies the following properties:

- **E1)** $sEs$ for any $s\in S$.
- **E2)** If $s$ and $s'$ are any elements of $S$ such that $sEs'$, then $s'Es$.
- **E3)** If $s,s'$, and $s''$ are any elements of $S$ such that $sEs'$ and $s'Es''$, then $sEs''$.

<span id="printed-page-8"></span>

<!-- source: PDF 17, printed 8 -->

Compare E1 through E3 with the properties of $=$. Note that the only difference between a partial ordering on $S$ and an equivalence relation on $S$ is that property P2 has been replaced by property E2.

**Example 7.** Let $T$ be the set of all plane triangles. Then _is similar to_ defines an equivalence relation on $T$. An equivalence relation on $T$ is also defined by _is congruent to_; still another equivalence relation on $T$ is defined by _has the same area as_.

The most important property of an equivalence relation is given in the following proposition.

**Proposition 2.** Let $S$ be any set. A _partition_ $\mathcal{P}$ of $S$ is any collection of nonempty subsets of $S$ such that each element of $S$ is contained in one and only one member of $\mathcal{P}$. Suppose $E$ is an equivalence relation on $S$. For each $s\in S$, set $\bar{s}=\{t\in S\mid sEt\}$. Then the collection of $\bar{s}$ for all $s\in S$ is a partition of $S$, called the _partition induced by $E$_. Moreover, given any partition $\mathcal{P}$ of $S$, there is an equivalence relation $E$ on $S$ such that $\mathcal{P}$ is the partition induced by $E$.

_Proof._ Suppose that $E$ is an equivalence relation on a set $S$. We must show that $\{\bar{s}\}$, $s\in S$, is a partition of $S$. Since $sEs$ for each $s\in S$ by E1, then $s\in\bar{s}$; hence each element of $S$ is contained in at least one member of $\{\bar{s}\}$, $s\in S$. We now must show that each element of $S$ is contained in only one member. Suppose that $s\in\bar{s}$ and $s\in\bar{t}$. Choose any $s'\in\bar{s}$. Then $sEs'$; also $tEs$ since $s\in\bar{t}$. By E3, $tEs$ and $sEs'$ implies $tEs'$; hence $s'\in\bar{t}$. Therefore $\bar{s}\subset\bar{t}$. A similar argument, however, shows that $\bar{t}\subset\bar{s}$, and hence $\bar{s}=\bar{t}$. Thus $\bar{s}$ is the only member of $\{\bar{s}\}$, $s\in S$, which contains $s$ for each $s\in S$. Therefore $\{\bar{s}\}$, $s\in S$, is a partition of $S$.

Suppose that $\mathcal{P}$ is a partition of $S$. Define a relation $E$ on $S$ by letting $sEs'$ if and only if $s$ and $s'$ are contained in the same member of $\mathcal{P}$ for any $s$ and $s'$ in $S$. It is left as an exercise to prove that $E$ is an equivalence relation on $S$. By definition of $E$, $\mathcal{P}$ is clearly the partition induced by $E$.

If $E$ is an equivalence relation on a set $S$, then if $sEs'$, $s$ and $s'$ are said to be _$E$-equivalent_, or simply, _equivalent_. The set of elements of $S$ which are equivalent to an element $s$ of $S$ is said to be the _$E$-equivalence class_ of $s$, or simply the _equivalence class_ of $s$. It is the collection of $E$-equivalence classes which forms the partition of $S$ induced by $E$.

**Example 8.** Let $f$ be a function from a set $S$ into a set $T$. Define $sEs'$ if $f(s)=f(s')$ for any $s$ and $s'$ in $S$. Then $E$ is an equivalence relation on $S$. Denote the set of $E$-equivalence classes by $S/E$; if $s\in S$, denote the equivalence class of $s$ by $\bar{s}$. We may associate with $f$ a function $\bar{f}:S/E\to T$, defined by $\bar{f}(\bar{s})=f(s)$ for any $\bar{s}\in S/E$. Since $\bar{s}'=\bar{s}$ if and only if $f(s)=f(s')$, $\bar{f}$ is well defined. Note that whereas $f$ may not have been one-one, $\bar{f}$ is one-one.

<span id="printed-page-9"></span>

<!-- source: PDF 18, printed 9, upper portion -->

## Exercises

1. Prove that $E$ in the second part of the proof of Proposition 2 is an equivalence relation on $S$.
2. The following refer to Example 8.

   a) Verify that $E$ is an equivalence relation on $S$.

   b) Prove that $\bar{f}$ is well defined, that is, single valued, and one-one.

3. Suppose that $N$ is the set of positive integers. Let $n\mid m$ denote _$n$ divides $m$_, that is, $m=nk$ for some positive integer $k$. Prove that $\mid$ defines a partial ordering on $N$. Does $N$ contain a maximal element (with respect to this partial ordering)? a minimal element?
4. Let $N,\mid$ be the partially ordered set described in Exercise 3.

   a) Prove that any two-element subset of $N$ has a greatest lower bound and a least upper bound.

   b) Which of the following subsets of $N$ are chains in $N$? Find a maximal and a minimal element, an upper and a lower bound, and a least upper bound for each subset:

   i) $\{1,2,4,6,8\}$, ii) $\{1,2,3,4,5\}$,

   iii) $\{3,6,9,12,15,18\}$, iv) $\{4,8,16,32,64,128\}$.

5. A subset $W$ of the set $Z$ of integers is said to be _closed under addition_ if given any elements $w$ and $w'$ of $W$, $w+w'\in W$. Prove that there is a maximal subset of $Z$ which is closed under addition and does not contain 9. Do this using Zorn's lemma.

[Continue to 1.3 Cardinality](./cardinality)

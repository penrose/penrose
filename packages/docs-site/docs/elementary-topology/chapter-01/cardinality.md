---
title: Cardinality — Elementary Topology
description: Section 1.3, printed pages 9–12, of Elementary Topology, second edition.
---

# 1.3 Cardinality

::: info Transcription note
Source: printed pages 9–12 (PDF pages 18–21). The unnumbered array diagram has a reviewed Penrose reproduction; its source location, labels, and visible enumeration path are retained below. The decimal table is also rendered by Penrose, with its values available as text.
:::

<span id="printed-page-9"></span>

<!-- source: PDF 18, printed 9, lower portion -->

Two sets $S$ and $T$ are said to have the _same number of elements_, or to have the _same cardinality_, if there is a one-one function $f$ from $S$ onto $T$. That is, $S$ and $T$ have the same cardinality if the elements of $S$ can be put into one-one correspondence with the elements of $T$.

A set $S$ is said to be _finite_ if $S$ has the same cardinality as $\phi$, or if there is a positive integer $n$ such that $S$ has the same cardinality as $\{1,2,\ldots,n\}$. Otherwise, $S$ is said to be _infinite_. Furthermore, a set $S$ is said to be _countable_ if $S$ has the same cardinality as a subset of $N$, the set of positive integers. Otherwise, $S$ is said to be _uncountable_. Thus any finite set is certainly countable.

**Proposition 3**

a) Any subset of a finite set $S$ is finite.

b) Any subset of any countable set $S$ is countable.

_Proof_

a) Since $S$ is finite, either $S=\phi$ or there is a positive integer $n$ such that $S$ has the same cardinality as $\{1,2,\ldots,n\}$. If $S=\phi$, then the only subset of $S$ is $\phi$, which is finite. Suppose that $S\neq\phi$. Then there

<span id="printed-page-10"></span>

<!-- source: PDF 19, printed 10 -->

is a one-one function $f$ from $S$ onto $\{1,2,\ldots,n\}$ for an appropriate $n$. Suppose $W\subset S$. If $W=\phi$, then $W$ is finite. If $W\neq\phi$, let $i_1,i_2,\ldots,i_m$ be the elements of $\{1,2,\ldots,n\}$ in the image of $W$. Then defining $g:W\to\{1,2,\ldots,m\}$ by $g(w)=j$, where $f(w)=i_j$, for each $w\in W$, we see that $W$ is finite.

The proof of (b) is left as an exercise.

**Proposition 4.** Let $\{A_n\}$, $n\in N$, be a countable collection of countable sets. Then $\bigcup_N A_n$ is also countable ($N$ represents the set of positive integers).

_Proof._ We may enumerate the elements of each of the $A_n$ in an array as shown.

<span id="countable-union-enumeration"></span>

<figure style="max-width:32rem; margin:1.75rem auto; background:white; padding:0.5rem"><img src="/elementary-topology/figures/figure-unnumbered-countable-union-enumeration.svg" alt="Rows A1 through A5, with elements a11 through a56 and arrows tracing the alternating diagonal enumeration, continuing into ellipses." /></figure>

The element $a_{nm}$ is the $m$th element of $A_n$. If we run out of elements in any set, i.e., if any of these sets are finite, we just put down $x$'s in the spot where an element should go.

We now must find a one-one function $f$ from $\bigcup_N A_n$ onto some subset of the set $N$ of positive integers. Set $f(a_{11})=1$, $f(a_{12})=2$, and $f(a_{21})=3$. In general, follow the path indicated in the diagram and correspond the $k$th element reached with $k$. Eventually every element of $\bigcup_N A_n$ will be reached; hence $\bigcup_N A_n$ can be put in one-one correspondence with a subset of $N$, and is therefore countable.

**Corollary 1.** If $A$ and $B$ are countable sets, then $A\times B$ is countable.

_Proof._ Let $A=\{a_1,a_2,a_3,\ldots\}$ and $B=\{b_1,b_2,b_3,\ldots\}$. Set

$$
A_n=\{(a_n,b)\mid b\in B\}\qquad\text{for each }n\in N.
$$

Then each $A_n$ has the same cardinality as $B$, and hence is countable. Therefore, by Proposition 4, $\bigcup_N A_n$ is a countable set. But then $A\times B=\bigcup_N A_n$, as the union of a countable number of countable sets, is countable.

<span id="printed-page-11"></span>

<!-- source: PDF 20, printed 11 -->

**Corollary 2.** The set $Q$ of rational numbers is countable.

_Proof._ If $q$ is any positive rational number, then we may consider $q$ as the quotient of two positive integers $m/n$, where the fraction $m/n$ is in lowest terms. Associate $q$ with the ordered pair $(m,n)$. We then see that the positive rational numbers may be associated with a subset of $N\times N$, where $N$ is the set of positive integers. But $N\times N$ is countable by Corollary 1. Therefore, by Proposition 3, the set of positive rational numbers is countable. The set of negative rational numbers, however, has the same cardinality as the set of positive rational numbers (corresponding $q$ with $-q$, where $q$ is any positive rational number), and hence the set of negative rational numbers is countable. But $Q$ is the union of $\{0\}$, the set of positive rational numbers, and the set of negative rational numbers, all three of which are countable sets; therefore, by Proposition 4, $Q$ is countable.

**Corollary 3.** The set $Z$ of integers is countable.

_Proof._ $Z$ is a subset of $Q$, the set of rationals, and hence is countable by Proposition 3.

Although we now have a goodly number of sets we know to be countable, we have not yet shown that any set is uncountable. The following example shows that the set of real numbers is uncountable.

**Example 9.** The set $S$ of unending decimals between 0 and 1 which contain only 0 or 1 as digits is uncountable. For proof, suppose that $S$ is countable. Then we can find a one-one correspondence between $S$ and the set $N$ of positive integers; hence we can make a table like the following, in which the first column gives a positive integer and the second column the element of $S$ associated with it by a suitable function $f$.

<span id="cantor-diagonal-table"></span>

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-unnumbered-cantor-diagonal-table.svg" alt="Indexed decimal expansions used in the diagonal argument for uncountability." />
<figcaption><a href="/docs/elementary-topology/reader?page=11">View the interactive table and its Substance program.</a></figcaption>
</figure>

<details>
<summary>Table values as text</summary>

|   $n$    |        $f(n)$        |
| :------: | :------------------: |
|    1     |   $0.011010\cdots$   |
|    2     |  $0.1110111\cdots$   |
|    3     |   $0.101101\cdots$   |
|    4     | $0.0000000111\cdots$ |
| $\vdots$ |       $\vdots$       |

</details>

We now form an element $.x_1x_2x_3x_4\cdots$ of $S$ as follows: If the first digit of $f(1)$ is 0, let $x_1=1$, and if the first digit of $f(1)$ is 1, let $x_1=0$. Similarly, if the second digit of $f(2)$ is 0, let $x_2=1$, and if the second digit of $f(2)$ is 1, let $x_2=0$. In general, if the $n$th digit of $f(n)$ is 0, let $x_n$, the $n$th digit of our new element of $S$, be 1, and if the $n$th digit of $f(n)$ is 1, let $x_n=0$. Then $.x_1x_2x_3\cdots$ could not be $f(n)$ for any positive integer $n$, since $.x_1x_2x_3\cdots$ differs from each $f(n)$ at least in the $n$th digit because of the way it has been constructed. Hence $f$ could not be onto, and $S$ is therefore uncountable.

<span id="printed-page-12"></span>

<!-- source: PDF 21, printed 12 -->

But $S$ is a subset of $R$, the set of real numbers. If $R$ were countable, then, by Proposition 3, $S$ would also be countable; therefore $R$ is uncountable.

We have only gone as far in our discussion of cardinality as it was felt necessary to go in order that the reader understand the contents of this book. The discussion has been somewhat informal and much has been left unsaid. For a more complete discussion of cardinality and cardinal numbers, the following texts are recommended.

1. J. L. Kelley, _General Topology_, Van Nostrand, New York, 1955. The appendix to Kelley gives a concise axiomatic treatment of set theory and ordinal and cardinal numbers. It may be a bit too concise for the reader.
2. G. Birkhoff and S. MacLane, _A Survey of Modern Algebra_, Macmillan, New York, 1953. Chapter XII gives a nice introduction to cardinal numbers and their arithmetic.
3. E. Kamke, _Theory of Sets_, Dover, New York, 1950. This book is one of the classics in set theory and is a must in the library of any serious mathematician.

## Exercises

1. Let $\mathfrak{C}$ be the class of all sets. (Technically, the collection of all sets is not a set, so in such cases we use some word like _class_.) Prove that _has the same cardinality as_ defines an equivalence relation on $\mathfrak{C}$. An equivalence class is called a _cardinal number_.
2. Prove (b) of Proposition 3.
3. Prove that no finite set $S$ has the same cardinality as one of its proper subsets $W$. (A subset $W$ of $S$ is said to be _proper_ if $W\neq S$.) Does this remain true if $S$ is infinite? Prove that any two infinite subsets of $N$, the set of positive integers, have the same cardinality.
4. The cardinality of a set $S$ is said to be _strictly greater_ than the cardinality of a set $T$ if there is a subset $W$ of $S$ which has the same cardinality as $T$, but no subset of $T$ which has the same cardinality as $S$.

   a) Prove that the cardinality of any uncountable set is strictly greater than the cardinality of any countable set.

   b) Let $S$ be any set and let $P(S)$ denote the collection of subsets of $S$. Prove that the cardinality of $P(S)$ is strictly greater than the cardinality of $S$.

   c) Show that given any set whatsoever, there is a set of strictly greater cardinality.

5. Prove: The set $R$ of real numbers has the same cardinality as a subset of the set $P(N)$ of all subsets of the positive integers. Prove that $P(N)$ has the same cardinality as a subset of $R$. It can then be shown that $R$ and $P(N)$ have the same cardinality.

[Continue to 1.4 Groups](./groups)

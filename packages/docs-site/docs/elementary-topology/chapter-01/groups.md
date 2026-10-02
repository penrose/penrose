---
title: Groups — Elementary Topology
description: The available text of section 1.4, printed pages 14–15, of Elementary Topology, second edition.
---

# 1.4 Groups

::: warning Source gap: printed page 13
Printed page 13, including the beginning of §1.4, is absent from the supplied scan. No definitions or other text from that page have been reconstructed. The available text resumes on printed page 14 with Example 9; the source numbering is retained.
:::

<span id="printed-page-14"></span>

<!-- source: PDF 22, printed 14 -->

**Example 9.** Let $R$ be the set of real numbers. Then $R,+$ is a group with identity 0. $R$ is not a group with respect to multiplication, since 0 has no inverse with respect to multiplication; but $R-\{0\}$ is a group with multiplication as the operation and 1 as the identity.

**Example 10.** There are groups which contain only a finite number of elements. The table below gives the “multiplication table” for a group of only four elements:

<span id="four-element-group-table"></span>

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-unnumbered-four-element-group-table.svg" alt="Operation table for the four-element group in Example 10." />
<figcaption><a href="/docs/elementary-topology/reader?page=14">View the interactive table and its Substance program.</a></figcaption>
</figure>

<details>
<summary>Table values as text</summary>

| $\#$  | $s_1$ | $s_2$ | $s_3$ | $s_4$ |
| :---: | :---: | :---: | :---: | :---: |
| $s_1$ | $s_1$ | $s_2$ | $s_3$ | $s_4$ |
| $s_2$ | $s_2$ | $s_1$ | $s_4$ | $s_3$ |
| $s_3$ | $s_3$ | $s_4$ | $s_1$ | $s_2$ |
| $s_4$ | $s_4$ | $s_3$ | $s_2$ | $s_1$ |

</details>

It would actually take a great deal of computation to verify directly that this is indeed the operation table for a group; therefore, if the reader does not immediately recognize this group, he will more or less have to accept its being a group on faith. Note that the identity of this group is $s_1$ and that each element of the group is its own inverse.

Suppose that $S,\#$ and $T,\$$ are groups. There may be many functions from $S$ into $T$, but perhaps only a few of these are related in any way to the group structures of $S$ and $T$. When studying groups, however, we wish to consider functions which somehow respect the operations of the groups; such functions are called homomorphisms. More formally, a function $f:S\to T$ is called a _homomorphism_ if

$$
f(s_1\mathbin{\#}s_2)=f(s_1)\mathbin{\$}f(s_2)
$$

for any elements $s_1$ and $s_2$ of $S$. If $f$ is a one-one and onto function as well as being a homomorphism, then $f$ is said to be an _isomorphism_, and the groups $S,\#$ and $T,\$$ are said to be _isomorphic_. Isomorphic groups have essentially the same group properties.

If $S,\#$ and $T,\$$ are any groups, then we can define an operation $\&$ on $S\times T$ as follows: If $(s,t)$ and $(s',t')$ are any elements of $S\times T$, define

$$
(s,t)\mathbin{\&}(s',t')=(s\mathbin{\#}s',t\mathbin{\$}t').
$$

We call the group thus formed the _direct sum_ of $S,\#$ and $T,\$$ (see Exercise 5). We denote the direct sum of $S,\#$ and $T,\$$ by $S\oplus T$.

<span id="printed-page-15"></span>

<!-- source: PDF 23, printed 15 -->

Again, we have only set forth as much about groups as will be required to understand the text. For a more complete treatment of the theory of groups, the following books are suggested.

1. G. Birkhoff and S. MacLane, _A Survey of Modern Algebra_, Macmillan, New York, 1953. Chapter VI is a good introduction to groups. Chapters I and II can also be used as a reference on the structure of the real numbers.
2. W. Ledermann, _Introduction to the Theory of Finite Groups_, Oliver and Boyd, London, 1961. This is another excellent book that should be in anyone's mathematics library.

## Exercises

1. Let $S$ be any set. Prove that the set of one-one functions from $S$ onto $S$ is a group with composition as the group operation. Suppose that $f$ and $g$ are any one-one functions from $S$ onto $S$. Is it necessarily true that $f\circ g=g\circ f$?
2. Suppose $f$ to be a homomorphism from the group $S,\#$ into the group $T,\$$. Prove that $f(S),\$$ is a group contained in the group $T,\$$. (If $S,\#$ is any group and $W$ is a subset of $S$ such that $W,\#$ is also a group, then $W,\#$ is said to be a _subgroup_ of $S,\#$.) Let $k'$ be the identity of $T,\$$ with respect to $\$$. Prove that $f^{-1}(k')$ is a subgroup of $S,\#$.
3. Let $Z,+$ be the additive group of integers. Prove that any subgroup of $Z,+$ consists of all the multiples of some fixed integer $n$; that is, if $W$ is a subgroup of $Z,+$, then there is an integer $n$ such that $W=\{nz\mid z\in Z\}$.
4. Suppose $S,\#$ is a group and $\{T_i\}$, $i\in I$, is a family of subgroups of $S$. Prove that $\bigcap_I T_i$ is a subgroup of $S$.
5. Prove that if $S,\#$ and $T,\$$ are groups, then $S\oplus T$, the direct sum of $S$ and $T$, is also a group. Prove that $S,\#$ and $T,\$$ are each isomorphic to some subgroup $S\oplus T$. Find a homomorphism $f$ from $S\oplus T$ onto $S,\#$.
6. Suppose a set $S$ with operation $\#$ has an identity $k$ with respect to $\#$. Prove that $k$ is the only identity in $S$ with respect to $\#$. Prove that if $\#$ is associative and $s\in S$ has an inverse $t$, then $t$ is the only inverse of $s$ in $S$.

::: info Chapter boundary
The next supplied page, printed page 16 (PDF page 24), begins Chapter 2, “Metric Spaces.”
:::

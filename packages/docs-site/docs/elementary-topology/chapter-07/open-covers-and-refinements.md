---
title: Open Covers and Refinements — Elementary Topology
description: The available text of section 7.1 of the supplied second-edition scan.
---

# 7.1 Open Covers and Refinements

::: info Transcription note
Source: printed pages 142–144 (PDF pages 136–138). Printed page 144 also begins §7.2, transcribed separately. The source's historical compactness discussion and its original footnote are retained together.
:::

<span id="printed-page-142"></span>

<!-- Source: PDF page 136, printed page 142. -->

Some of the most important aspects of certain types of topological spaces can be expressed as _covering properties_. The nice definitions given in this chapter were not always used in the study of topology. As is usually the case with a new discipline, those who pioneered in topology thought certain properties were important for a space to have. The best means of expressing those properties, best from the point of view of most elegant and most workable, were only developed from years of experience. The student should be sophisticated enough to realize that areas of mathematical study are not born full-grown but, as with human infants, require a period of growth of many years before reaching maturity.

_Compactness_, the most important covering property, was once defined as follows: A space $X,\tau$ is _compact_ if for every infinite subset $A\subset X$, there is at least one $y\in X$ such that given any two neighborhoods $U$ and $U'$ of $y$, $U\cap A$ and $U'\cap A$ have the same cardinality. Even the novice in topology will realize that this is a rather cumbersome definition. As more became known about the property that this definition was intended to convey, equivalent expressions of it became known. Compactness is now defined as a covering property.<sup><a href="#compactness-note">\*</a></sup> Certain other concepts valuable in the study of topological spaces can also be best expressed as covering properties.

A _cover_ of a space $X,\tau$ is exactly what its name implies, a collection of subsets of $X$ which cover $X$, that is, whose union is $X$. Usually, however, we wish the members of the cover to be sets of a particular form, generally, open sets. We therefore state the following.

**Definition 1.** Let $X,\tau$ be a topological space. An _open cover_ of $X$ is a collection $\{U_i\}$, $i\in I$, of open subsets of $X$ such that

$$
\bigcup_I U_i=X.
$$

<aside id="compactness-note" class="source-footnote">
* The old definition of compactness, however, is actually not equivalent to the definition of compactness as a covering property.
</aside>

Let $\{U_i\}$, $i\in I$, be an open cover of the space $X,\tau$. A collection <span id="printed-page-143"></span><!-- Source: PDF page 137, printed page 143. --> $\{V_j\}$, $j\in J$, is said to be an _open subcover_ of $\{U_i\}$, $i\in I$, if

$$
\{V_j\mid j\in J\}\subset\{U_i\mid i\in I\}
$$

(that is, each $V_j$ is a $U_i$) and $\{V_j\}$, $j\in J$, is itself an open cover of $X$. The collection $\{V_j\}$, $j\in J$, is said to be a _refinement_ of $\{U_i\}$, $i\in I$, if $\{V_j\}$, $j\in J$, is an open cover, and for each $V_j$, there is $U_i$ such that $V_j\subset U_i$.

Note that an open subcover is a refinement, but a refinement is not necessarily an open subcover.

**Example 1.** Let $R$ be the set of real numbers with the topology induced by the absolute value metric. Then

$$
\{N(x,4)\mid x\in R\},
$$

that is, the set of all 4-neighborhoods in $R$, is an open cover of $R$. The set

$$
\{N(n,4)\mid n\text{ is an integer}\}
$$

is an open subcover of $\{N(x,4)\mid x\in R\}$. The set

$$
\{N(x,1)\mid x\in R\}
$$

is a refinement of $\{N(x,4)\mid x\in R\}$, since every 1-neighborhood is contained in some 4-neighborhood. In fact, every 1-neighborhood in $R$ is contained in a 4-neighborhood of an integer; hence $\{N(x,1)\mid x\in R\}$ is a refinement of $\{N(n,4)\mid n\text{ is an integer}\}$, even though the cardinality of $\{N(x,1)\mid x\in R\}$ is greater than that of $\{N(n,4)\mid n\text{ an integer}\}$.

**Example 2.** Let $X$ be a set with the discrete topology. Then $\{\{x\}\mid x\in X\}$ is an open cover of $X$. Moreover, this open cover has no proper subcover, nor any proper refinement. If $X$ has the trivial topology, then the only open covers of $X$ are $\{X,\phi\}$ and $\{X\}$. (See Exercise 1.)

**Example 3.** Let $N$ be the set of positive integers with the topology determined by calling a subset $U$ of $N$ open if $U$ contains all but at most finitely many elements of $N$. Let $\{U_i\}$, $i\in I$, be any open cover of $N$. Pick any $U_i$. Then $U_i$ contains all but at most finitely many of the positive integers; say $U_i$ excludes $n_1,\ldots,n_p$. Since $\{U_i\}$, $i\in I$, is an open cover, every element of $N$ is in at least one of the $U_i$, and hence there are at most $p$ other members of $\{U_i\}$, $i\in I$, say $U_{i_1},\ldots,U_{i_p}$ such that

$$
N=U_i\cup U_{i_1}\cup\cdots\cup U_{i_p}.
$$

Thus $\{U_i,U_{i_1},\ldots,U_{i_p}\}$ is a finite open subcover of $\{U_i\}$, $i\in I$. We therefore see that every open cover of $N$ (with the prescribed topology) has a finite open subcover.

<span id="printed-page-144"></span>

<!-- Source: PDF page 138, printed page 144; section 7.1 fragment. -->

## Exercises

1. Prove the assertions made in Example 2.

2. Let $R^2$ be the coordinate plane with the topology induced by the Pythagorean metric. Which of the following are open subcovers of

   $$
   \{N((x,y),1)\mid(x,y)\in R^2\}?
   $$

   Which are refinements of

   $$
   \{N((x,y),3)\mid(x,y)\in R^2\}?
   $$

   In the event a collection is not a subcover, or not a refinement, explain which properties are lacking.

   a) $\{N((m,n),\frac12)\mid m\text{ and }n\text{ are integers}\}$

   b) $\{N((0,0),p)\mid p\text{ a positive real number}\}$

   c) $\{N((x,y),1)\mid x\text{ and }y\text{ are rational}\}$

   d) $\{N((x,y),\frac19)\mid x\text{ and }y\text{ are rational}\}$

   e) the family of all sets of the form $\{(x,y)\mid |x-a|+|y-b|<1\}$, where $(a,b)$ is any point of $R^2$

   f) the family of all subsets of $R^2$

3. Prove that $R^2$ with the Pythagorean topology has a countable cover consisting of $p$-neighborhoods. Prove that the set of real numbers with the order topology has a countable cover consisting of intervals of the form $(-p,p)$, where $p>0$.

4. Suppose the open interval $(0,1)$ is given the absolute value topology. Form $\{U_n\}$, $n=1,2,3,\ldots$, where $U_n=(1/(n+1),1)$. Prove that $\{U_n\}$, $n\in N$, is an open cover of $(0,1)$. Show that no finite number of the $U_n$ cover $(0,1)$, even though any finite number of the $U_n$ may be omitted and what remains still give an open cover of $(0,1)$.

5. Suppose $\tau$ is a topology on the set $N$ of positive integers with the property that any open cover of $N$ has an open subcover which contains at most two elements. Describe all possibilities for $\tau$.

<style scoped>
.source-footnote {
  font-size: 0.875rem;
  border-top: 1px solid var(--vp-c-divider);
  padding-top: 0.6rem;
  margin: 1.25rem 0;
}
</style>

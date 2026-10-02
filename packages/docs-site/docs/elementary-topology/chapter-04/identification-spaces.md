---
title: Identification Spaces — Elementary Topology
description: Section 4.5 of the supplied second-edition scan.
---

# 4.5 Identification Spaces

::: info Transcription note
Source: printed pages 79–84 (PDF pages 80–85). The equivalence-class notation $\bar{x}$ and induced mappings $\bar{g}$ and $\bar{h}$ are retained. The source spelling “equivalance” on printed page 82 and the printed formulas are retained. Printed pages 79 and 84 are split at section boundaries.
:::

<span id="printed-page-79"></span>

<!-- Source: PDF page 80, printed page 79; section 4.5 fragment. -->

If $X$ is any set and $R$ is an equivalence relation on $X$, then $R$ determines a partition of $X$ into $R$-equivalence classes. We will denote the set of equivalence classes by $X/R$. If $X$ also has a topology $\tau$, we might inquire if $\tau$ can be used in a natural way to give a topology on $X/R$.

We note that there is a natural function $f$ from $X$ to $X/R$ defined by $f(x)=\bar{x}$, where $x$ is any element of $X$ and $\bar{x}$ is the $R$-equivalence class of $x$. If $X$ is a topological space, it is reasonable to want a topology on $X/R$ which would at least make $f$ continuous. Of course, if $X/R$ is given the trivial topology, then $f$ is continuous. But the trivial topology is pretty much what its name implies, trivial. Furthermore, the trivial topology on $X/R$ is not necessarily related to $\tau$, and we are looking for a <span id="printed-page-80"></span><!-- Source: PDF page 81, printed page 80. --> topology which is derived from $\tau$. We know that the function $f$ will be continuous if and only if given any open set $U$ of $X/R$, $f^{-1}(U)$ is open in $X$. We will use this fact to define a topology on $X/R$; that is, we will say that a subset $U$ of $X/R$ will be open if $f^{-1}(U)$ is open in $X$.

**Definition 5.** Suppose $X,\tau$ is a topological space and $R$ is an equivalence relation on $X$. Let $X/R$ denote the set of $R$-equivalence classes. Define the function $f$ from $X$ to $X/R$ by $f(x)=\bar{x}$, where $x$ is any element of $X$ and $\bar{x}$ is the $R$-equivalence class of $x$. Then $f$ is called the _identification mapping_ from $X$ to $X/R$. Define a subset $U$ of $X/R$ to be open if $f^{-1}(U)$ is open in $X$. The topology thus obtained on $X/R$ (Proposition 16) is called the _identification topology_ on $X/R$. (Some topologists refer to this topology as the _quotient topology_, and of $X/R$ as a _quotient space_.)

**Proposition 16.** The collection of open sets of $X/R$ actually forms a topology for $X/R$, that is, the identification topology is really a topology.

_Proof._ We verify that the collection of open sets in $X/R$ satisfies Definition 1, Chapter 3.

i) $f^{-1}(\phi)=\phi$ and $f^{-1}(X/R)=X$. Since $X$ and $\phi$ are both open subsets of $X$, $\phi$ and $X/R$ are open subsets of $X/R$.

ii) Let $U$ and $V$ be open subsets of $X/R$. Then $f^{-1}(U)$ and $f^{-1}(V)$ are open subsets of $X$. Now $f^{-1}(U)\cap f^{-1}(V)$ is also an open subset of $X$. But

$$
f^{-1}(U)\cap f^{-1}(V)=f^{-1}(U\cap V).
$$

Therefore $U\cap V$ is also an open subset of $X/R$.

iii) Suppose $\{U_i\}$, $i\in I$, is a family of open subsets of $X/R$. Then $f^{-1}(U_i)$ is open in $X$ for each $i\in I$, and thus $\bigcup_I f^{-1}(U_i)$ is an open subset of $X$. But since

$$
\bigcup_I f^{-1}(U_i)=f^{-1}\left(\bigcup_I U_i\right),
$$

$\bigcup_I U_i$ is an open subset of $X/R$. Therefore the collection of open subsets of $X/R$ forms a topology on $X/R$.

**Proposition 17.** Let $X,\tau$ be a topological space, let $R$ be an equivalence relation on $X$, and suppose that $X/R$ has the identification topology $\tau'$. Then the identification map

$$
f:X,\tau\to X/R,\tau'
$$

is continuous. Furthermore, $\tau'$ is the finest topology on $X/R$ for which $f$ is continuous.

<span id="printed-page-81"></span>

<!-- Source: PDF page 82, printed page 81. -->

_Proof._ By definition of the identification topology, $f^{-1}(U)$ is an open subset of $X$ whenever $U$ is an open subset of $X/R$. The identification topology has been specifically defined so as to make $f$ continuous. Suppose $\tau''$ is a topology on $X/R$ which is strictly finer than $\tau'$. Then there is $U\in\tau''$ such that $U\notin\tau'$. Then $f^{-1}(U)$ is not an open subset of $X$ (if it were, $U$ would also be in $\tau'$); hence $f$ is not a continuous function from $X,\tau$ onto $X/R,\tau''$.

The identification topology is so called because it may be viewed in the following manner: Let $X,\tau$ be a space, and let $R$ be an equivalence relation on $X$. Then we obtain $X/R$ by identifying all $R$-equivalent elements of $X$ with one another, that is, we make an equivalence class a point of a new set. We put a topology on $X/R$ by defining a subset $U$ of $X/R$ to be open if all elements of $X$ contained in all the members of $U$ form an open subset of $X$.

**Example 14.** Let the closed interval $[0,1]$ have the usual (absolute value) topology. An equivalence relation on any set can be specified either by giving the equivalence classes, that is, a partition of the set, or by defining the relation. For $[0,1]$, we will let $0$ be equivalent to $1$, and every other element of $[0,1]$ be equivalent only to itself. Then the equivalence classes are $\{0,1\}$ and $\{x\}$, for $x\ne 0,1$. By identifying $0$ and $1$, we obtain a circle (Fig. 4.3). We have joined the endpoints of $[0,1]$ by making the endpoints a single point of a new topological space, which is a simple closed curve. We therefore have a continuous function from $[0,1]$ onto a circle.

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-4.3.svg" alt="Figure 4.3: Identifying the endpoints of an interval produces a circle." /><figcaption>Figure 4.3</figcaption></figure>

**Example 15.** Suppose $X$ is any set and $Y,\tau'$ is a topological space. Let $g$ be a function from $X$ to $Y$. Suppose we want to find a topology on $X$ which will make $g$ continuous. Of course, the discrete topology will do, but as with the trivial topology, this possibility is not of much interest. Since $g$ will be continuous if and only if given any open set $U$ of $Y$, $g^{-1}(U)$ is open in $X$, we will define a subset $V$ of $X$ to be open if there is an open subset $U$ of $Y$ such that $V=g^{-1}(U)$. Then the family of open subsets of $X$ forms a topology $\tau$ on $X$; moreover, this topology is the coarsest topology which makes $g$ continuous (Section 4.3, Exercise 4).

Define an equivalence relation $R$ on $X$ by letting $x\mathrel{R}x'$ if $g(x)=g(x')$ for any $x$ and $x'$ in $X$. If $\bar{x}$ is the $R$-equivalence class of $x$, then there is a natural function from $X/R$ into $Y$ defined by $\bar{g}(\bar{x})=g(x)$. The function $\bar{g}$ is well-defined, for if $\bar{x}=\bar{x}'$, then

$$
\bar{g}(\bar{x})=g(x)=g(x')=\bar{g}(\bar{x}').
$$

<span id="printed-page-82"></span>

<!-- Source: PDF page 83, printed page 82. -->

Let $f$ be the identification mapping from $X$ to $X/R$. Then if $g$ is onto and $X/R$ is given the identification topology $\tau''$, $\bar{g}$ is a homeomorphism from $X/R,\tau''$ onto $Y,\tau'$. This is proved as follows: Since $g$ is onto, $\bar{g}$ is onto. Suppose $\bar{g}(\bar{x}_1)=\bar{g}(\bar{x}_2)$. Then $g(x_1)=g(x_2)$. Therefore $\bar{x}_1=\bar{x}_2$, and hence $\bar{g}$ is one-one.

It remains to show that $\bar{g}$ and $\bar{g}^{-1}$ are continuous. Suppose $U$ is any open subset of $Y$. Then

$$
f^{-1}(\bar{g}^{-1}(U))=g^{-1}(U),
$$

which by definition is an open subset of $X$. Since $f^{-1}(\bar{g}^{-1}(U))$ is an open subset of $X$, and since $X/R$ has the identification topology, $\bar{g}^{-1}(U)$ is open in $X/R$. Therefore $\bar{g}$ is continuous. Assume that $V$ is any open subset of $X/R$. Then $f^{-1}(V)$ is an open subset of $X$. Hence

$$
f^{-1}(V)=g^{-1}(U),
$$

where $U$ is some open subset of $Y$, and then

$$
\bar{g}(V)=g(g^{-1}(U))=U
$$

is an open subset of $Y$. Since $\bar{g}=(\bar{g}^{-1})^{-1}$, $(\bar{g}^{-1})^{-1}(V)$ is open in $Y$; hence $\bar{g}^{-1}$ is continuous. Therefore $\bar{g}$ is a homeomorphism.

If the reader is familiar with some group theory, he may find it instructive to recall the relationship between quotient groups and homomorphisms. If $G$ and $G'$ are groups and $h$ is a homomorphism from $G$ onto $G'$, then $G'$ is isomorphic to the quotient group $G/K$, where $K$ is the kernel of $h$. The quotient group $G/K$ is nothing but the set of equivalence classes of the relation $R$, defined by $g\mathrel{R}g'$ if $h(g)=h(g')$. There is a function $\bar{h}$ from the set of $R$-equivalence classes $G/K$ onto $G'$, defined by $\bar{h}(\bar{g})=h(g)$ where $\bar{g}$ is the equivalence class of $g\in G$. If $h$ is onto, then $\bar{h}$ is one-one and onto. An operation is then defined on $G/K$ by means of the operation on $G$ such that $\bar{h}$ and $\bar{h}^{-1}$ are homomorphisms; hence $\bar{h}$ is an isomorphism.

This procedure is quite important in mathematics. That is, starting with a function $g$ onto a structured set $Y$ from an unstructured set $X$, we might wish to find a structure on $X$ so that $g$ becomes a structure-preserving function with the structure on $X$ derived from the structure on $Y$ in a natural way. It might be that $X$ already has a structure and that $g$ is structure preserving, hence making it unnecessary to define another structure on $X$. This is the case for example, if $X$ and $Y$ are groups and $g$ is a homomorphism. In any event, taking the equivalance classes determined by $g$ [that is, $x$ is equivalent to $x'$ if $g(x)=g(x')$], we have a function $\bar{g}$ from the set of equivalence classes onto $Y$ which is one-one. We also have the identification mapping from $X$ onto the set of equivalence <span id="printed-page-83"></span><!-- Source: PDF page 84, printed page 83. --> classes. We then find a structure on the set of equivalence classes so that both the identification mapping, $\bar{g}$, and $\bar{g}^{-1}$ are structure preserving. Note the close parallel in this respect between quotient groups and homomorphisms in group theory, and identification spaces and continuous functions in topology (also see Exercise 2).

## Exercises

1. Verify that the identification space obtained from $[0,1]$ in Example 14 is really homeomorphic to a circle. An actual homeomorphism might be given by “wrapping” $[0,1]$ around a circle in $R^2$ of radius $1/2\pi$.

2. A fundamental theorem of group homomorphisms states: There is a homomorphism from the group $G$ onto the group $G'$ if and only if there is a normal subgroup $K$ of $G$ such that $G'$ is isomorphic to the quotient group $G/K$. Provide an example to show that the following analogous statement about topological spaces is not true: There is a continuous function $g$ from the space $X,\tau$ onto the space $Y,\tau'$ if and only if there is an equivalence relation $R$ on $X$ such that the identification space $X/R$ is homeomorphic to $Y,\tau'$. Explain why this statement fails to be true. Is it true if the phrase “if and” is omitted? Is it true if the phrase “and only if” is omitted?

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-4.4.svg" alt="Figure 4.4: A rectangle with the two marked edges in one equivalence class." /><figcaption>Figure 4.4</figcaption></figure>

**Fig. 4.4**

$$
\begin{gathered}
\bigl\{\{x\mid x\in\overline{AB}\cup\overline{CD}\}\bigr\}\cup\\
\bigl\{\{x\mid x=x\}\mid x\notin\overline{AB}\cup\overline{CD}\bigr\}
\end{gathered}
$$

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-4.5.svg" alt="Figure 4.5: A disk whose opposite boundary points are paired." /><figcaption>Figure 4.5</figcaption></figure>

**Fig. 4.5**

$$
\begin{gathered}
\bigl\{\bar{P}=\{w\mid w\text{ is diagonally opposite}\\
P,\text{ or }w=P\},\text{ if }P\text{ is on the circumference}\bigr\}\cup\\
\bigl\{\{P\mid P=P\},\text{ if }P\text{ is not on the circumference}\bigr\}
\end{gathered}
$$

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-4.6.svg" alt="Figure 4.6: The plane partitioned by integral differences between coordinates." /><figcaption>Figure 4.6</figcaption></figure>

**Fig. 4.6**

$$
\bigl\{\{(x,y)\mid x=y+m,\text{ where }m\text{ is an integer}\}\bigr\}
$$

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-4.7.svg" alt="Figure 4.7: A polygon with its perimeter in one equivalence class." /><figcaption>Figure 4.7</figcaption></figure>

**Fig. 4.7**

$$
\begin{gathered}
\bigl\{\{z\mid z\text{ is on the perimeter of }C\}\bigr\}\cup\\
\bigl\{\{z\mid z=z\}\mid z\text{ is not on the perimeter of }C\bigr\}
\end{gathered}
$$

3. Each of Figs. 4.4 through 4.7 is to be considered as a subspace of the plane $R^2$ with the usual Pythagorean metric topology. Under each figure is given a partition of the set which the figure represents. Draw a picture of the identification space determined by each partition.

<span id="printed-page-84"></span>

<!-- Source: PDF page 85, printed page 84; section 4.5 fragment. -->

4. Let $g$ be a function from a space $X,\tau$ onto a set $Y$. Define a subset $U$ of $Y$ to be open if $g^{-1}(U)$ is an open subset of $X$. It was shown in Section 4.3, Exercise 3 that the open subsets of $Y$ then form a topology $\tau'$ on $Y$ and that $g:X,\tau\to Y,\tau'$ is continuous. Prove that $Y,\tau'$ is homeomorphic to the identification space $X/R$, where $R$ is the equivalence relation associated with the function $g$.

5. Find a quotient space of $R^2$ homeomorphic to each of the following.

   a) a rectangle with its interior

   b) a sphere

   c) a straight line

<style>
.topology-chapter-figure { max-width: 28rem; margin: 1.75rem auto; text-align: center; }
.topology-chapter-figure img { width: 100%; background: white; }
.topology-chapter-figure figcaption { font-family: Georgia, serif; font-size: 0.875rem; margin-top: 0.4rem; }
.topology-chapter-figure-pair { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 1.5rem; margin: 1.75rem 0; }
.topology-chapter-figure-pair .topology-chapter-figure { margin: 0; }
@media (max-width: 480px) { .topology-chapter-figure-pair { grid-template-columns: 1fr; } }
</style>

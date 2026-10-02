---
title: “Distance” Between Two Sets — Elementary Topology
description: The available text of section 2.7, printed pages 34–39, of Elementary Topology, second edition.
---

# 2.7 “Distance” Between Two Sets

::: info Transcription note
Source: printed pages 34–37 and 39 (PDF pages 40–44). Printed page 38 is missing; its prose and exercise openings have not been reconstructed. The lowercase $d$ in $d(0,A)=0$ in Example 20 is retained as printed. Printed page 40 begins Chapter 3 and is not included here.
:::

<span id="printed-page-34"></span>

<!-- source: PDF 40, printed 34; section 2.7 fragment -->

**Definition 7.** Let $X,D$ be any metric space. Suppose that $x\in X$ and $A\subset X$. Then define

$$
D(x,A)=\text{greatest lower bound }\{D(x,a)\mid a\in A\}.
$$

If $A$ and $B$ are subsets of $X$, define

$$
D(A,B)=\text{greatest lower bound }\{D(a,b)\mid a\in A,b\in B\}.
$$

The following equalities follow immediately from the definition:

$$
D(x,A)=D(\{x\},A)
$$

and

$$
D(A,B)=\operatorname{glb}\{D(a,B)\mid a\in A\}=\operatorname{glb}\{D(A,b)\mid b\in B\}.
$$

Note, however, that $D$ is not a metric for the set of all subsets of $X$. It is quite possible, for example, to have sets $W$ and $Y$ such that $D(W,Y)=0$, but $W\cap Y=\phi$ (a contradiction to Definition 1iii) as we see from the following.

**Example 18.** Let $R^2$ be the plane with the Pythagorean metric $D$. Set

$$
Y=\{(x,y)\mid x^2+y^2<1\}\quad\text{and}\quad W=\{(1,0)\}
$$

<div class="topology-chapter-figure-pair">
<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-2.17.svg" alt="Figure 2.17: the open unit disk Y and the boundary singleton sets Z and W." /><figcaption>Figure 2.17</figcaption></figure>
<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-2.18.svg" alt="Figure 2.18: neighborhoods separating a closed set F from a point x outside F." /><figcaption>Figure 2.18</figcaption></figure>
</div>

<span id="printed-page-35"></span>

<!-- source: PDF 41, printed 35 -->

(Fig. 2.17). Then $D(Y,W)=0$, since there are points of $Y$ arbitrarily close to $(1,0)$, but $(1,0)$ is not a point of $Y$. If we let $Z=\{(-1,0)\}$, then

$$
D(Z,W)=2>D(W,Y)+D(Y,Z)=0+0=0.
$$

Therefore the “metric” $D$ on the set of all subsets of $R^2$ does not even satisfy the triangle inequality.

Note too that the distance between a set and any of its nonempty subsets is always 0.

Even though the “metric” for the subsets of a metric space is not really a metric according to Definition 1, it is still of great use in helping us describe the properties of metric spaces.

**Proposition 11.** Let $X,D$ be a metric space. A subset $F$ of $X$ is closed if and only if given any point $x$ in $X-F$, $D(x,F)\neq0$.

_Proof._ If $F$ is a closed subset of $X$, then $X-F$ is open. Therefore, given any $x\in X-F$, there is a positive number $\rho$ such that $N(x,\rho)\subset X-F$. But then $D(x,F)\geq\rho$; hence $D(x,F)\neq0$.

Suppose, on the other hand, that given any point $x$ in $X-F$, $D(x,F)\neq0$. Then setting $\rho=D(x,F)$, we have $N(x,\rho)\subset X-F$. That is, for each $x\in X-F$, we have a positive number $\rho$ such that

$$
N(x,\rho)\subset X-F,
$$

which is to say that $X-F$ is open. Therefore $F=X-(X-F)$ is closed.

**Proposition 12.** Let $X,D$ be a metric space. Suppose $F$ is a closed subset of $X$ and $x\in X-F$. Then there are open sets $U$ and $V$ of $X$ such that $x\in U$, $F\subset V$, and $U\cap V=\phi$.

_Proof._ Since $F$ is closed and $x\in X-F$, $D(x,F)\neq0$ (Fig. 2.18). For each $y\in F$, set

$$
U_y=N(y,\tfrac12D(x,F)).
$$

Then $V=\bigcup_F U_y$ is an open set which contains $F$. Also $U=N(x,\frac12D(x,F))$ is an open set which contains $x$. In order to complete the proof that $U$ and $V$ satisfy the terms of Proposition 12, we must show that $U\cap V=\phi$. Suppose $U\cap V\neq\phi$, and select $w$ from $U\cap V$. Then $D(w,y)<\frac12D(x,F)$ for some $y\in F$, and $D(w,x)<\frac12D(x,F)$. It then follows that

$$
D(x,y)\leq D(w,x)+D(w,y)<\tfrac12D(x,F)+\tfrac12D(x,F)=D(x,F).
$$

<span id="printed-page-36"></span>

<!-- source: PDF 42, printed 36 -->

But $D(x,F)=\operatorname{glb}\{D(x,z)\mid z\in F\}$, and $y\in F$; hence $D(x,y)\geq D(x,F)$, a contradiction. Since the assumption that $U\cap V\neq\phi$ led to a contradiction, it must be that $U\cap V=\phi$.

We now prove an even stronger result.

**Proposition 13.** Let $X,D$ be a metric space. Then if $F$ and $F'$ are two closed subsets of $X$ such that $F\cap F'=\phi$, there are open sets $U$ and $V$ of $X$ such that

$$
F\subset U,\quad F'\subset V,\quad\text{and}\quad U\cap V=\phi.
$$

(Note that this proposition contains Proposition 12 as a special case, since each one-element subset of $X$ is a closed subset by Proposition 5.)

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-2.19.svg" alt="Figure 2.19: disjoint closed sets F and F′ with disjoint surrounding open sets U and V." /><figcaption>Figure 2.19</figcaption></figure>

_Proof._ For each $y\in F$, set $\rho_y=\frac12D(y,F')$, and for each $y'\in F'$, set $\rho'_{y'}=\frac12D(y',F)$. Set $U=\bigcup_F N(y,\rho_y)$ and $V=\bigcup_{F'}N(y',\rho'_{y'})$ (Fig. 2.19). Since both $U$ and $V$ are the union of a family of open sets, both are open. Then, since $F\subset U$ and $F'\subset V$, it remains to show that $U\cap V=\phi$. Suppose we can find $w\in U\cap V$. It follows that $D(y,w)<\rho_y$ for some $y\in F$, and that $D(y',w)<\rho'_{y'}$ for some $y'\in F'$. We may suppose $\rho_y\geq\rho'_{y'}$. Then

$$
D(y,y')\leq D(y,w)+D(y',w)<\rho_y+\rho'_{y'}\leq2\rho_y=D(y,F').
$$

But $D(y,y')\geq D(y,F')$, hence a contradiction. It follows then that $U\cap V=\phi$.

Propositions 12 and 13 represent what are called _separation properties_ because they measure our ability to “separate” or distinguish disjoint closed subsets. Oddly enough, as the following example demonstrates, Proposition 13 does not imply that the “distance” between two disjoint nonempty closed subsets of a metric space is always greater than 0.

<span id="printed-page-37"></span>

<!-- source: PDF 43, printed 37 -->

**Example 19.** Let $R^2$ be the coordinate plane with the Pythagorean metric $D$, and let

$$
F=\{(x,y)\mid y=1/x,x\neq0\}
$$

and

$$
F'=\{(x,y)\mid y=0\}
$$

(Fig. 2.20). That is, $F$ is the graph of the rectangular hyperbola $y=1/x$, while $F'$ is the $x$-axis. Both these sets are closed and $F\cap F'=\phi$. Since $y=1/x$ has the $x$-axis for an asymptote, $D(F,F')=0$. Nevertheless, $F$ and $F'$ can still be separated in the sense of Proposition 13.

<figure class="topology-chapter-figure"><img src="/elementary-topology/figures/figure-2.20.svg" alt="Figure 2.20: the rectangular hyperbola y=1/x and the x-axis are disjoint closed sets at distance zero." /><figcaption>Figure 2.20</figcaption></figure>

We saw in Proposition 11 that a subset $F$ of a metric space $X,D$ is closed if and only if each point of $X$ which is 0 distance from $F$ is an element of $F$. This inspires the following definition.

**Definition 8.** Let $X,D$ be a metric space and $A\subset X$. We define the _closure_ of $A$, denoted by $\operatorname{Cl}A$, by

$$
\operatorname{Cl}A=\{x\in X\mid D(x,A)=0\}.
$$

**Example 20.** Let $R$ be the space of real numbers with the absolute value metric. Set $A=\{1/n\mid n=1,2,3,\ldots\}$. Since $1/n\to0$, then $d(0,A)=0$ (Exercise 5). Since $D(1/n,1/n)=0$ for each $n$, then $A\subset\operatorname{Cl}A$. On the other hand, if $y$ is any number other than 0 or an element of $A$, then it is readily verified that $D(y,A)>0$. Therefore $\operatorname{Cl}A=A\cup\{0\}$.

**Proposition 14.** $\operatorname{Cl}A$ as given in Definition 8 is a closed subset of $X$.

_Proof._ Suppose that $\operatorname{Cl}A$ is not closed. Then $X-\operatorname{Cl}A$ is not open; therefore there is an element $x\in X-\operatorname{Cl}A$ such that for any positive number $\rho$, $N(x,\rho)\cap\operatorname{Cl}A\neq\phi$. Select $w\in N(x,\rho)\cap\operatorname{Cl}A$. Then since $N(x,\rho)$ is open, there is a positive number $q$ such that $N(w,q)\subset N(x,\rho)$. But since $w\in\operatorname{Cl}A$, then $D(w,A)=0$; therefore there is at least one element

$$
a\in A\cap N(w,q)\subset N(x,\rho).
$$

But this means that, for any positive number $\rho$, there is an element $a\in A$ such that $D(x,a)<\rho$. It follows then that $\operatorname{glb}\{D(x,a)\mid a\in A\}=D(x,A)=0$; thus $x\in\operatorname{Cl}A$, a contradiction, since $x\in X-\operatorname{Cl}A$. $\operatorname{Cl}A$ must therefore be a closed subset of $X$.

::: warning Missing source page 38
Printed page 38 is absent. The next available page contains exercise parts (c) and (d) whose opening and number are missing, followed by Exercise 7. Any missing definition of the notation $\operatorname{Fr}$ and any other intervening text are not reconstructed here.
:::

<span id="printed-page-39"></span>

<!-- source: PDF 44, printed 39 -->

## Available exercise continuation

::: info Fragment boundary
The following two parts are transcribed exactly from the available page. Their missing exercise opening has not been supplied.
:::

c) $\operatorname{Fr}\{x\mid x\text{ is a rational number}\}\subset R$, as in (a)

d) $\operatorname{Fr}\{(x,y)\mid x=3\}\subset R^2$, with metric $D_2$ (Example 3)

7. Prove that a subset $A$ of a metric space $X,D$ is open if and only if $\operatorname{Fr}A\cap A=\phi$. Prove that $A$ is closed if and only if $\operatorname{Fr}A\subset A$.

::: info Chapter boundary
The next supplied page, printed page 40 (PDF page 45), begins Chapter 3, _Topologies_.
:::

[Return to Chapter 2](./index)

<style>
.topology-chapter-figure { max-width: 28rem; margin: 1.75rem auto; text-align: center; }
.topology-chapter-figure img { width: 100%; background: white; }
.topology-chapter-figure figcaption { font-family: Georgia, serif; font-size: 0.875rem; margin-top: 0.4rem; }
.topology-chapter-figure-pair { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 1.5rem; margin: 1.75rem 0; }
.topology-chapter-figure-pair .topology-chapter-figure { margin: 0; }
@media (max-width: 480px) { .topology-chapter-figure-pair { grid-template-columns: 1fr; } }
</style>

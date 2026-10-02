---
title: Homotopic Functions — Elementary Topology
---

# 11.1 Homotopic Functions

<span id="printed-page-233"></span>

<!-- Source: PDF217, printed233. -->

Good mathematical terminology is generally intuitively appealing. For example, consider this statement: The unit disk

$$
Y=\{(x,y)\mid x^2+y^2\le1\}\subset R^2
$$

(with the usual topology) can be _contracted_ to $(0,0)$; that is, $Y$ is a _contractible space_ (Fig. 11.1). Most likely, the reader has not encountered the notion of a contractible space before, but having studied the topology of $Y$, he might feel that such a statement fits in with his notions about $Y$.

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.1.svg" alt="A shaded unit disk Y on coordinate axes, centered at (0,0)." />
<figcaption>Figure 11.1. <a href="/docs/elementary-topology/reader?page=233">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.2.svg" alt="The unit disk with a darker concentric contracted image and radial point labels, illustrating a contraction toward the origin." />
<figcaption>Figure 11.2. <a href="/docs/elementary-topology/reader?page=233">View the interactive figure and its Substance program.</a></figcaption>
</figure>

The word _contractible_ implies the idea of shrinkability, that somehow we can reduce $Y$ to something smaller; in particular, contractible to $(0,0)$ implies that $Y$ can in some reasonable way be shrunk down to $(0,0)$. One of the most obvious ways to contract the disk $Y$ is to slide its points along radii toward the center $(0,0)$. One such “contracting function,” call it $j_{1/2}$, of the disk into itself could be described as follows: For each $(x,y)\in Y$, let $j_{1/2}(x,y)$ be the point of $Y$ on the segment $\overline{(0,0)(x,y)}$ midway between $(x,y)$ and $(0,0)$ (Fig. 11.2). It is easily seen that $j_{1/2}$ is continuous. We may further note that if $0\le r\le1$, we may define $j_r:Y\to Y$ by letting <span id="printed-page-234"></span><!-- Source: PDF218, printed234. -->$j_r(x,y)$ be the point on $\overline{(0,0)(x,y)}$ which is $1/r$th of the distance from $(0,0)$ to $(x,y)$. Then $j_1$ is the identity mapping on $Y$, and $j_0$ maps all of $Y$ into $(0,0)$.

::: info Source formula
The source says “$1/r$th,” retained as printed, alongside its statements about $j_0$ and $j_1$.
:::

For each $r\in[0,1]$, we have a continuous function $j_r$ from $Y$ into $Y$. We can therefore define a function $j$ from $Y\times[0,1]$ into $Y$ by

$$
j((x,y),r)=j_r(x,y)
$$

for each $(x,y)\in Y$ and $r\in[0,1]$. Thus

$$
j_1=j\mid(Y\times\{1\})\quad\text{and}\quad j_0=j\mid(Y\times\{0\}).
$$

If we look at the images of $j_r$ in $Y$ for each $r\in[0,1]$, we note that as $r$ proceeds from $1$ to $0$, we gradually shrink $Y$ to $(0,0)$. Looking at it from the point of view of mappings, we are transforming the identity function on $Y$ into a constant function in a natural sort of way. An informal three-dimensional representation of what is happening is given in Fig. 11.3.

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.3.svg" alt="A cone with several horizontal circular cross sections shrinking from Y at height 1 to a single point at height 0, with contraction labels." />
<figcaption>Figure 11.3. <a href="/docs/elementary-topology/reader?page=234">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.4.svg" alt="A cylinder X times [0,1] mapped by H to a region Y containing the images of its horizontal slices." />
<figcaption>Figure 11.4. <a href="/docs/elementary-topology/reader?page=234">View the interactive figure and its Substance program.</a></figcaption>
</figure>

Throughout this chapter we will assume that the space $R$ of real numbers, the plane $R^2$, and Euclidean $n$-space in general, have their standard topologies unless it is specified otherwise. Any subsets of $R^n$ will be assumed to have the subspace topology from $R^n$.

Look again at the function $j:Y\times[0,1]\to Y$. It is intuitively clear that since $j_r$ is “close to” $j_{r'}$ provided $r$ is close to $r'$, $j$ is continuous. Of course, the continuity of $j$ can also be demonstrated quite rigorously.

To sum it all up then, for each $r\in[0,1]$, we have a function $j_r$ from $Y$ into $Y$ such that $j_1$ is the identity function on $Y$ and $j_0$ is a constant function. As $r$ varies smoothly from $1$ to $0$, $j_r$ varies smoothly from $j_1$ to $j_0$. This enables us to define a continuous function $j:Y\times[0,1]\to Y$ such that $j\mid(Y\times\{r\})=j_r$. We have in a sense smoothly transformed the identity function on $Y$ into a constant function. Thus we now have more justification for calling $Y$ a contractible space.

<span id="printed-page-235"></span>

<!-- Source: PDF219, printed235. -->

This notion of transforming one function continuously into another is one of the most important ideas in topology. We express it formally in the following definition.

**Definition 1.** Let $f$ and $g$ be continuous functions from a space $X,\tau$ into a space $Y,\tau'$. Then $f$ is said to be _homotopic_ to $g$ if there is a continuous function $H$ from $X\times[0,1]$ into $Y$ such that

$$
H\mid(X\times\{1\})=f\quad\text{and}\quad H\mid(X\times\{0\})=g.
$$

The function $H$ is said to be a _homotopy between $f$ and $g$_ (Fig. 11.4).

Intuitively again, $f$ and $g$ are homotopic if $f$ can be continuously transformed into $g$. We see that the identity function on the unit disk is homotopic to a constant function, that is, the function which maps the entire unit disk into $(0,0)$.

**Example 1.** Reexamine Example 17 of Chapter 5. Note that any two continuous functions from $[0,1]$ into $R^2$ are homotopic. More generally, we can say that any two continuous functions from $[0,1]$ into any absolute retract (Section 5.5, Exercise 2) are homotopic.

The fact that two functions $f$ and $g$ are homotopic is generally far easier to see intuitively than to express analytically; that is, it is often evident that $f$ and $g$ are homotopic even when the actual writing out of an explicit homotopy between $f$ and $g$ would require considerable labor.

**Example 2.** We have already seen that the identity function $j_1=i$ on the unit disk $Y$ is homotopic to the function $j_0$ which maps all of $Y$ into $(0,0)$. We could also show that $i$ is homotopic to the function $k:Y\to Y$ defined by

$$
k(x,y)=(0,\tfrac12)
$$

for any $(x,y)\in Y$. One argument to show that $i$ is homotopic to $k$ could use functions similar to the $j_r$ previously used to show that $i$ was homotopic to $j_0$. Still another method to produce a homotopy between $i$ and $k$ would be to break $[0,1]$ up into $[0,\tfrac12]\cup[\tfrac12,1]$. Let $j$ be the homotopy between $j_1=i$ and $j_0$ as defined earlier. Define

$$
H((x,y),r)=\begin{cases}
j((x,y),1-2r),&\text{if }0\le r\le\tfrac12,\\
(0,r-\tfrac12),&\text{if }\tfrac12\le r\le1.
\end{cases}
$$

Geometrically, $H$ first contracts $Y$ to $(0,0)$ and then slides it along the segment $\overline{(0,0)(0,\tfrac12)}$ from $(0,0)$ to $(0,\tfrac12)$ (Fig. 11.5). Since

$$
H\mid(Y\times[0,\tfrac12])\quad\text{and}\quad H\mid(Y\times[\tfrac12,1])
$$

are each continuous, and since $H\mid(Y\times\{\tfrac12\})$ is well-defined, $H$ is con<span id="printed-page-236"></span><!-- Source: PDF220, printed236. -->tinuous by Proposition 11, Chapter 4. Since

$$
H\mid(Y\times\{1\})=i\quad\text{and}\quad H\mid(Y\times\{0\})=k,
$$

$H$ is a suitable homotopy between $i$ and $k$.

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.5.svg" alt="A cylinder of parameter slices mapped to concentric contracting disk images in a coordinate plane." />
<figcaption>Figure 11.5. <a href="/docs/elementary-topology/reader?page=236">View the interactive figure and its Substance program.</a></figcaption>
</figure>

::: info Source endpoint formulas
The piecewise formula on printed page 235 and its asserted endpoint restrictions on printed page 236 are retained as printed, even though the piecewise formula has those endpoints reversed.
:::

**Example 3.** An _arc_ is a homeomorphism from $[0,1]$ into any space; thus any arc is also a path. Since any two paths in $R^2$ are homotopic, it is certainly true that any two arcs in $R^2$ are homotopic. In particular, if $a_1$ and $a_2$ are distinct arcs in $R^2$ such that $a_1(0)=a_2(0)$ and $a_1(1)=a_2(1)$, then $a_1$ and $a_2$ are homotopic. Suppose that $P$ is some point in the area bounded by the images of $a_1$ and $a_2$ (Fig. 11.6), and that $a_1$ and $a_2$ are considered as arcs in $R^2-\{P\}$. Then $a_1$ and $a_2$ are not homotopic in $R^2-\{P\}$, since the missing point would prevent us from transforming $a_1$ continuously into $a_2$. Intuitively, in order to transform $a_1$ into $a_2$ we would have to have a break at some stage of the transformation to get past the barrier posed by the missing point.

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.6.svg" alt="Many curved paths between two fixed endpoints, surrounding an interior point P and labeled as members of a homotopy family." />
<figcaption>Figure 11.6. <a href="/docs/elementary-topology/reader?page=236">View the interactive figure and its Substance program.</a></figcaption>
</figure>

In like manner, removing a point from the interior of the unit disk $Y$ prevents the identity map on $Y$ from being homotopic to a constant map. We therefore see that any two given functions from one space into another are not necessarily homotopic. The next proposition shows that the notion of homotopy enables us to classify functions from one space into another according to the functions to which they are homotopic.

**Proposition 1.** Let $X,\tau$ and $Y,\tau'$ be topological spaces, and let $Y^X$ denote the set of all continuous functions from $X$ into $Y$. (The reason for the notation $Y^X$ will be made clearer in the appendix.) Let $\sim$ denote “is homotopic to,” that is, $f\sim g$ will mean that $f$ is homotopic to $g$. Then $\sim$ is an equivalence relation on $Y^X$.

<span id="printed-page-237"></span>

<!-- Source: PDF221, printed237. -->

_Proof._ If $f\in Y^X$, then $f\sim f$. The explicit homotopy $H:X\times[0,1]\to Y$ between $f$ and $f$ is given by $H(x,r)=f(x)$ for all $x\in X$ and $r\in[0,1]$. In order to verify that $H$ is a suitable homotopy, we must show that

$$
H\mid(X\times\{1\})=H\mid(X\times\{0\})=f
$$

and that $H$ is continuous. Now

$$
H\mid(X\times\{1\})(x,1)=H(x,1)=f(x)
$$

for any $x\in X$, and hence $H\mid(X\times\{1\})=f$; similarly, $H\mid(X\times\{0\})=f$. Suppose $U$ is any open subset of $Y$. Then

$$
H^{-1}(U)=f^{-1}(U)\times[0,1].
$$

But since $f$ is continuous, $f^{-1}(U)$ is an open subset of $X$; thus $f^{-1}(U)\times[0,1]$ is an open subset of $X\times[0,1]$. Therefore $H$ is continuous.

If $f\sim g$, then $g\sim f$. Since $f\sim g$, there is a homotopy

$$
H:X\times[0,1]\to Y
$$

such that

$$
H\mid(X\times\{1\})=f\quad\text{and}\quad H\mid(X\times\{0\})=g.
$$

Define $H':X\times[0,1]\to Y$ by

$$
H'(x,r)=H(x,1-r)\quad\text{for any }x\in X\text{ and }r\in[0,1].
$$

Then $H'(X\times\{0\})=f$ and $H'\mid(X\times\{1\})=g$. All we have done is to turn $H$ upside down, or reverse its direction, if you prefer. Then $H'$ is a suitable homotopy between $g$ and $f$.

If $f\sim g$ and $g\sim k$, then $f\sim k$. Since $f\sim g$, there is a homotopy $H_1:X\times[0,1]\to Y$ such that

$$
H_1\mid(X\times\{1\})=f\quad\text{and}\quad H_1\mid(X\times\{0\})=g.
$$

Since $g\sim k$, there is a homotopy $H_2:X\times[0,1]\to Y$ such that

$$
H_2\mid(X\times\{1\})=g\quad\text{and}\quad H_2\mid(X\times\{0\})=k.
$$

Define $H:X\times[0,1]\to Y$ by

$$
H(x,r)=\begin{cases}
H_2(x,2r),&0\le r\le\tfrac12,\\
H_1(x,2r-1),&\tfrac12\le r\le1.
\end{cases}
$$

Note that, for all $x\in X$,

$$
\begin{aligned}
H(x,1)&=H_1(x,2-1)=H_1(x,1)=f(x);\\
H(x,0)&=H_2(x,0)=k(x);\\
H(x,\tfrac12)&=H_1(x,0)=H_2(x,1)=g(x).
\end{aligned}
$$

<span id="printed-page-238"></span>

<!-- Source: PDF222, printed238. -->

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.7.svg" alt="A sliced cylinder and its target image, used to depict a homotopy assembled from consecutive homotopies." />
<figcaption>Figure 11.7. <a href="/docs/elementary-topology/reader?page=238">View the interactive figure and its Substance program.</a></figcaption>
</figure>

Therefore $H$ is well-defined and will be a homotopy between $f$ and $k$ if it is continuous. The proof of the continuity of $H$ is left as an exercise. Essentially what we have done is to “paste” $H_1$ and $H_2$ together to get a new homotopy between $f$ and $k$ (Fig. 11.7).

We have therefore shown that $\sim$ is an equivalence relation on $Y^X$.

**Definition 2.** If $f\in Y^X$, then the family of continuous functions from $X$ into $Y$ which are homotopic to $f$ is called the _homotopy class_ of $f$.

Because of Proposition 1, we can say the homotopy classes of $Y^X$ form a partition of $Y^X$.

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.8.svg" alt="The two end disks of a cylinder mapped separately by f and g into a target Y, introducing the extension formulation of a homotopy." />
<figcaption>Figure 11.8. <a href="/docs/elementary-topology/reader?page=238">View the interactive figure and its Substance program.</a></figcaption>
</figure>

We close this section by noting that the question of whether two functions in $Y^X$ are homotopic is really a question of whether or not a certain function can be extended. In particular, we may define

$$
h:X\times\{0\}\cup X\times\{1\}\to Y
$$

by $h(x,0)=f(x)$ and $h(x,1)=g(x)$ for all $x\in X$ (Fig. 11.8). Then $f$ and $g$ are homotopic if and only if $h$ can be extended to a continuous function $H$ from $X\times[0,1]\to Y$ such that

$$
H\mid(X\times\{0\})\cup(X\times\{1\})=h.
$$

::: warning Missing printed page 239
The supplied scan omits printed page 239. Exercises 1–4 and any other unavailable material are not reconstructed. Printed page 240 supplies Exercise 5 below.
:::

<span id="printed-page-240"></span>

<!-- Source: PDF223, printed240, section11.1 fragment. -->

## Exercises — available continuation

5. Explain intuitively why the identity function on the unit circle $X=\{(x,y)\mid x^2+y^2=1\}$ is not homotopic to $f:X\to X$ where $f(z)=(1,0)$ for all $z\in X$. Take a rubber band and pin the rubber band at one point to a table. Then try to push the rubber band back along itself to the point at which it is pinned.

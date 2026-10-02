---
title: The Fundamental Group — Elementary Topology
---

# 11.3 The Fundamental Group

<span id="printed-page-246"></span>

<!-- Source: PDF229, printed246. -->

Thus far we have seen that $\pi_1(Y,y_0),\#$ is a semigroup (that is, a set with an associative operation) which also has an identity which we will denote by $|k|$ (Section 11.2, Exercise 3). It remains to be shown that each element of $\pi_1(Y,y_0)$ has an inverse with respect to $\#$. If $a\in L(Y,y_0)$, we define $a^{-1}$ by letting $a^{-1}(r)=a(1-r)$ for each $r\in[0,1]$. Then

$$
a^{-1}(0)=a(1)=y_0\quad\text{and}\quad a^{-1}(1)=a(0)=y_0.
$$

Since $a$ is continuous, $a^{-1}$ is also; hence $a^{-1}$ is an element of $L(Y,y_0)$. Geometrically, $a^{-1}$ is $a$ going around in the opposite direction (Fig. 11.15).

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.15.svg" alt="Two circular based loops showing a path and the same path traversed in the opposite direction." />
<figcaption>Figure 11.15. <a href="/docs/elementary-topology/reader?page=246">View the interactive figure and its Substance program.</a></figcaption>
</figure>

**Proposition 4.** If $a\in L(Y,y_0)$, then

$$
a\#a^{-1}\sim a^{-1}\#a\sim k.
$$

Therefore the inverse of $|a|$ in $\pi_1(Y,y_0)$ is $|a^{-1}|$.

_Proof._ An explicit homotopy between $k$ and $a\#a^{-1}$ is given by

$$
H(r,s)=\begin{cases}
a(2r(1-s)),&\text{if }0\le r\le\tfrac12,\\
a(2(1-r)(1-s)),&\text{if }\tfrac12\le r\le1.
\end{cases}
$$

The reader should confirm that this is indeed a suitable homotopy. The geometric idea behind it is that we are starting at $y_0$, then going out along $a$ to a certain point and finally coming back along $a^{-1}$, each time shortening the distance we go until have pulled $a\#a^{-1}$ entirely back into $y_0$. The reader should also produce a homotopy to show $a^{-1}\#a\sim k$.

We therefore conclude that $\pi_1(Y,y_0),\#$ is a group with identity $|k|$ in which $|a|\in\pi_1(Y,y_0)$ has as its inverse $|a^{-1}|$. We are therefore justified in making the following definition.

**Definition 4.** Let $Y,\tau'$ be any space, and let $y_0\in Y$ and $\pi_1(Y,y_0),\#$ be as described above. Then

$$
\pi_1(Y,y_0),\#
$$

is called the _fundamental group based on $y_0$_ of the space $Y$.

<span id="printed-page-247"></span>

<!-- Source: PDF230, printed247. -->

Of course it is all well and good to know that $\pi_1(Y,y_0),\#$ is a group. This is a good beginning, but hardly any more than that. For to be of much use in the study of topological spaces, $\pi_1(Y,y_0)$ must have certain properties. First, $\pi_1(Y,y_0)$ should be computable, at least for the majority of spaces which might be of interest. Second, $\pi_1(Y,y_0)$ should tell us something about the space $Y,\tau'$; if it gives no information about the structure of $Y$ as a topological space, it clearly has no value in the study of topology. Third, it is rather repugnant that $\pi_1(Y,y_0)$ should depend on the point of $Y$ on which it is based; in other words, $\pi_1(Y,y_0)$ should depend on $Y$ and $\tau'$ rather than on $Y,\tau'$, and $y_0$. Otherwise, we might get different fundamental groups for the same space without any clear idea of how to choose among them, and we would also be given the repugnant implication that some point of $Y$ was better, or at least in some way significantly different than, other points of $Y$. Fourth, if there is a continuous function $f$ from $Y$ onto a space $Z,\tau''$, we would hope that there is naturally associated with $f$ a homomorphism from the fundamental group of $Y$ into the fundamental group of $Z$. This would enable us to attack the difficult problem of whether there is a continuous function from one space onto another; most of all, however, we would expect that if groups are to be associated with spaces, then homomorphisms of groups will be associated with continuous functions from one space to the other.

All of the above considerations are very basic and very important. It will be the goal of the remainder of this chapter to at least partly settle each one of them. We first consider the question of the computation of fundamental groups.

There are a number of theorems and techniques for computing fundamental groups of spaces. Many of these are beyond the scope of this book. For most of the simpler spaces, however, it is not very difficult to compute the fundamental group.

**Example 4.** Let $Y,\tau'$ be a space which is contractible to one of its points $y_0$. Then from Section 11.2, Exercise 2, we see that $\pi_1(Y,y_0),\#$ is a group of precisely one element.

**Example 5.** Let

$$
Y=\{(x,y)\mid x^2+y^2=1\}\subset R^2
$$

and let $y_0$ be any point of $Y$. Let $a$ be the loop which goes once around the circle in the counterclockwise direction. Then $a$ cannot be homotopic to $k$ (the function which takes all of $[0,1]$ onto $y_0$), since $a$ cannot be “pulled back” into $y_0$ without breaking (see Section 11.2, Exercise 4). For each $a\in L(Y,y_0)$ and each integer $n$, define

$$
na=\begin{cases}
a\#\cdots\#\text{(}n\text{ times)}\#a,&\text{if }n\text{ is positive},\\
k,&\text{if }n=0,\\
a^{-1}\#\cdots\#(-n)\#a^{-1},&\text{if }n\text{ is negative}.
\end{cases}
$$

::: warning Missing printed page 248
The supplied scan omits printed page 248, including the continuation of Example 5 and unavailable Example 6. Printed page 249 begins Example 7 below. Figure captions 11.16–11.18 have not been located in the available scan; Figure 11.18 is referenced on printed page 249. No missing text or drawings are reconstructed.
:::

<span id="printed-page-249"></span>

<!-- Source: PDF231, printed249. -->

**Example 7.** Let

$$
Y=\{(x,y,z)\mid x^2+y^2+z^2=1\}\subset R^3\quad\text{and}\quad y_0\in Y.
$$

If $P$ is any point of $Y$, we can show that $Y-\{P\}$ is homeomorphic to $R^2$ and hence is a contractible space. For let $P'$ be the point of $Y$ antipodal to $P$, and let $\mu$ be the plane in $R^3$ tangent to $Y$ at $P'$ (Fig. 11.18). For each $w\in Y-\{P\}$, the line $\ell(P,w)$ determined by $P$ and $w$ intersects $\mu$ in a unique point $u$. Define $h:Y-\{P\}\to\mu$ by

$$
\{h(w)\}=\ell(P,w)\cap\mu
$$

for each $w\in Y-\{P\}$. It is not hard to show that $h$ is a homeomorphism.

Now $Y$ is not contractible to any of its points (the intuitive argument runs, “You can't peel an orange without breaking its skin”), but if $a$ is any loop in $Y$ and $P$ is any point in $Y-a([0,1])$, then $a$ is essentially a loop in $R^2$; hence $a$ is homotopic to $k$, where $|k|$ is the identity of $\pi_1(Y,y_0)$ (Fig. 11.19). Thus any loop in $Y$ based on $y_0$ is homotopic to $k$, and therefore $\pi_1(Y,y_0)$ consists only of $|k|$.

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.19.svg" alt="Based loops through y₀ on a sphere, contracted while avoiding the pole P." />
<figcaption>Figure 11.19. <a href="/docs/elementary-topology/reader?page=249">View the interactive figure and its Substance program.</a></figcaption>
</figure>

$Y$ is hence an example of a noncontractible space which has a trivial fundamental group. Note that if $P$ and $Q$ are any two points of $Y$, then $Y-\{P,Q\}$ is homeomorphic to the space $Y'$ in Example 6 and thus has a fundamental group isomorphic to the additive group of integers.

The following proposition aids in the computation of many fundamental groups.

**Proposition 5.** Let $X,\tau$ and $Y,\tau'$ be spaces with base points $x_0$ and $y_0$, respectively. Then

$$
\pi_1(X\times Y,(x_0,y_0))
$$

is isomorphic to the direct sum of $\pi_1(X,x_0)$ and $\pi_1(Y,y_0)$,

$$
\pi_1(X,x_0)\oplus\pi_1(Y,y_0).
$$

That is, the fundamental group of the product space of two spaces is the direct sum of the fundamental groups of the component spaces.

_Proof._ Suppose $a\in L(X\times Y,(x_0,y_0))$. Let $p_X$ and $p_Y$ be the projections of $X\times Y$ into $X$ and $Y$, respectively. Then

$$
p_X\circ a\in L(X,x_0)
$$

<span id="printed-page-250"></span>

<!-- Source: PDF232, printed250. -->

and

$$
p_Y\circ a\in L(Y,y_0).
$$

Define

$$
f:\pi_1(X\times Y,(x_0,y_0))\to\pi_1(X,x_0)\oplus\pi_1(Y,y_0)
$$

by

$$
f(|a|)=(|p_X\circ a|,|p_Y\circ a|).
$$

We will prove that $f$ is the desired isomorphism.

We first show that $f$ is well-defined, that is, that $f(|a|)$ is independent of the representative of $|a|$ that is used. Suppose $a\sim a'$. Then there is a homotopy

$$
H:[0,1]\times[0,1],\{0,1\}\times[0,1]\to X\times Y,(x_0,y_0)
$$

between $a$ and $a'$. It can, however, then be verified by straightforward computation that $p_X\circ H$ and $p_Y\circ H$ are suitable homotopies between $p_X\circ a$ and $p_X\circ a'$, and $p_Y\circ a$ and $p_Y\circ a'$, respectively. (Compare this to Section 11.1, Exercise 4.) Therefore $f$ is well-defined.

We now show that $f$ is onto: Suppose

$$
(|a_1|,|a_2|)\in\pi_1(X,x_0)\oplus\pi_1(Y,y_0).
$$

Define $a\in L(X\times Y,(x_0,y_0))$ by

$$
a(r)=\begin{cases}
(a_1(2r),y_0),&\text{if }0\le r\le\tfrac12,\\
(x_0,a_2(2r-1)),&\text{if }\tfrac12\le r\le1.
\end{cases}
$$

Then $p_X\circ a\sim a_1$ and $p_Y\circ a\sim a_2$ (Exercise 1). Moreover, $a$ is easily seen to be continuous ($a$ is continuous on both $[0,\tfrac12]$ and $[\tfrac12,1]$ and is well-defined at $r=\tfrac12$); also

$$
a(0)=(a_1(0),y_0)=(x_0,y_0)=a(1),
$$

and hence $a$ is an element of $L(X\times Y,(x_0,y_0))$. But $f(|a|)$ then is $(|a_1|,|a_2|)$; therefore $f$ is onto.

Now we show that $f$ is one-one: Suppose $f(|a|)=f(|a'|)$. Then

$$
(|p_X\circ a|,|p_Y\circ a|)=(|p_X\circ a'|,|p_Y\circ a'|).
$$

Therefore there is a homotopy $H_1$ between $p_X\circ a$ and $p_X\circ a'$ and a homotopy $H_2$ between $p_Y\circ a$ and $p_Y\circ a'$. Define a homotopy

$$
H:[0,1]\times[0,1]\to X\times Y
$$

by

$$
H(r,s)=(H_1(r,s),H_2(r,s)).
$$

Direct computation shows that $H$ is a homotopy between $a$ and $a'$; hence $|a|=|a'|$. Therefore $f$ is one-one.

<span id="printed-page-251"></span>

<!-- Source: PDF233, printed251. -->

It remains to be shown that $f$ is a homomorphism. Suppose $|a|$ and $|a'|$ are elements of $\pi_1(X\times Y,(x_0,y_0))$. Then

$$
\begin{aligned}
f(|a|\#|a'|)&=f(|a\#a'|)=(|p_X\circ(a\#a')|,|p_Y\circ(a\#a')|)\\
&=(|p_X\circ a\#p_X\circ a'|,|p_Y\circ a\#p_Y\circ a'|)
\end{aligned}
$$

(this latter equality follows at once from the manner in which the addition of loops has been defined)

$$
\begin{aligned}
&=(|p_X\circ a|\#|p_X\circ a'|,|p_Y\circ a|\#|p_Y\circ a'|)\\
&=(|p_X\circ a|,|p_Y\circ a|)\#(|p_X\circ a'|,|p_Y\circ a'|)
\end{aligned}
$$

[where $\#$ is here “addition” in $\pi_1(X,x_0)\oplus\pi_1(Y,y_0)$]

$$
=f(|a|)\#f(|a'|).
$$

Therefore $f$ is a homomorphism, and, consequently, an isomorphism.

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.20.svg" alt="A torus with a marked base point y0 and two distinguished loop generators a and b." />
<figcaption>Figure 11.20. <a href="/docs/elementary-topology/reader?page=251">View the interactive figure and its Substance program.</a></figcaption>
</figure>

**Example 8.** We have seen that the fundamental group of the circle $Y$ in Example 5 (relative to any base point) is isomorphic to the additive group of integers $Z,+$. The torus $Y\times Y$ then has a fundamental group which is isomorphic to the direct sum of the additive group of integers with itself, that is, $Z\oplus Z,+$. Note that $Z\oplus Z,+$ has two generators, $(0,1)$ and $(1,0)$. These correspond to the homotopy classes of the loops $a$ and $b$ (actually the images of loops) in $Y\times Y$ shown in Fig. 11.20. Note that since the fundamental group of $Y$ does not depend on which base point is used, neither does the fundamental group of $Y\times Y$.

## Exercises

1. The following refer to the proof of Proposition 5.

   a) Prove that $p_X\circ a\sim a_1$ and $p_Y\circ a\sim a_2$.

   b) Prove each of the equalities in the chain of equalities used to show that $f$ is a homomorphism.

2. A space $X,\tau$ is said to be _simply connected_ if its fundamental group (with respect to some base point) is trivial. The circle is an example of a space which is connected but not simply connected. Which of the following spaces are <span id="printed-page-252"></span><!-- Source: PDF234, printed252. -->simply connected? In all cases, compute the fundamental groups using the given base point.

   a) $(0,1)\subset R$, using any base point

   b) $(0,1)\times Y$, with $Y$ as in Example 5 and using any base point

   c) $[0,1]\times Y'$, with $Y'$ as in Example 7 and using any base point

   d) $\{(x,y)\mid x^2+y^2<1\}\cup\{(0,1)\}\subset R^2$, with $(0,1)$ as base point

   e) $Y^n$, where $n$ is any positive integer, $Y$ is as in Example 5, and any base point is used

3. Suppose $X,\tau$ and $Y,\tau'$ are contractible to $x_0$ and $y_0$, respectively (Section 11.1, Exercise 3). Prove that the product space $X\times Y$ is contractible and simply connected.

4. Prove that the function $H$ defined in Proposition 4 is a genuine homotopy between $a\#a^{-1}$ and $k$. Find a homotopy between $a^{-1}\#a$ and $k$.

5. In the examples given in this section, the fundamental group has not depended on the base point which was used to compute it. Actually the following important proposition is true:

   If $Y,\tau'$ is arc-connected, and if $y_0$ and $y_1$ are any two points of $Y$, then $\pi_1(Y,y_0)$ is isomorphic to $\pi_1(Y,y_1)$.

   [Recall that an arc in $Y$ is a homeomorphism from $[0,1]$ into $Y$, and that $Y$ is said to be arc-connected if given any two distinct points $x$ and $y$ in $Y$, there is an arc $h$ in $Y$ such that $h(0)=x$ and $h(1)=y$.] Prove this proposition. [_Hint:_ An isomorphism can be defined as follows: Since $Y$ is arc-connected, there is a homeomorphism $j$ from $[0,1]$ into $Y$ such that $j(0)=y_1$ and $j(1)=y_0$. Define $j^{-1}:[0,1]\to Y$ by

   $$
   j^{-1}(r)=j(1-r)
   $$

   for each $r\in[0,1]$. Defining $j\#j^{-1}$ in the “natural” way, it can be shown that $j\#j^{-1}$ is homotopic to $k$, where $|k|$ is the identity of $\pi_1(Y,y_0)$. Suppose $|a|\in\pi_1(Y,y_0)$. Define $f:\pi_1(Y,y_0)\to\pi_1(Y,y_1)$ by

   $$
   f(|a|)=|j\#a\#j^{-1}|
   $$

   (Fig. 11.21). We know that $f(|a|)$ is a well-defined element of $\pi_1(Y,y_1)$. The details showing that $f$ is an isomorphism are straightforward, but should be supplied carefully by the reader to check his understanding of homotopies and the fundamental group.]

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.21.svg" alt="A circle with an attached line segment, a loop arrow, a junction point, and a path j along the segment to a second base point." />
<figcaption>Figure 11.21. <a href="/docs/elementary-topology/reader?page=252">View the interactive figure and its Substance program.</a></figcaption>
</figure>

Note that if a space is not arc-connected, then the fundamental group might depend on the base point. For example, let

$$
Y=\{(x,y)\mid x=3\}\cup\{(x,y)\mid x^2+y^2=1\}\subset R^2.
$$

If a base point $y_0$ in $\{(x,y)\mid x=3\}$ is chosen, then all of the loops based on $y_0$ are also in $\{(x,y)\mid x=3\}$ since otherwise we would have a continuous image of a connected space $[0,1]$ which was not connected. Therefore if

::: warning Missing printed page 253
The supplied scan omits printed page 253. The preceding discussion ends mid-sentence. The opening of Section 11.4 is also unavailable; no missing text is reconstructed.
:::

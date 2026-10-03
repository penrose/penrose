---
title: Loops — Elementary Topology
---

# 11.2 Loops

<span id="printed-page-240"></span>

<!-- Source: PDF223, printed240, section11.2 fragment. -->

It should be well known to the reader that one branch of mathematics can often be used to give results in another branch. Algebra and geometry are wedded in algebraic geometry, and calculus is a tool in differential geometry. There is no branch of mathematics today which is really self-contained. Even logic has started to draw heavily in recent years on topological methods to obtain some of its most significant discoveries. It must also have occurred to the reader that topology is highly geometric; at the same time, topology often has its inspiration and applications in real and complex analysis. What we are going to do now is to begin to develop a method by which algebra can be used to express topological properties. The method we will study is only one of several applications of algebra to topology, and, in fact, we will study only a small part of the method at that. _Algebraic topology_ is both one of the oldest and one of the newest areas of topological studies; the _fundamental group_ dates back to the early days of topology (c. 1900, which really was not so long ago), while much of homology theory only dates back a few years, or less.

If $X,\tau$ and $Y,\tau'$ are arbitrary spaces, there is not much algebraic structure that might be given to either $Y^X$, the family of continuous functions from $X$ into $Y$, or the homotopy equivalence classes of $Y^X$. We therefore would like to find a suitable space $X,\tau$ so that either $Y^X$ or the homotopy classes could be given an algebraic structure which could help us to study $Y$. The following definition proves useful.

**Definition 3.** Let $Y,\tau'$ be any space and $y_0\in Y$. Then a continuous function $a$ from $[0,1]$ into $Y$ such that

$$
a(0)=a(1)=y_0
$$

is said to be a _loop_ in $Y$ with _base point_ $y_0$ (Fig. 11.9). Two loops $a_0$ and $a_1$ in $Y$ with base point $y_0$ are said to be _homotopic relative to $y_0$_ if there is a homotopy $H$ between $a_0$ and $a_1$ such that for each $r\in[0,1]$,

$$
H(0,r)=H(1,r)=y_0;
$$

that is, for each $r\in[0,1]$, $H\mid[0,1]\times\{r\}$ is a loop in $Y$ with base point $y_0$ (Fig. 11.10). We will denote the set of all loops in $Y$ with base point $y_0$ by $L(Y,y_0)$.

<span id="printed-page-241"></span>

<!-- Source: PDF224, printed241. -->

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.9.svg" alt="An irregular loop alpha inside a region Y, with the common initial and final point marked y0." />
<figcaption>Figure 11.9. <a href="/docs/elementary-topology/reader?page=241">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.10.svg" alt="A shaded unit parameter square mapped to a shaded band between two based loops in Y." />
<figcaption>Figure 11.10. <a href="/docs/elementary-topology/reader?page=241">View the interactive figure and its Substance program.</a></figcaption>
</figure>

The following notation will occasionally prove useful: Let $X,\tau$ and $Y,\tau'$ be spaces, and let $A$ and $B$ be subspaces of $X$ and $Y$, respectively. Then $f:X,A\to Y,B$ will denote that the function $f$ from $X$ to $Y$ has the property that $f(A)\subset B$. Thus $f:[0,1],\{0,1\}\to Y,y_0$ would denote a loop in $Y$ based on $y_0$ if $f$ were continuous.

Now $L(Y,y_0)$ is a subset of the family $Y^{[0,1]}$ of all continuous functions from $[0,1]$ into $Y$. The relation on $L(Y,y_0)$ defined by “is homotopic relative $y_0$ to,” which we will again denote by $\sim$, is seen to be an equivalence relation on $L(Y,y_0)$ by the same argument as was used in Proposition 1. We will denote the set of homotopy (relative to $y_0$) classes of $L(Y,y_0)$ by $\pi_1(Y,y_0)$. If $a$ is any loop in $Y$ with base point $y_0$, we will denote the equivalence class of $a$ by $|a|$. We shall now determine an algebraic structure on $\pi_1(Y,y_0)$; in particular, we shall make $\pi_1(Y,y_0)$ into a group.

We define an operation $\#$ on $\pi_1(Y,y_0)$ as follows: Suppose $|a_1|$ and $|a_2|$ are elements of $\pi_1(Y,y_0)$, that is, $|a_1|$ and $|a_2|$ are the homotopy relative $y_0$ equivalence classes of the loops $a_1$ and $a_2$. Define $|a_1|\#|a_2|$ to be the equivalence class of the loop $a_1\#a_2$ defined by

$$
(a_1\#a_2)(r)=\begin{cases}
a_1(2r),&0\le r\le\tfrac12,\\
a_2(2r-1),&\tfrac12\le r\le1.
\end{cases}
$$

We first verify that $a_1\#a_2$ is a bona fide element of $L(Y,y_0)$. Now $a_1\#a_2$ is at least a function from $[0,1]$ into $Y$. Note that $a_1\#a_2$ is formed by “going around” $a_1$ once and then going around $a_2$ once. Also $a_1\#a_2$ is continuous because $a_1\#a_2$ is continuous on both $[0,\tfrac12]$ and $[\tfrac12,1]$, and is well-defined at $r=\tfrac12$ since $(a_1\#a_2)(\tfrac12)=a_1(1)=a_2(0)=y_0$. [One of the principal reasons that we restricted ourselves to $L(Y,y_0)$ was so that we could “add” functions in this way and have them well-defined. If $a_1$ and $a_2$ both did not begin and end at $y_0$, we would have no assurance that $a_1\#a_2$ was well-defined at $r=\tfrac12$.] Since

$$
(a_1\#a_2)(0)=a_1(0)=(a_1\#a_2)(1)=a_2(1)=y_0,
$$

<span id="printed-page-242"></span>

<!-- Source: PDF225, printed242. -->

we see that $a_1\#a_2$ is really an element of $L(Y,y_0)$. We are therefore justified in taking its homotopy equivalence class, which is an element of $\pi_1(Y,y_0)$. Defining

$$
|a_1|\#|a_2|=|a_1\#a_2|,
$$

we have a beginning on an operation on $\pi_1(Y,y_0)$.

We are not sure yet, though, that $\#$ is really an operation. In defining $|a_1|\#|a_2|$, we made use of particular representatives of the equivalence classes $|a_1|$ and $|a_2|$. In order to have a valid operation on $\pi_1(Y,y_0)$, however, $|a_1\#a_2|$ must be independent of the representatives we pick. That is, the “sum” of two equivalence classes must depend only on the equivalence classes we are adding and not on which loops we pick from each class to compute the sum. Proposition 2 shows that $\#$ is a well-defined operation on $\pi_1(Y,y_0)$.

**Proposition 2.** If $a_1,a_2,a_3$, and $a_4$ are any elements of $L(Y,y_0)$ and $a_1\sim a_3$, then

$$
a_1\#a_2\sim a_3\#a_2.
$$

Similarly, if $a_2\sim a_4$, then

$$
a_1\#a_2\sim a_1\#a_4.
$$

Therefore

$$
|a_1|\#|a_2|=|a_1\#a_2|=|a_3\#a_2|=|a_3|\#|a_2|
$$

and

$$
|a_1|\#|a_2|=|a_1|\#|a_4|.
$$

_Proof._ Since $a_1\sim a_3$, there is a homotopy (relative to $y_0$)

$$
H:[0,1]\times[0,1]\to Y
$$

between $a_1$ and $a_3$. Define $H':[0,1]\times[0,1]\to Y$ by

$$
H'(r,s)=\begin{cases}
H(2r,s),&\text{if }0\le r\le\tfrac12,\\
a_2(2r-1),&\text{if }\tfrac12\le r\le1.
\end{cases}
$$

Then

$$
\begin{aligned}
H'(r,0)&=\begin{cases}a_1(2r),&\text{if }0\le r\le\tfrac12,\\a_2(2r-1),&\text{if }\tfrac12\le r\le1,\end{cases}\\
&=(a_1\#a_2)(r)\quad\text{for any }r\in[0,1].
\end{aligned}
$$

Similarly, $H'(r,1)=(a_3\#a_2)(r)$ for any $r\in[0,1]$. Also,

$$
H'(0,s)=H(0,s)=y_0=H'(1,s)=a_2(1).
$$

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.11.svg" alt="A unit parameter square with labeled edge paths and a horizontal division, giving a picture of a homotopy between concatenated loops." />
<figcaption>Figure 11.11. <a href="/docs/elementary-topology/reader?page=242">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<span id="printed-page-243"></span>

<!-- Source: PDF226, printed243. -->

Direct computation further shows that $H'$ is well-defined for $r=\tfrac12$. We have yet to show that $H'$ is continuous.

The continuity of $H'$ can be demonstrated in a formal argument, and the reader is urged to provide such an argument in this case. In general, however, an appeal to a “picture” of the homotopy is much easier, and usually just as convincing. For example, Fig. 11.11 gives a picture of the homotopy $H'$. Note how we continuously deform $a_1$ into $a_3$ while keeping $a_2$ fixed. The homotopy is continuous on $[0,\tfrac12]\times[0,1]$ and on $[\tfrac12,1]\times[0,1]$, and hence is continuous.

It is left as an exercise to prove that $a_1\#a_2\sim a_1\#a_4$.

**Corollary.** If $a_1,a_2,a_3$, and $a_4$ are in $L(Y,y_0)$, and if $a_1\sim a_3$ and $a_2\sim a_4$, then

$$
a_1\#a_2\sim a_3\#a_4.
$$

Therefore if $|a_1|=|a_3|$ and $|a_2|=|a_4|$ in $\pi_1(Y,y_0)$, then

$$
|a_1|\#|a_2|=|a_3|\#|a_4|.
$$

_Proof._ $a_1\#a_2\sim a_3\#a_2\sim a_3\#a_4$.

Although it is not obvious at first glance, the operation $\#$ defined on $\pi_1(Y,y_0)$ is not necessarily commutative. In other words, there is no particular reason for $a_1\#a_2$ to always be homotopic to $a_2\#a_1$.

We now have a set $\pi_1(Y,y_0)$ with an operation $\#$, and we claim that $\pi_1(Y,y_0),\#$ is a group. In order to prove this assertion, we must show that $\#$ is an associative operation, that there is an identity in $\pi_1(Y,y_0)$ with respect to $\#$, and that each element of $\pi_1(Y,y_0)$ has an inverse with respect to $\#$.

We first prove the associativity of $\#$.

**Proposition 3.** If $|a_1|$, $|a_2|$, and $|a_3|$ are any three elements of $\pi_1(Y,y_0)$, then

$$
(|a_1|\#|a_2|)\#|a_3|=|a_1|\#(|a_2|\#|a_3|).
$$

_Proof._ It suffices to show that $(a_1\#a_2)\#a_3\sim a_1\#(a_2\#a_3)$. By definition,

$$
((a_1\#a_2)\#a_3)(r)=\begin{cases}
(a_1\#a_2)(2r),&\text{if }0\le r\le\tfrac12,\\
a_3(2r-1),&\text{if }\tfrac12\le r\le1.
\end{cases}
$$

Therefore

$$
((a_1\#a_2)\#a_3)(r)=\begin{cases}
a_1(4r),&\text{if }0\le r\le\tfrac14,\\
a_2(4r-1),&\text{if }\tfrac14\le r\le\tfrac12,\\
a_3(2r-1),&\text{if }\tfrac12\le r\le1.
\end{cases}
$$

<span id="printed-page-244"></span>

<!-- Source: PDF227, printed244. -->

Similarly,

$$
(a_1\#(a_2\#a_3))(r)=\begin{cases}
a_1(2r),&\text{if }0\le r\le\tfrac12,\\
a_2(4r-2),&\text{if }\tfrac12\le r\le\tfrac34,\\
a_3(4r-3),&\text{if }\tfrac34\le r\le1.
\end{cases}
$$

Define

$$
H(r,s)=\begin{cases}
a_1(4r/(1+s)),&\text{if }0\le r\le\tfrac14(1+s),\\
a_2(4r-1-s),&\text{if }\tfrac14(1+s)\le r\le\tfrac14(2+s),\\
a_3(1-4(1-r)/(2-s)),&\text{if }\tfrac14(2+s)\le r\le1.
\end{cases}
$$

Direct computation shows that $H$ is a suitable homotopy between $(a_1\#a_2)\#a_3$ and $a_1\#(a_2\#a_3)$. Actually, a convincing argument for the suitability of $H$ can be made from its picture alone (Fig. 11.12).

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.12.svg" alt="A rectangular parameter domain whose two slanting internal boundaries change the relative widths of regions labeled by three paths, illustrating associativity." />
<figcaption>Figure 11.12. <a href="/docs/elementary-topology/reader?page=244">View the interactive figure and its Substance program.</a></figcaption>
</figure>

The reader should have begun to realize by now that pictures are a very useful way of arriving at homotopies. Sometimes a picture alone is sufficient to convince one that a homotopy exists. Almost always, at any rate, a picture helps to obtain an analytic expression of the homotopy. Perhaps the reader has been brought up to believe that arguing from pictures is a cardinal sin in mathematics; certainly there is no doubt that pictures can be misleading if improperly used. Nevertheless, diagrams intelligently employed can be indispensable tools in a mathematical argument. This is particularly true with regard to homotopy arguments for two reasons. First, the notion of a homotopy is highly geometric (as is much of topology); hence we should expect pictures to be apropos. Second, actually producing a homotopy analytically may be so cumbersome, while, at the same time, producing a picture may be so easy and convincing, that no reasonable person would demand the explicit analytic expression. Let the reader then accustom himself to the use of pictures in homotopy theory, while, at the same time, being sure that he understands the theory and meaning behind any picture he uses, and would be able to produce an analytic expression if necessary.

<span id="printed-page-245"></span>

<!-- Source: PDF228, printed245. -->

## Exercises

1. In Proposition 2, prove $a_1\#a_2\sim a_1\#a_4$.

2. Suppose the space $Y,\tau'$ is contractible to the point $y_0$; let $k$ denote the function which maps $Y$ onto $y_0$. Let $f$ be any continuous function from a space $X,\tau$ into $Y$. Prove that $f$ is homotopic to $k'$, where $k'$ is the function which maps all of $X$ onto $y_0$. Prove that $\pi_1(Y,y_0)$ contains only one element.

3. Let $Y,\tau'$ be any space, $y_0\in Y$, and $k$ the function which maps all of $[0,1]$ onto $y_0$. Prove that $|k|$ is an identity for $\pi_1(Y,y_0)$ with respect to $\#$. That is, prove that if $|a_1|\in\pi_1(Y,y_0)$, then

   $$
   |k|\#|a_1|=|a_1|\#|k|=|a_1|.
   $$

   [*Hint:* Figures 11.13 and 11.14 give pictures of the desired homotopies. Express analytically what these pictures say graphically.]

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.13.svg" alt="A square parameter diagram with a slanted separator and regions labeled by a loop and a constant path; one of the identity homotopy exercises." />
<figcaption>Figure 11.13. <a href="/docs/elementary-topology/reader?page=245">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.14.svg" alt="A second square parameter diagram with a slanted separator and loop/constant-path regions; the companion identity homotopy exercise." />
<figcaption>Figure 11.14. <a href="/docs/elementary-topology/reader?page=245">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<!-- Continue the source exercise numbering. -->

4. Give an argument to show that if $Y=\{(x,y)\mid x^2+y^2=1\}\subset R^2$ and $P$ is any point of $Y$, then $\pi_1(Y,P)$ contains infinitely many elements. [_Hint:_ Let $k$ be the loop which maps all of $[0,1]$ onto $P$, and let $a_1$ be the loop which “wraps” $[0,1]$ once around $Y$ (the identification of $0$ and $1$, if you prefer). Show that $a_1$ is not homotopic to $k$. Can $a_1\#a_1$ be homotopic to $a_1$? For any positive integer $n$, define $na_1=a_1\#\cdots\#\text{(}n\text{ times)}\#a_1$. Try to show that for any positive integers $n$ and $m$, $na_1$ is homotopic to $ma_1$ if and only if $m=n$.]

5. Find an explicit homotopy between the loop $a$ on the disk $\{(x,y)\mid x^2+y^2\le1\}$ with base point $(0,1)$ defined by

   $$
   a(x)=\begin{cases}
   (4x,\sqrt{1-(4x)^2}),&0\le x\le\tfrac14,\\
   (2-4x,-\sqrt{1-(2-4x)^2}),&\tfrac14\le x\le\tfrac12,\\
   (-4x+2,-\sqrt{1-(2-4x)^2}),&\tfrac12\le x\le\tfrac34,\\
   (4x-4,\sqrt{1-(4x-4)^2}),&\tfrac34\le x\le1,
   \end{cases}
   $$

   with the loop which maps $[0,1]$ entirely into $(0,1)$. There are in fact infinitely many such homotopies.

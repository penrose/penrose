---
title: The Fundamental Group and Continuous Functions — Elementary Topology
---

# 11.4 The Fundamental Group and Continuous Functions

::: warning Missing printed page 253
The supplied scan omits printed page 253, including this section's opening and Proposition 6 statement. The section title is verified by the running header on printed page 255. Printed page 254 resumes a proof fragment using $\bar f$ and the induced homomorphism $f_*$. No missing definitions or statements are reconstructed.
:::

<span id="printed-page-254"></span>

<!-- Source: PDF235, printed254. -->

two continuous functions, it is continuous. Furthermore, $\bar f(a)$ is a function from $[0,1]$ into $Y$ such that

$$
\bar f(a)(0)=f(a(0))=f(x_0)=y_0=\bar f(a)(1);
$$

hence $\bar f(a)$ is indeed an element of $L(Y,y_0)$. For any $|a|\in\pi_1(X,x_0)$, set

$$
f_*(|a|)=|\bar f(a)|.
$$

We first show that $f_*$ is well-defined, that is, that $f_*(|a|)$ does not depend on the representative of $|a|$ used to compute it, but only on the equivalence class. Suppose $a\sim a'$, that is, $|a|=|a'|$. Then there is a homotopy

$$
H:[0,1]\times[0,1],\{0,1\}\times[0,1]\to X,x_0
$$

such that

$$
H\mid[0,1]\times\{0\}=a\quad\text{and}\quad H\mid[0,1]\times\{1\}=a'.
$$

Define $H'=f\circ H$. Since $H'$ is the composition of continuous functions, it is continuous. Direct computation shows that $H'$ is a homotopy between $f\circ a'$ and $f\circ a$, and hence

$$
|f\circ a|=f_*(|a|)=|f\circ a'|=f_*(|a'|).
$$

Therefore $f_*$ is well-defined.

We now prove that $f_*$ is a homomorphism. Suppose $|a|$ and $|a'|$ are any elements of $\pi_1(X,x_0)$. Now $a\#a'$ is defined by

$$
(a\#a')(r)=\begin{cases}
a(2r),&\text{if }0\le r\le\tfrac12,\\
a'(2r-1),&\text{if }\tfrac12\le r\le1.
\end{cases}
$$

Therefore $f\circ(a\#a')$ is the loop defined by

$$
f\circ(a\#a')(r)=\begin{cases}
f(a(2r)),&\text{if }0\le r\le\tfrac12,\\
f(a'(2r-1)),&\text{if }\tfrac12\le r\le1.
\end{cases}
$$

But this is precisely the definition of $f\circ a\#f\circ a'$. Then

$$
\bar f(a\#a')=\bar f(a)\#\bar f(a').
$$

Therefore $f_*(|a\#a'|)=f_*(|a|)\#f_*(|a'|)$. Since $|a\#a'|=|a|\#|a'|$, we have

$$
f_*(|a|\#|a'|)=f_*(|a|)\#f_*(|a'|).
$$

Hence $f_*$ is a homomorphism.

The following example shows that $f_*$ may not be onto even when $f$ is.

**Example 9.** We have already seen that there is a continuous function $f$ from the interval $[0,1]$ onto a circle $Y$. But $[0,1]$ is a contractible space <span id="printed-page-255"></span><!-- Source: PDF236, printed255. -->and thus has a trivial homotopy group, whereas the homotopy group of $Y$ is isomorphic to the additive group of integers. Therefore $f_*$ in this instance is merely a function which takes the sole element (the identity) of $\pi_1([0,1])$ onto the identity $|k|$ of $\pi_1(Y)$; hence $f_*$ is clearly not onto.

The next two propositions show that the homomorphisms induced by continuous functions behave fairly respectably in relation to the functions that induce them.

**Proposition 7.** Suppose $f$ is a continuous function from $X,\tau$ into $Y,\tau'$ and $g$ is a continuous function from $Y,\tau'$ into $Z,\tau''$. Also suppose that $f(x_0)=y_0$ and $g(y_0)=z_0$. Then

$$
(g\circ f)_*:\pi_1(X,x_0)\to\pi_1(Z,z_0)\quad\text{is the same as}\quad g_*\circ f_*.
$$

That is, the composition of continuous functions gives a corresponding composition of the homomorphisms that these functions induce.

_Proof._ Suppose $a\in L(X,x_0)$. Then

$$
\overline{(g\circ f)}(a)=(g\circ f)\circ a=g\circ(f\circ a)=\bar g(f\circ a)=\bar g(\bar f(a))=(\bar g\circ\bar f)(a).
$$

Therefore

$$
(g\circ f)_*(|a|)=(g_*\circ f_*)(|a|).
$$

**Proposition 8.** Suppose $f$ and $g$ are continuous functions from $X,\tau$ into $Y,\tau'$ and $f(x_0)=g(x_0)=y_0$. Then if $f$ is homotopic to $g$,

$$
f_*=g_*.
$$

_Proof._ If $f$ is homotopic to $g$, let $H$ be a homotopy between $f$ and $g$. Define

$$
(H*a)(r,s)=H(a(r),s)
$$

for each $(r,s)\in[0,1]\times[0,1]$. Then $H*a$ is easily verified to be a homotopy between $f\circ a$ and $g\circ a$ for any $a\in L(X,x_0)$. Then $\bar f(a)\sim\bar g(a)$ for any $a\in L(X,x_0)$; hence

$$
f_*(|a|)=g_*(|a|)\quad\text{for any }|a|\in\pi_1(X,x_0).
$$

If fundamental groups are to have much meaning topologically, then homeomorphic spaces should have isomorphic fundamental groups. As we have already seen, however, it is quite possible for two spaces which are not homeomorphic to have isomorphic fundamental groups. We may therefore suspect that there is a condition even weaker than homeomorphism which assures that two spaces have isomorphic fundamental groups. Experience has shown that this suspicion is indeed quite correct. The following definition proves to be appropriate.

<span id="printed-page-256"></span>

<!-- Source: PDF237, printed256. -->

**Definition 5.** Let $X,\tau$ and $Y,\tau'$ be (arc-connected) spaces. Then $X$ and $Y$ are said to have the _same homotopy type_, or to be _homotopically equivalent_, if there are continuous functions

$$
f:X\to Y\quad\text{and}\quad g:Y\to X
$$

such that $f\circ g$ is homotopic to the identity function $i_Y$ on $Y$ and $g\circ f$ is homotopic to the identity function $i_X$ on $X$.

Being of the same homotopy type is a weaker condition than being homeomorphic, since $X$ and $Y$ would be homeomorphic if and only if there were continuous functions $f:X\to Y$ and $g:Y\to X$ such that $f\circ g=i_Y$ and $g\circ f=i_X$ (hence $f=g^{-1}$). Of course, if two spaces are homeomorphic, they are also of the same homotopy type.

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.23.svg" alt="A shaded contractible space X and a singleton Y, connected by arrows f and g illustrating homotopy equivalence." />
<figcaption>Figure 11.23. <a href="/docs/elementary-topology/reader?page=256">View the interactive figure and its Substance program.</a></figcaption>
</figure>

**Example 10.** Let $X,\tau$ be any contractible space and let $Y,\tau'$ be a space consisting of a single point $P$. Then $X$ has the same homotopy type as $Y$. Let $x_0$ be a point of $X$ to which $X$ can be contracted and let $k$ be the function on $X$ which takes all of $X$ into $x_0$ (Fig. 11.23). Then $i_X\sim k$. Let $f:X\to Y$ be defined by $f(x)=P$ for all $x\in X$, and $g:Y\to X$ be defined by $g(P)=x_0$. Then

$$
f\circ g=k\sim i_Y
$$

and

$$
g\circ f=i_X.
$$

Therefore $X$ and $Y$ have the same homotopy type.

::: info Source composition formulas
The two displayed compositions in Example 10 are retained as printed, although $k$ has been defined on $X$ and $g\circ f$ is constant at $x_0$.
:::

Note how apparently different two spaces of the same homotopy type can be.

**Example 11.** Suppose $W$ is a subspace of a space $X,\tau$. Then a continuous function $f:X\to W$ is said to be a _retraction_ of $X$ onto $W$ if $f\mid W=i_W$. We call $W$ a _deformation retract_ of $X$ if $i_X$ is homotopic to a retraction of $X$ onto $W$. For example, the letter O is a deformation retract of the letter Q; here the retraction $f$ could be described by saying that $f$ takes any point in the tail of the Q into the point where the tail crosses the O part of the Q, and leaves all other points fixed.

<span id="printed-page-257"></span>

<!-- Source: PDF238, printed257. -->

If $W$ is a deformation retract of $X$, then $W$ and $X$ have the same homotopy type. For convenience, set $i_X\mid W=i_W$. Let $f$ be the continuous function from $X$ into $W$ which is homotopic to $i_X$. Then

$$
f\circ i_W=f\mid W\sim i_W\quad\text{and}\quad i_W\circ f=f\sim i_X.
$$

Therefore $W$ and $X$ have the same homotopy type.

The following proposition is pure set theory and will be stated without proof. It is of sufficiently wide application that the reader should already be familiar with it; if such is not the case, he should supply a proof.

**Proposition 9.** Let $f$ be a function from a set $S$ into a set $T$. Then $f$ is one-one and onto if and only if there is a function $g$ from $T$ into $S$ such that $f\circ g$ is the identity mapping on $T$ and $g\circ f$ is the identity mapping on $S$, that is, $f$ is one-one and onto if and only if it has a two-sided inverse.

We use this immediately to prove the following.

**Proposition 10.** Suppose two spaces $X,\tau$ and $Y,\tau'$ have the same homotopy type. Then $\pi_1(X)$ is isomorphic to $\pi_1(Y)$. (Recall that we are assuming $X$ and $Y$ arc-connected; hence their fundamental groups are independent of the base point.)

_Proof._ Since $X$ and $Y$ are of the same homotopy type, there are continuous functions $f:X\to Y$ and $g:Y\to X$ such that $f\circ g\sim i_Y$ and $g\circ f\sim i_X$. Applying Propositions 7 and 8, we have

$$
f_*\circ g_*=i_{Y*}\quad\text{and}\quad g_*\circ f_*=i_{X*}.
$$

But $i_{X*}$ and $i_{Y*}$ are the identity functions on $\pi_1(X)$ and $\pi_1(Y)$, respectively. It follows then from Proposition 9 that $f_*$ is one-one and onto, and hence is an isomorphism.

It is not true that if two spaces have isomorphic fundamental groups they are then of the same homotopy type (e.g., see Example 7). Nevertheless, homotopy equivalence does give a partition of the family of topological spaces, just as homotopy gave a partition of the family of continuous functions from one space into another.

**Proposition 11.** Let the phrase “is of the same homotopy type as” be denoted by $\simeq$, and let $\mathcal T$ denote the family of all topological spaces. Then $\simeq$ is an equivalence relation on $\mathcal T$.

_Proof._ If $X\in\mathcal T$, then $X\simeq X$. Let $f=g=i_X$. Then $f\circ g=g\circ f=i_X$; therefore $X\simeq X$.

::: warning Missing printed page 258
The supplied scan omits printed page 258, including the continuation of Proposition 11's proof and the beginning of the closing discussion. Printed page 259 resumes mid-sentence below. No missing argument is reconstructed.
:::

<span id="printed-page-259"></span>

<!-- Source: PDF239, printed259. -->

space without any holes. On the other hand, it should be noted that although the sphere also has a trivial fundamental group, it could hardly be said not to have any holes. The hole in a sphere, however, is a higher-dimensional hole, and is not one which can be registered by the fundamental group. Note that a torus has two types of holes and that its fundamental group has two generators. We should not, however, try to push this point too far, since it is only approximately true.

The fact that there are “higher-dimensional holes,” as well as the fact that the fundamental group can give but rather limited information, leads us to hope that there are other algebraic structures which can be associated with a space to supplement the information given by the fundamental group. Such is indeed the case. There are _higher homotopy groups_ [previously implied by using the notation $\pi_1(X)$ instead of merely $\pi(X)$], _homology groups_, _cohomology groups_, and a long list of others, but these will not be explored in this text.

## Exercises

1. Supply an argument to prove more fully that O is a deformation retract of Q (Example 11).

2. Classify each of the diagrams in Fig. 11.24 (considered as subspaces of $R^2$) according to homotopy type.

<figure class="book-figure">
<img src="/elementary-topology/figures/figure-11.24.svg" alt="Eleven separate diagram items for classifying homotopy types: seven capital letters, an animal, a house, the numeral 8, and the group 108." />
<figcaption>Figure 11.24. <a href="/docs/elementary-topology/reader?page=259">View the interactive figure and its Substance program.</a></figcaption>
</figure>

<!-- Continue the source exercise numbering. -->

3. In each of the following, decide whether or not the two spaces given are of the same homotopy type.

   a) a circle in $R^2$ and $R^2-\{(0,0)\}$

   b) the sphere in $R^3$ and a circle in $R^2$

   c) a triangle and a circle in $R^2$

   d) $R^3$ and $R^2$

4. In which of the following is the second space a deformation retract of the first space? An intuitive argument may be all the reader will be able to give.

   a) $\{(x,y)\mid x^2+y^2\le1\}$ and $\{(x,y)\mid x^2+y^2=1\}$

   b) $R^2$ and $\{(0,0)\}$

   c) $\{(x,y)\mid x^2+y^2\le1\}$ and $\{(x,y)\mid x^2+y^2<1\}$

5. Suppose $W$ is a subspace of $X,\tau$ such that $i_W=i_X\mid W$ can be extended to a continuous function from $X$ into $W$. Discuss the relation between $\pi_1(W)$ and <span id="printed-page-260"></span><!-- Source: PDF240, printed260. -->$\pi_1(X)$. Formulate a proposition which tells us when $i_W$ cannot be extended to a continuous function from $X$ into $W$. Let

   $$
   W=\{(x,y)\mid x^2+y^2=1\}\subset X=\{(x,y)\mid x^2+y^2\le1\}.
   $$

   In this case can $i_W$ be extended to a continuous function from $X$ into $W$? How could we interpret this result geometrically?

6. Suppose $X,\tau$ is a space with a contractible subspace $A$. Let $X/A$ be the identification space obtained by identifying all the points of $A$ (and leaving the other points of $X$ as they are). What can be said about the relation of $\pi_1(X)$ to $\pi_1(X/A)$? Give an argument for your assertion. Can you interpret your statement geometrically?

7. We will call a continuous function $f$ from a space $X,\tau$ into a space $Y,\tau'$ _trivial_ if $f_*$ takes all of $\pi_1(X)$ onto the identity of $\pi_1(Y)$. Examine each of the following and decide whether or not there is a nontrivial function from the first space into the second. If there is a nontrivial function, describe one.

   a) a circle in $R^2$ and the open interval $(0,1)$

   b) a circle in $R^2$ and $R^2-\{(0,0)\}$

   c) a circle $C$ in $R^2$ and the torus $C\times C$

   d) a torus and a circle in $R^2$

   e) a space with fundamental group $Z_5$ and a space with fundamental group $Z_3$, where $Z_5$ and $Z_3$ represent the additive groups of integers modulo $5$ and $3$, respectively

8. If $X$ and $Y$ have the same homotopy type, which of the following situations cannot occur?

   a) $X$ compact, but $Y$ not compact

   b) $X$ connected, but $Y$ disconnected

   c) $X$ arcwise connected, but $Y$ not arcwise connected

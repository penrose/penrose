---
title: The Notion of a Metric Space — Elementary Topology
description: Section 2.1, printed pages 16–18, of Elementary Topology, second edition.
---

# 2.1 The Notion of a Metric Space

::: info Transcription note
Source: printed pages 16–18 (PDF pages 24–26). The mathematical formulas have been checked against the supplied scans.
:::

<span id="printed-page-16"></span>

<!-- source: PDF 24, printed 16 -->

Most of the important notions in point set topology are generalizations of concepts which were first studied in the context of metric spaces. By way of motivation, and also because metric spaces are still extremely important in their own right, we would do well to consider the fundamental properties of metric spaces.

A metric space is a set in which we have a measure of the closeness or proximity of two elements of the set, that is, we have a distance defined on the set. A metric is nothing more than the ordinary notion of distance. More precisely, we make the following definition.

**Definition 1.** Let $X$ be any set. A function $D$ from $X\times X$ into $R$, the set of real numbers, is said to be a _metric_ on $X$ if

i) $D(x,y)\geq 0$, for all $x,y\in X$;

ii) $D(x,y)=D(y,x)$, for all $x,y\in X$;

iii) $D(x,y)=0$ if and only if $x=y$; and

iv) $D(x,y)+D(y,z)\geq D(x,z)$, for all $x,y,z\in X$.

A set $X$ with metric $D$ is said to be a _metric space_, and may be denoted by $X,D$.

Note that the metric $D$ has properties that we intuitively associate with distance; in fact, as has already been remarked, a metric is merely a formalized expression of distance.

**Example 1.** Let $R$ be the set of real numbers. One possible metric for $R$ is the _absolute value metric_; that is, define $D(x,y)=|x-y|$.

**Example 2.** The metric in Example 1 should already have been familiar to the reader (although he may not have called it a metric). Another metric usually encountered in more elementary courses is the “Pythagorean” metric on the coordinate plane $R^2$. If $(x_1,y_1)$ and $(x_2,y_2)$ are any two points of $R^2$, then we can define a metric $D$ on $R^2$ by setting

$$
D((x_1,y_1),(x_2,y_2))=\sqrt{(x_1-x_2)^2+(y_1-y_2)^2}.
$$

<span id="printed-page-17"></span>

<!-- source: PDF 25, printed 17 -->

**Example 3.** The metric defined on $R^2$ in Example 2 is not the only metric which can be defined for $R^2$. Again letting $(x_1,y_1)$ and $(x_2,y_2)$ be any two points of $R^2$, we can define metrics $D_1,D_2$, and $D_3$ on $R^2$ as follows:

$$
\begin{aligned}
D_1((x_1,y_1),(x_2,y_2))&=|x_1-x_2|+|y_1-y_2|;\\
D_2((x_1,y_1),(x_2,y_2))&=\begin{cases}
0&\text{if }(x_1,y_1)=(x_2,y_2);\\
1&\text{if }(x_1,y_1)\neq(x_2,y_2);
\end{cases}\\
D_3((x_1,y_1),(x_2,y_2))&=\max(|x_1-x_2|,|y_1-y_2|).
\end{aligned}
$$

**Example 4.** If $X$ is any metric space with metric $D$, and $Y$ is any subset of $X$, then $Y$ can also be considered to be a metric space using the same metric as $X$. More precisely, $Y$ with metric $D\mid Y$ (i.e. $D$ defined for pairs of elements of $Y$) is a metric space. $Y,D\mid Y$ is said to be a _subspace_ of the metric space $X,D$.

**Example 5.** This example is given to illustrate that a metric can be defined on a set which is neither $R^n$ for some $n$ nor a subset of $R^n$. Let $X$ be the set of all functions from the closed interval $[0,1]$ into itself. If $f$ and $g$ are any such functions, define

$$
D(f,g)=\text{least upper bound }\{|f(x)-g(x)|\mid x\in[0,1]\}.
$$

Since any subset of the real numbers which has an upper bound has a least upper bound and

$$
0\leq |f(x)-g(x)|\leq 1\quad\text{for all }f,g\in X\text{ and }x\in[0,1],
$$

then $D(f,g)$ is defined for all $f,g\in X$. We will now show that $D$ is a metric for $X$ by showing that $D$ satisfies each of the properties required for a metric in Definition 1:

i) $D(f,g)\geq 0$ for all $f,g\in X$. Since each element of

$$
\{|f(x)-g(x)|\mid x\in[0,1]\}
$$

is greater than or equal to 0, the least upper bound of this set, $D(f,g)$, is greater than or equal to 0.

ii) $D(f,g)=D(g,f)$ for all $f,g\in X$. This follows at once from the fact that $|f(x)-g(x)|=|g(x)-f(x)|$.

iii) $D(f,g)=0$ if and only if $f=g$. If $f=g$, then $f(x)=g(x)$ for all $x\in[0,1]$, and hence $\{|f(x)-g(x)|\mid x\in[0,1]\}=\{0\}$. Therefore it follows that $D(f,g)=0$. On the other hand, if $D(f,g)=0$, then

$$
\operatorname{lub}\{|f(x)-g(x)|\mid x\in[0,1]\}=0.
$$

<span id="printed-page-18"></span>

<!-- source: PDF 26, printed 18 -->

Since $|f(x)-g(x)|$ is always greater than or equal to 0, it follows that

$$
\{|f(x)-g(x)|\mid x\in[0,1]\}=\{0\};
$$

hence $|f(x)-g(x)|=0$ for all $x\in[0,1]$. Therefore $f(x)=g(x)$ for all $x\in[0,1]$; that is, $f=g$.

iv) $D(f,g)+D(g,h)\geq D(f,h)$ for all $f,g,h\in X$. This inequality follows from

$$
|f(x)-h(x)|\leq|f(x)-g(x)|+|g(x)-h(x)|\quad\text{for all }x\in[0,1].
$$

The details are left as an exercise.

## Exercises

1. Prove that $D_1,D_2$, and $D_3$ as defined in Example 3 are really metrics for $R^2$.
2. Let $(x_1,y_1)$ and $(x_2,y_2)$ be any points of $R^2$. Which of the following do not define metrics for $R^2$? Explain your answer in each case.

   a) $D((x_1,y_1),(x_2,y_2))=\min(|x_1-x_2|,|y_1-y_2|)$.

   b) $D((x_1,y_1),(x_2,y_2))=(x_1-x_2)^2+(y_1-y_2)^2$.

   c) $D((x_1,y_1),(x_2,y_2))=D_1((x_1,y_1),(x_2,y_2))-D_3((x_1,y_1),(x_2,y_2))$, where $D_1$ and $D_3$ are as defined in Example 3.

   d) $D((x_1,y_1),(x_2,y_2))=|x_1|+|x_2|+|y_1|+|y_2|$.

3. Suppose $X,D$ is a metric space. We may define a metric $D'$ for $X\times X$ as follows: If $(x,y)$ and $(x',y')$ are any elements of $X\times X$, set

   $$
   D'((x,y),(x',y'))=D(x,x')+D(y,y').
   $$

   Prove that $D'$ is really a metric for $X\times X$. Define a metric for $X^n$, that is, $X\times\cdots\times$ ($n$ times) $\times X$.

4. Suppose $X,D$ is a metric space. If $x$ and $y$ are any elements of $X$, which of the following define metrics on $X$?

   a) $D_1(x,y)=kD(x,y)$, where $k$ is any positive real number.

   b) $D_2(x,y)=kD(x,y)$, where $k$ is any real number.

   c) $D_3(x,y)=D^n(x,y)$, where $n$ is any positive integer.

   d) $D_4(x,y)=D^r(x,y)$, where $0<r<1$.

5. Supply the details for the proof of Example 5(iv).
6. For any two distinct points $P_1$ and $P_2$ of $R^2$, set

   $$
   M(P_1,P_2)=\{P\in R^2\mid d(P_1,P)=d(P_2,P)\},
   $$

   where $d$ is a metric on $R^2$. Describe geometrically $M(P_1,P_2)$ for each of the metrics on $R^2$ introduced in Example 3 as well as for the Pythagorean metric.

[Continue to 2.2 Neighborhoods](../neighborhoods)
